#' Generate ready-to-use R code for the recommended method
#'
#' This function creates executable R code based on the recommended statistical test,
#' including proper variable names and additional parameters (e.g., paired, mu, var.equal).
#'
#' @param recommendation A recommendation object from decide_method().
#' @param data_info Output from detect_data_structure().
#' @param data_name Character; name of the data object in the user's environment.
#' @param var_dependent Character; name of dependent variable.
#' @param var_independent Character; name of independent variable (optional).
#' @param ... Additional arguments to pass to the R function (e.g., mu = 0, paired = TRUE).
#'
#' @return Character string containing the R code.
#'
#' @examples
#' \dontrun{
#' data(iris)
#' info <- detect_data_structure(iris, "Sepal.Length", "Species")
#' rec <- decide_method(info)
#' generate_code(rec, info, "iris", "Sepal.Length", "Species")
#' }
#' @export
generate_code <- function(recommendation,
                          data_info,
                          data_name = "data",
                          var_dependent = NULL,
                          var_independent = NULL,
                          ...) {
  
  # Extract method name and function
  method <- recommendation$method
  r_func <- recommendation$r_function
  
  # Get variable names from data_info if not provided
  if (is.null(var_dependent) && !is.null(data_info$dependent)) {
    var_dependent <- data_info$dependent$name
  }
  if (is.null(var_independent) && !is.null(data_info$independent)) {
    var_independent <- data_info$independent$name
  }
  
  # Base code template
  code <- ""
  
  # Handle different method families
  if (grepl("t-test", method, ignore.case = TRUE)) {
    code <- generate_ttest_code(method, data_name, var_dependent, var_independent, ...)
  } else if (grepl("wilcoxon|mann-whitney", method, ignore.case = TRUE)) {
    code <- generate_wilcox_code(method, data_name, var_dependent, var_independent, ...)
  } else if (grepl("anova", method, ignore.case = TRUE)) {
    code <- generate_anova_code(method, data_name, var_dependent, var_independent, ...)
  } else if (grepl("kruskal", method, ignore.case = TRUE)) {
    code <- generate_kruskal_code(method, data_name, var_dependent, var_independent, ...)
  } else if (grepl("pearson|spearman|kendall", method, ignore.case = TRUE)) {
    code <- generate_correlation_code(method, data_name, var_dependent, var_independent, ...)
  } else if (grepl("chi-square", method, ignore.case = TRUE)) {
    code <- generate_chisq_code(method, data_name, var_dependent, var_independent, ...)
  } else if (grepl("binomial", method, ignore.case = TRUE)) {
    code <- generate_binomial_code(method, data_name, var_dependent, ...)
  } else if (grepl("fisher", method, ignore.case = TRUE)) {
    code <- generate_fisher_code(method, data_name, var_dependent, var_independent, ...)
  } else {
    # Fallback: generic formula
    if (!is.null(var_independent)) {
      code <- sprintf("%s(%s ~ %s, data = %s)", r_func, var_dependent, var_independent, data_name)
    } else {
      code <- sprintf("%s(%s, data = %s)", r_func, var_dependent, data_name)
    }
  }
  
  # Add comments and formatting
  code <- paste0("# ", method, "\n", code)
  
  # If there are additional parameters, append them
  dots <- list(...)
  if (length(dots) > 0) {
    params <- paste(names(dots), dots, sep = " = ", collapse = ", ")
    code <- sub("\\)$", paste0(", ", params, ")"), code)
  }
  
  return(code)
}

#' Generate code for t-test variants
#' @keywords internal
generate_ttest_code <- function(method, data_name, var_dep, var_indep, ...) {
  if (grepl("one sample", method, ignore.case = TRUE)) {
    sprintf("t.test(%s$%s, mu = 0)", data_name, var_dep)
  } else if (grepl("paired", method, ignore.case = TRUE)) {
    sprintf("t.test(%s$%s ~ %s$%s, paired = TRUE)", data_name, var_dep, data_name, var_indep)
  } else if (grepl("welch", method, ignore.case = TRUE)) {
    sprintf("t.test(%s ~ %s, data = %s, var.equal = FALSE)", var_dep, var_indep, data_name)
  } else {
    # Independent t-test (equal variance assumed)
    sprintf("t.test(%s ~ %s, data = %s, var.equal = TRUE)", var_dep, var_indep, data_name)
  }
}

#' Generate code for Wilcoxon/Mann-Whitney tests
#' @keywords internal
generate_wilcox_code <- function(method, data_name, var_dep, var_indep, ...) {
  if (grepl("signed.*rank|one sample", method, ignore.case = TRUE)) {
    sprintf("wilcox.test(%s$%s, mu = 0)", data_name, var_dep)
  } else if (grepl("paired", method, ignore.case = TRUE)) {
    sprintf("wilcox.test(%s ~ %s, data = %s, paired = TRUE)", var_dep, var_indep, data_name)
  } else {
    # Mann-Whitney (two independent samples)
    sprintf("wilcox.test(%s ~ %s, data = %s)", var_dep, var_indep, data_name)
  }
}

#' Generate code for ANOVA
#' @keywords internal
generate_anova_code <- function(method, data_name, var_dep, var_indep, ...) {
  if (grepl("one[- ]way", method, ignore.case = TRUE)) {
    code <- sprintf("aov(%s ~ %s, data = %s)", var_dep, var_indep, data_name)
    # Add post-hoc comment
    code <- paste0(code, "\n# For post-hoc tests: TukeyHSD(model)")
  } else if (grepl("welch", method, ignore.case = TRUE)) {
    code <- sprintf("oneway.test(%s ~ %s, data = %s, var.equal = FALSE)", var_dep, var_indep, data_name)
  } else {
    # Default to aov
    code <- sprintf("aov(%s ~ %s, data = %s)", var_dep, var_indep, data_name)
  }
  code
}

#' Generate code for Kruskal-Wallis
#' @keywords internal
generate_kruskal_code <- function(method, data_name, var_dep, var_indep, ...) {
  sprintf("kruskal.test(%s ~ %s, data = %s)", var_dep, var_indep, data_name)
}

#' Generate code for correlation tests
#' @keywords internal
generate_correlation_code <- function(method, data_name, var_dep, var_indep, ...) {
  method_param <- switch(tolower(method),
    "pearson correlation" = "pearson",
    "spearman's rank correlation" = "spearman",
    "kendall's tau" = "kendall",
    "pearson" = "pearson",
    "spearman" = "spearman",
    "kendall" = "kendall",
    "pearson"
  )
  sprintf("cor.test(%s$%s, %s$%s, method = '%s')", data_name, var_dep, data_name, var_indep, method_param)
}

#' Generate code for Chi-square test
#' @keywords internal
generate_chisq_code <- function(method, data_name, var_dep, var_indep, ...) {
  sprintf("chisq.test(table(%s$%s, %s$%s))", data_name, var_dep, data_name, var_indep)
}

#' Generate code for Binomial test
#' @keywords internal
generate_binomial_code <- function(method, data_name, var_dep, ...) {
  sprintf("binom.test(table(%s$%s))", data_name, var_dep)
}

#' Generate code for Fisher's exact test
#' @keywords internal
generate_fisher_code <- function(method, data_name, var_dep, var_indep, ...) {
  sprintf("fisher.test(table(%s$%s, %s$%s))", data_name, var_dep, data_name, var_indep)
}
