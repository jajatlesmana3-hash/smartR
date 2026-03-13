#' Core decision logic for selecting statistical methods
#'
#' This function implements the decision tree based on the comprehensive guide
#' table. It uses the detected data structure and assumption checks to recommend
#' the most appropriate statistical test.
#'
#' @param data_info Output from detect_data_structure().
#' @param assumptions Output from check_assumptions() (optional, will be computed if missing).
#' @param data Raw data (required if assumptions not provided).
#' @param var_dependent Character; name of dependent variable (required if assumptions not provided).
#' @param var_independent Character; name of independent variable (optional).
#' @param test_type Character; one of "comparison" (default), "association", or "auto".
#'        "auto" attempts to infer from variable types.
#'
#' @return A list with recommendation details, including method name, R function,
#'         confidence score, and alternatives.
#'
#' @examples
#' \dontrun{
#' data(iris)
#' info <- detect_data_structure(iris, "Sepal.Length", "Species")
#' decide_method(info)
#' }
#' @export
decide_method <- function(data_info,
                          assumptions = NULL,
                          data = NULL,
                          var_dependent = NULL,
                          var_independent = NULL,
                          test_type = c("auto", "comparison", "association")) {
  
  test_type <- match.arg(test_type)
  
  # If data_info not provided but data and var names are given, run detection
  if (missing(data_info) && !is.null(data) && !is.null(var_dependent)) {
    data_info <- detect_data_structure(data, var_dependent, var_independent)
  }
  
  if (is.null(data_info) || is.null(data_info$dependent)) {
    stop("Data information is incomplete. Please provide valid data_info or data/variable names.")
  }
  
  # If assumptions not provided, try to compute them if data is available
  if (is.null(assumptions) && !is.null(data) && !is.null(var_dependent)) {
    assumptions <- check_assumptions(data, var_dependent, var_independent, data_info = data_info)
  }
  
  # Determine test type if auto
  if (test_type == "auto") {
    test_type <- infer_test_type(data_info)
  }
  
  # Initialize result
  recommendation <- list(
    method = NULL,
    r_function = NULL,
    category = NULL,
    confidence = NULL,
    assumptions_used = assumptions,
    alternatives = list(),
    notes = character(0),
    references = character(0)
  )
  
  # Branch based on test type
  if (test_type == "comparison") {
    recommendation <- recommend_comparison(data_info, assumptions)
  } else if (test_type == "association") {
    recommendation <- recommend_association(data_info, assumptions)
  } else {
    stop("Unknown test type. Must be 'comparison' or 'association'.")
  }
  
  # Add confidence score based on assumptions met
  if (!is.null(assumptions)) {
    confidence <- calculate_confidence(recommendation$method, assumptions, data_info)
    recommendation$confidence <- confidence
  } else {
    recommendation$confidence <- 3  # default moderate if no assumptions checked
  }
  
  # Add references (to be implemented later from a reference database)
  recommendation$references <- get_method_references(recommendation$method)
  
  class(recommendation) <- "smartR_recommendation"
  return(recommendation)
}

#' Infer test type from variable types
#' @param data_info Output from detect_data_structure
#' @return "comparison" or "association"
#' @keywords internal
infer_test_type <- function(data_info) {
  dep <- data_info$dependent
  indep <- data_info$independent
  
  # If no independent variable, it's one-sample comparison
  if (is.null(indep)) {
    return("comparison")
  }
  
  dep_type <- dep$type_info$main_type
  indep_type <- indep$type_info$main_type
  
  # Both numeric -> association (correlation)
  if (dep_type == "NUMERIC" && indep_type == "NUMERIC") {
    return("association")
  }
  
  # Dependent numeric, independent categorical -> comparison
  if (dep_type == "NUMERIC" && indep_type == "CATEGORICAL") {
    return("comparison")
  }
  
  # Dependent categorical, independent categorical -> association (Chi-square)
  if (dep_type == "CATEGORICAL" && indep_type == "CATEGORICAL") {
    return("association")
  }
  
  # Dependent categorical, independent numeric -> possibly logistic regression? but for simplicity treat as comparison with reversed roles? We'll default to comparison with warning.
  warning("Ambiguous variable types. Defaulting to comparison test. Check your variables.")
  return("comparison")
}

#' Recommend a comparison test (differences between groups)
#' @param data_info Data info
#' @param assumptions Assumption checks
#' @return List with recommendation details
#' @keywords internal
recommend_comparison <- function(data_info, assumptions) {
  
  dep <- data_info$dependent
  indep <- data_info$independent
  n_groups <- if (!is.null(indep)) indep$n_unique else 1
  
  # One-sample case
  if (is.null(indep)) {
    if (dep$type_info$main_type == "NUMERIC") {
      # One-sample t-test or Wilcoxon
      norm_pass <- if (!is.null(assumptions$normality)) isTRUE(assumptions$normality$passed) else NA
      if (isTRUE(norm_pass)) {
        return(list(
          method = "One-Sample t-test",
          r_function = "t.test",
          category = "Parametric",
          alternatives = list("Wilcoxon Signed-Rank" = "wilcox.test"),
          notes = "Compares mean of one group to a hypothesized value. Use mu parameter."
        ))
      } else {
        return(list(
          method = "Wilcoxon Signed-Rank Test",
          r_function = "wilcox.test",
          category = "Non-parametric",
          alternatives = list("One-Sample t-test" = "t.test"),
          notes = "Non-parametric alternative to one-sample t-test. Tests median."
        ))
      }
    } else if (dep$type_info$main_type == "CATEGORICAL") {
      # Binomial / Chi-square goodness of fit
      if (dep$n_unique == 2) {
        return(list(
          method = "Binomial Test",
          r_function = "binom.test",
          category = "Non-parametric",
          alternatives = list("Chi-square Goodness of Fit" = "chisq.test"),
          notes = "Tests proportion in a binary category."
        ))
      } else {
        return(list(
          method = "Chi-square Goodness of Fit",
          r_function = "chisq.test",
          category = "Non-parametric",
          alternatives = list(),
          notes = "Tests if observed frequencies match expected proportions."
        ))
      }
    }
  }
  
  # Two or more groups, independent variable is categorical
  if (dep$type_info$main_type == "NUMERIC") {
    if (n_groups == 2) {
      # Two groups
      norm_pass <- if (!is.null(assumptions$normality)) isTRUE(assumptions$normality$passed) else NA
      homo_pass <- if (!is.null(assumptions$homogeneity)) isTRUE(assumptions$homogeneity$passed) else NA
      
      if (isTRUE(norm_pass) && isTRUE(homo_pass)) {
        return(list(
          method = "Independent t-test",
          r_function = "t.test",
          category = "Parametric",
          alternatives = list(
            "Welch's t-test" = "t.test(var.equal=FALSE)",
            "Mann-Whitney U" = "wilcox.test"
          ),
          notes = "Compares means between two independent groups. Assumes normality and equal variances."
        ))
      } else if (isTRUE(norm_pass) && !isTRUE(homo_pass)) {
        return(list(
          method = "Welch's t-test",
          r_function = "t.test",
          category = "Parametric (unequal variances)",
          alternatives = list(
            "Independent t-test" = "t.test(var.equal=TRUE)",
            "Mann-Whitney U" = "wilcox.test"
          ),
          notes = "t-test without assuming equal variances. Use var.equal=FALSE."
        ))
      } else {
        return(list(
          method = "Mann-Whitney U Test",
          r_function = "wilcox.test",
          category = "Non-parametric",
          alternatives = list(
            "Independent t-test" = "t.test",
            "Welch's t-test" = "t.test(var.equal=FALSE)"
          ),
          notes = "Non-parametric alternative for two independent groups."
        ))
      }
    } else {
      # More than two groups
      norm_pass <- if (!is.null(assumptions$normality)) isTRUE(assumptions$normality$passed) else NA
      homo_pass <- if (!is.null(assumptions$homogeneity)) isTRUE(assumptions$homogeneity$passed) else NA
      
      if (isTRUE(norm_pass) && isTRUE(homo_pass)) {
        return(list(
          method = "One-Way ANOVA",
          r_function = "aov",
          category = "Parametric",
          alternatives = list(
            "Welch's ANOVA" = "oneway.test",
            "Kruskal-Wallis" = "kruskal.test"
          ),
          notes = "Compares means across multiple groups. Follow with TukeyHSD for post-hoc."
        ))
      } else if (isTRUE(norm_pass) && !isTRUE(homo_pass)) {
        return(list(
          method = "Welch's ANOVA",
          r_function = "oneway.test",
          category = "Parametric (unequal variances)",
          alternatives = list(
            "One-Way ANOVA" = "aov",
            "Kruskal-Wallis" = "kruskal.test"
          ),
          notes = "ANOVA without assuming equal variances. Use var.equal=FALSE."
        ))
      } else {
        return(list(
          method = "Kruskal-Wallis Test",
          r_function = "kruskal.test",
          category = "Non-parametric",
          alternatives = list(
            "One-Way ANOVA" = "aov",
            "Welch's ANOVA" = "oneway.test"
          ),
          notes = "Non-parametric alternative to one-way ANOVA."
        ))
      }
    }
  } else if (dep$type_info$main_type == "CATEGORICAL") {
    # Dependent categorical, independent categorical -> Chi-square
    # Check expected frequencies for Fisher's exact
    if (!is.null(data_info$overview)) {
      # Rough check: if any expected count < 5, suggest Fisher
      # We don't have the full table here, so we'll note it.
      return(list(
        method = "Chi-square Test of Independence",
        r_function = "chisq.test",
        category = "Non-parametric",
        alternatives = list(
          "Fisher's Exact Test" = "fisher.test"
        ),
        notes = "Tests association between two categorical variables. If expected counts <5, consider Fisher's exact test."
      ))
    }
  }
  
  # Fallback
  return(list(
    method = "Unknown",
    r_function = "",
    category = "",
    notes = "Unable to determine appropriate test based on input."
  ))
}

#' Recommend an association test (correlation/relationship)
#' @param data_info Data info
#' @param assumptions Assumption checks
#' @return List with recommendation details
#' @keywords internal
recommend_association <- function(data_info, assumptions) {
  
  dep <- data_info$dependent
  indep <- data_info$independent
  
  # Both numeric -> correlation
  if (dep$type_info$main_type == "NUMERIC" && indep$type_info$main_type == "NUMERIC") {
    norm_pass <- if (!is.null(assumptions$normality)) isTRUE(assumptions$normality$passed) else NA
    if (isTRUE(norm_pass)) {
      return(list(
        method = "Pearson Correlation",
        r_function = "cor.test",
        category = "Parametric",
        alternatives = list(
          "Spearman's rho" = "cor.test(method='spearman')",
          "Kendall's tau" = "cor.test(method='kendall')"
        ),
        notes = "Measures linear correlation between two continuous variables. Assumes bivariate normality."
      ))
    } else {
      return(list(
        method = "Spearman's Rank Correlation",
        r_function = "cor.test",
        category = "Non-parametric",
        alternatives = list(
          "Pearson Correlation" = "cor.test(method='pearson')",
          "Kendall's tau" = "cor.test(method='kendall')"
        ),
        notes = "Monotonic correlation based on ranks. Robust to outliers."
      ))
    }
  }
  
  # Both categorical -> Chi-square / Cramer's V
  if (dep$type_info$main_type == "CATEGORICAL" && indep$type_info$main_type == "CATEGORICAL") {
    return(list(
      method = "Chi-square Test of Independence",
      r_function = "chisq.test",
      category = "Non-parametric",
      alternatives = list(
        "Fisher's Exact Test" = "fisher.test",
        "Cramer's V" = "vcd::assocstats"
      ),
      notes = "Tests association between two categorical variables. For effect size, use Cramer's V."
    ))
  }
  
  # Mixed types? Could be point-biserial, etc. For simplicity, we'll default to appropriate.
  if (dep$type_info$main_type == "NUMERIC" && indep$type_info$main_type == "CATEGORICAL" && indep$n_unique == 2) {
    return(list(
      method = "Point-Biserial Correlation",
      r_function = "cor.test",  # same as Pearson with binary factor
      category = "Parametric",
      alternatives = list("Mann-Whitney" = "wilcox.test"),
      notes = "Correlation between a continuous variable and a binary variable. Equivalent to Pearson."
    ))
  }
  
  # Fallback
  return(list(
    method = "Unknown",
    r_function = "",
    category = "",
    notes = "Unable to determine appropriate association test."
  ))
}

#' Calculate confidence score (1-5) based on assumptions met
#' @param method Name of method
#' @param assumptions Assumption checks
#' @param data_info Data info
#' @return Integer 1-5
#' @keywords internal
calculate_confidence <- function(method, assumptions, data_info) {
  score <- 5  # start high
  
  # Reduce score if assumptions are not met
  if (!is.null(assumptions$normality) && isFALSE(assumptions$normality$passed)) {
    if (grepl("t-test|ANOVA|Pearson", method, ignore.case = TRUE)) {
      score <- score - 2
    }
  }
  if (!is.null(assumptions$homogeneity) && isFALSE(assumptions$homogeneity$passed)) {
    if (grepl("t-test|ANOVA", method, ignore.case = TRUE) && !grepl("Welch", method)) {
      score <- score - 1
    }
  }
  
  # Check sample size
  if (!is.null(data_info$overview)) {
    n <- data_info$overview$complete_cases
    if (n < 30) score <- score - 1
    if (n < 10) score <- score - 1
  }
  
  # Clamp between 1 and 5
  max(1, min(5, score))
}

#' Get references for a given statistical method
#' @param method Name of method
#' @return Character vector of references
#' @keywords internal
get_method_references <- function(method) {
  # This will be populated with real references later.
  # For now, placeholder references (to be replaced with actual credible sources).
  refs <- switch(tolower(method),
    "independent t-test" = c(
      "Student (1908). The probable error of a mean. Biometrika.",
      "Welch, B.L. (1947). The generalization of 'Student's' problem. Biometrika."
    ),
    "welch's t-test" = c(
      "Welch, B.L. (1947). The generalization of 'Student's' problem. Biometrika."
    ),
    "mann-whitney u test" = c(
      "Mann, H.B., & Whitney, D.R. (1947). On a test of whether one of two random variables is stochastically larger than the other. Annals of Mathematical Statistics."
    ),
    "one-way anova" = c(
      "Chambers, J.M., Freeny, A., & Heiberger, R.M. (2017). Analysis of variance; designed experiments. In Statistical Models in S."
    ),
    "kruskal-wallis test" = c(
      "Kruskal, W.H., & Wallis, W.A. (1952). Use of ranks in one-criterion variance analysis. Journal of the American Statistical Association."
    ),
    "pearson correlation" = c(
      "Pearson, K. (1895). Notes on regression and inheritance in the case of two parents. Proceedings of the Royal Society of London."
    ),
    "spearman's rank correlation" = c(
      "Spearman, C. (1904). The proof and measurement of association between two things. American Journal of Psychology."
    ),
    "chi-square test of independence" = c(
      "Pearson, K. (1900). On the criterion that a given system of deviations from the probable in the case of a correlated system of variables is such that it can be reasonably supposed to have arisen from random sampling. Philosophical Magazine."
    ),
    c("References will be updated in the next version. For now, consult standard statistical textbooks.")
  )
  
  # Limit to 2 references for brevity
  head(refs, 2)
}

#' Print method for smartR_recommendation
#' @param x Object of class smartR_recommendation
#' @param ... Additional arguments
#' @export
print.smartR_recommendation <- function(x, ...) {
  cat("\n==================== smartR RECOMMENDATION ====================\n\n")
  
  cat("PRIMARY RECOMMENDATION\n")
  cat("  Method: ", x$method, "\n")
  cat("  R function: ", x$r_function, "()\n")
  cat("  Category: ", x$category, "\n")
  cat("  Confidence: ", x$confidence, "/5\n\n")
  
  if (length(x$notes) > 0) {
    cat("NOTES\n")
    for (n in x$notes) {
      cat("  • ", n, "\n")
    }
    cat("\n")
  }
  
  if (length(x$alternatives) > 0) {
    cat("ALTERNATIVES\n")
    for (alt in names(x$alternatives)) {
      cat("  • ", alt, ": ", x$alternatives[[alt]], "\n")
    }
    cat("\n")
  }
  
  if (length(x$references) > 0) {
    cat("REFERENCES\n")
    for (r in x$references) {
      cat("  • ", r, "\n")
    }
    cat("\n")
  }
  
  cat("================================================================\n")
  invisible(x)
}
