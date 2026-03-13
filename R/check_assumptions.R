#' Check statistical assumptions for a variable or group comparison
#'
#' This function performs assumption checks (normality, homogeneity of variance)
#' depending on the data structure and the type of analysis anticipated.
#'
#' @param data A data.frame or vector.
#' @param var_dependent Character; name of dependent variable (if data is data.frame).
#' @param var_independent Character; name of independent/grouping variable (optional).
#' @param data_info Optional output from detect_data_structure to avoid recomputation.
#'
#' @return A list with results of assumption checks, each containing logical passed/failed,
#'         p-value, and method used.
#'
#' @examples
#' \dontrun{
#' data(iris)
#' check_assumptions(iris, "Sepal.Length", "Species")
#' }
#' @export
check_assumptions <- function(data, var_dependent = NULL, var_independent = NULL,
                              data_info = NULL) {
  
  # If data_info not provided, run detection first
  if (is.null(data_info)) {
    data_info <- detect_data_structure(data, var_dependent, var_independent)
  }
  
  result <- list()
  
  # Check if we have dependent variable info
  if (is.null(data_info$dependent)) {
    warning("No dependent variable info available. Assumptions cannot be checked.")
    return(result)
  }
  
  dep_type <- data_info$dependent$type_info$main_type
  dep_data <- data_info$dependent$data
  
  # 1. NORMALITY CHECK (if dependent is numeric)
  if (dep_type == "NUMERIC") {
    result$normality <- check_normality(dep_data, group = if (!is.null(data_info$independent)) data_info$independent$data else NULL)
  } else {
    result$normality <- list(
      passed = NA,
      p_value = NA,
      method = "Not applicable for non-numeric data",
      details = "Normality check skipped because dependent variable is not numeric."
    )
  }
  
  # 2. HOMOGENEITY OF VARIANCE (if both dependent numeric and independent categorical)
  if (dep_type == "NUMERIC" && !is.null(data_info$independent) &&
      data_info$independent$type_info$main_type == "CATEGORICAL") {
    result$homogeneity <- check_homogeneity(dep_data, data_info$independent$data)
  } else {
    result$homogeneity <- list(
      passed = NA,
      p_value = NA,
      method = "Not applicable",
      details = "Homogeneity check requires numeric dependent and categorical independent."
    )
  }
  
  # 3. Add a summary field for easy access
  result$all_passed <- all(sapply(result, function(x) isTRUE(x$passed)))
  
  class(result) <- "smartR_assumptions"
  return(result)
}

#' Check normality of a numeric vector, optionally by groups
#'
#' @param x Numeric vector.
#' @param group Optional grouping factor.
#' @return A list with passed (logical), p_value, method, and possibly per-group details.
#' @keywords internal
check_normality <- function(x, group = NULL) {
  
  # Remove NAs
  x_clean <- stats::na.omit(x)
  if (length(x_clean) < 3) {
    return(list(
      passed = NA,
      p_value = NA,
      method = "Insufficient data",
      details = "Sample size too small for normality test (n < 3)."
    ))
  }
  
  # If groups are provided, test each group separately
  if (!is.null(group)) {
    group <- droplevels(as.factor(group))
    levels_group <- levels(group)
    results <- list()
    all_passed <- TRUE
    for (lv in levels_group) {
      idx <- which(group == lv)
      sub_x <- x[idx]
      sub_x_clean <- stats::na.omit(sub_x)
      if (length(sub_x_clean) >= 3) {
        # Use Shapiro-Wilk if n <= 5000, else Anderson-Darling (or heuristic)
        if (length(sub_x_clean) <= 5000) {
          sw <- stats::shapiro.test(sub_x_clean)
          p <- sw$p.value
          method <- "Shapiro-Wilk"
        } else {
          # For large samples, use Anderson-Darling from nortest if available, else rely on heuristic
          if (requireNamespace("nortest", quietly = TRUE)) {
            ad <- nortest::ad.test(sub_x_clean)
            p <- ad$p.value
            method <- "Anderson-Darling"
          } else {
            # Heuristic: if sample is large, small deviations from normality are common.
            p <- 0.05  # conservative
            method <- "Heuristic (large sample)"
          }
        }
        passed <- p > 0.05
        all_passed <- all_passed && passed
        results[[lv]] <- list(passed = passed, p_value = p, method = method)
      } else {
        results[[lv]] <- list(passed = NA, p_value = NA, method = "Insufficient data")
        all_passed <- FALSE
      }
    }
    return(list(
      passed = all_passed,
      p_value = NULL,  # no single p-value for grouped
      method = "Group-wise tests",
      group_results = results,
      details = "Normality assessed per group."
    ))
  } else {
    # No grouping: single sample
    if (length(x_clean) <= 5000) {
      sw <- stats::shapiro.test(x_clean)
      p <- sw$p.value
      method <- "Shapiro-Wilk"
    } else {
      if (requireNamespace("nortest", quietly = TRUE)) {
        ad <- nortest::ad.test(x_clean)
        p <- ad$p.value
        method <- "Anderson-Darling"
      } else {
        # Heuristic: large sample -> assume central limit theorem, but warn
        p <- 0.05
        method <- "Heuristic (large sample, consider CLT)"
      }
    }
    passed <- p > 0.05
    return(list(
      passed = passed,
      p_value = p,
      method = method,
      details = if (!passed) "Data may not be normal." else "Data appears normal."
    ))
  }
}

#' Check homogeneity of variances across groups
#'
#' @param x Numeric vector (dependent).
#' @param group Factor (grouping variable).
#' @return A list with passed (logical), p_value, method, details.
#' @keywords internal
check_homogeneity <- function(x, group) {
  
  # Remove missing values pairwise
  valid <- stats::complete.cases(x, group)
  x <- x[valid]
  group <- group[valid]
  group <- droplevels(as.factor(group))
  
  if (length(unique(group)) < 2) {
    return(list(
      passed = NA,
      p_value = NA,
      method = "Not applicable",
      details = "Need at least two groups for homogeneity test."
    ))
  }
  
  # Check minimum group size
  group_sizes <- table(group)
  if (any(group_sizes < 2)) {
    return(list(
      passed = NA,
      p_value = NA,
      method = "Insufficient data",
      details = "Some groups have less than 2 observations."
    ))
  }
  
  # Choose test: Bartlett if data appear normal (but we haven't tested overall), else Levene (robust)
  # For simplicity, we use Bartlett's test (parametric) and also provide Levene as robust alternative.
  # We can decide later based on normality results. For now, we compute both and flag.
  
  # Bartlett's test
  bartlett_result <- tryCatch({
    stats::bartlett.test(x ~ group)
  }, error = function(e) NULL)
  
  # Levene's test (from car package if available, otherwise use a simple version)
  if (requireNamespace("car", quietly = TRUE)) {
    levene_result <- car::leveneTest(x ~ group, center = median)
    levene_p <- levene_result$`Pr(>F)`[1]
    levene_method <- "Levene's test (median)"
  } else {
    # Simple Levene-type: absolute deviations from median
    medians <- tapply(x, group, stats::median, na.rm = TRUE)
    abs_dev <- abs(x - medians[group])
    lm_abs <- stats::lm(abs_dev ~ group)
    anova_abs <- stats::anova(lm_abs)
    levene_p <- anova_abs$`Pr(>F)`[1]
    levene_method <- "Levene-type (absolute deviations from median)"
  }
  
  # For primary recommendation, we may rely on Bartlett if normal, else Levene.
  # But for now, return both and let the calling function decide.
  
  bartlett_p <- if (!is.null(bartlett_result)) bartlett_result$p.value else NA
  bartlett_method <- if (!is.null(bartlett_result)) "Bartlett's test" else "Failed"
  
  # Simple heuristic: if Bartlett p > 0.05, pass; else consider Levene.
  # We'll return both and a suggested interpretation.
  passed_bartlett <- !is.na(bartlett_p) && bartlett_p > 0.05
  passed_levene <- !is.na(levene_p) && levene_p > 0.05
  
  list(
    passed = passed_bartlett,  # default to Bartlett for simplicity
    p_value = bartlett_p,
    method = bartlett_method,
    alternative = list(
      method = levene_method,
      p_value = levene_p,
      passed = passed_levene
    ),
    details = if (passed_bartlett) "Variances appear homogeneous." else "Variances may be heterogeneous (Bartlett test suggests violation). Consider using Welch's correction or non-parametric alternative."
  )
}

#' Print method for smartR_assumptions
#' @param x An object of class smartR_assumptions.
#' @param ... Additional arguments.
#' @export
print.smartR_assumptions <- function(x, ...) {
  cat("\n--- Assumption Checks ---\n")
  
  if (!is.null(x$normality)) {
    cat("Normality:\n")
    if (is.list(x$normality$group_results)) {
      cat("  Group-wise:\n")
      for (g in names(x$normality$group_results)) {
        grp <- x$normality$group_results[[g]]
        status <- if (isTRUE(grp$passed)) "PASSED" else if (isFALSE(grp$passed)) "FAILED" else "NA"
        cat("    ", g, ": ", status, " (p = ", round(grp$p_value, 4), ", ", grp$method, ")\n", sep = "")
      }
    } else {
      status <- if (isTRUE(x$normality$passed)) "PASSED" else if (isFALSE(x$normality$passed)) "FAILED" else "NA"
      cat("  Overall: ", status, " (p = ", round(x$normality$p_value, 4), ", ", x$normality$method, ")\n", sep = "")
    }
  }
  
  if (!is.null(x$homogeneity)) {
    cat("Homogeneity of variance:\n")
    status <- if (isTRUE(x$homogeneity$passed)) "PASSED" else if (isFALSE(x$homogeneity$passed)) "FAILED" else "NA"
    cat("  ", status, " (p = ", round(x$homogeneity$p_value, 4), ", ", x$homogeneity$method, ")\n", sep = "")
    if (!is.null(x$homogeneity$alternative) && !is.na(x$homogeneity$alternative$p_value)) {
      alt_status <- if (isTRUE(x$homogeneity$alternative$passed)) "PASSED" else if (isFALSE(x$homogeneity$alternative$passed)) "FAILED" else "NA"
      cat("  Alternative (Levene): ", alt_status, " (p = ", round(x$homogeneity$alternative$p_value, 4), ")\n", sep = "")
    }
  }
  
  cat("------------------------\n")
  invisible(x)
}
