#' Check statistical assumptions (normality, homogeneity)
#'
#' @param data Data input (data.frame)
#' @param var_dependent Nama/indeks/vektor variabel dependen
#' @param var_independent Nama/indeks/vektor variabel independen (opsional)
#' @param data_info Output dari detect_data_structure (opsional, untuk efisiensi)
#' @return List hasil pemeriksaan asumsi
#' @export
check_assumptions <- function(data, var_dependent = NULL, var_independent = NULL,
                              data_info = NULL) {
  
  if (is.null(data_info)) {
    data_info <- detect_data_structure(data, var_dependent, var_independent)
  }
  
  result <- list()
  
  if (is.null(data_info$dependent)) {
    warning("Tidak ada informasi variabel dependen.")
    return(result)
  }
  
  dep <- data_info$dependent
  indep <- data_info$independent
  
  # Normality check (jika dependen numerik)
  if (dep$type_info$main_type == "NUMERIC") {
    result$normality <- check_normality(dep$data, group = if (!is.null(indep)) indep$data else NULL)
  } else {
    result$normality <- list(passed = NA, method = "Not applicable (non-numeric)")
  }
  
  # Homogeneity check (jika dependen numerik dan independen kategorikal)
  if (dep$type_info$main_type == "NUMERIC" && !is.null(indep) &&
      indep$type_info$main_type == "CATEGORICAL") {
    result$homogeneity <- check_homogeneity(dep$data, indep$data)
  } else {
    result$homogeneity <- list(passed = NA, method = "Not applicable")
  }
  
  class(result) <- "smartR_assumptions"
  return(result)
}

#' Normality test (internal)
#' @param x Numeric vector
#' @param group Optional grouping factor
#' @keywords internal
check_normality <- function(x, group = NULL) {
  x_clean <- stats::na.omit(x)
  if (length(x_clean) < 3) {
    return(list(passed = NA, method = "Insufficient data (n<3)", p_value = NA))
  }
  
  if (!is.null(group)) {
    group <- droplevels(as.factor(group))
    levs <- levels(group)
    results <- list()
    all_passed <- TRUE
    for (lv in levs) {
      idx <- which(group == lv)
      sub_x <- x[idx]
      sub_x_clean <- stats::na.omit(sub_x)
      if (length(sub_x_clean) >= 3) {
        if (length(sub_x_clean) <= 5000) {
          sw <- stats::shapiro.test(sub_x_clean)
          p <- sw$p.value
          method <- "Shapiro-Wilk"
        } else {
          p <- 0.05  # heuristic
          method <- "Heuristic (large sample)"
        }
        passed <- p > 0.05
        all_passed <- all_passed && passed
        results[[lv]] <- list(passed = passed, p_value = p, method = method)
      } else {
        results[[lv]] <- list(passed = NA, p_value = NA, method = "Insufficient data")
        all_passed <- FALSE
      }
    }
    return(list(passed = all_passed, group_results = results, method = "Group-wise tests"))
  } else {
    if (length(x_clean) <= 5000) {
      sw <- stats::shapiro.test(x_clean)
      p <- sw$p.value
      method <- "Shapiro-Wilk"
    } else {
      p <- 0.05
      method <- "Heuristic (large sample)"
    }
    passed <- p > 0.05
    return(list(passed = passed, p_value = p, method = method))
  }
}

#' Homogeneity test (internal)
#' @param x Numeric vector
#' @param group Factor
#' @keywords internal
check_homogeneity <- function(x, group) {
  valid <- stats::complete.cases(x, group)
  x <- x[valid]
  group <- group[valid]
  group <- droplevels(as.factor(group))
  
  if (length(unique(group)) < 2) {
    return(list(passed = NA, method = "Need at least 2 groups", p_value = NA))
  }
  
  group_sizes <- table(group)
  if (any(group_sizes < 2)) {
    return(list(passed = NA, method = "Some groups have <2 obs", p_value = NA))
  }
  
  bt <- tryCatch(stats::bartlett.test(x ~ group), error = function(e) NULL)
  if (!is.null(bt)) {
    return(list(passed = bt$p.value > 0.05, p_value = bt$p.value, method = "Bartlett test"))
  } else {
    return(list(passed = NA, method = "Bartlett test failed", p_value = NA))
  }
}

#' Print method for assumptions
#' @export
print.smartR_assumptions <- function(x, ...) {
  cat("\nAssumption Checks:\n")
  if (!is.null(x$normality)) {
    cat("  Normality: ")
    if (!is.null(x$normality$group_results)) {
      cat("per group\n")
      for (g in names(x$normality$group_results)) {
        grp <- x$normality$group_results[[g]]
        status <- if (isTRUE(grp$passed)) "PASSED" else if (isFALSE(grp$passed)) "FAILED" else "NA"
        pval <- if (!is.null(grp$p_value) && !is.na(grp$p_value)) sprintf("(p=%.3f)", grp$p_value) else ""
        cat("    ", g, ": ", status, " ", pval, "\n", sep = "")
      }
    } else {
      status <- if (isTRUE(x$normality$passed)) "PASSED" else if (isFALSE(x$normality$passed)) "FAILED" else "NA"
      pval <- if (!is.null(x$normality$p_value) && !is.na(x$normality$p_value)) sprintf("(p=%.3f)", x$normality$p_value) else ""
      cat(status, " ", pval, "\n", sep = "")
    }
  }
  if (!is.null(x$homogeneity)) {
    cat("  Homogeneity of variance: ")
    status <- if (isTRUE(x$homogeneity$passed)) "PASSED" else if (isFALSE(x$homogeneity$passed)) "FAILED" else "NA"
    pval <- if (!is.null(x$homogeneity$p_value) && !is.na(x$homogeneity$p_value)) sprintf("(p=%.3f)", x$homogeneity$p_value) else ""
    cat(status, " ", pval, "\n", sep = "")
  }
}
