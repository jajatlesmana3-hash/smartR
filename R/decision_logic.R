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
#' @export
#'
#' @examples
#' \dontrun{
#' data(iris)
#' info <- detect_data_structure(iris, "Sepal.Length", "Species")
#' decide_method(info)
#' }
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
    recommendation$confidence <- 3
  }
  
  # Add references
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
      norm_pass <- if
