#' Main function: recommend statistical method based on data
#'
#' This is the primary function of the smartR package. It takes data and variable
#' names, automatically detects data types, checks assumptions, and recommends
#' the most appropriate statistical test. It also provides ready-to-use R code.
#'
#' @param data A data.frame, tibble, matrix, or vector containing the data.
#' @param var_dependent Character string; name of the dependent variable column.
#' @param var_independent Character string; name of the independent variable column
#'        (optional for one-sample tests).
#' @param test_type Character; one of "auto" (default), "comparison", or "association".
#'        "auto" infers from variable types.
#' @param output Character; one of "compact" (default) or "full". "compact" shows
#'        main recommendation and code; "full" shows all details including assumptions.
#' @param ... Additional arguments passed to the underlying test (e.g., mu, paired).
#'
#' @return An object of class 'smartR_recommendation' (invisibly) and prints
#'         the recommendation to the console.
#'
#' @examples
#' \dontrun{
#' data(iris)
#' smart_recommend(iris, "Sepal.Length", "Species")
#' smart_recommend(iris, "Sepal.Length", "Species", output = "full")
#' }
#' @export
smart_recommend <- function(data,
                            var_dependent,
                            var_independent = NULL,
                            test_type = c("auto", "comparison", "association"),
                            output = c("compact", "full"),
                            ...) {
  
  test_type <- match.arg(test_type)
  output <- match.arg(output)
  
  # Step 1: Detect data structure
  data_info <- detect_data_structure(data, var_dependent, var_independent)
  
  # Step 2: Check assumptions
  assumptions <- check_assumptions(data, var_dependent, var_independent, data_info = data_info)
  
  # Step 3: Make decision
  recommendation <- decide_method(
    data_info = data_info,
    assumptions = assumptions,
    data = data,
    var_dependent = var_dependent,
    var_independent = var_independent,
    test_type = test_type
  )
  
  # Step 4: Generate code
  data_name <- deparse(substitute(data))
  code <- generate_code(
    recommendation = recommendation,
    data_info = data_info,
    data_name = data_name,
    var_dependent = var_dependent,
    var_independent = var_independent,
    ...
  )
  recommendation$code <- code
  
  # Step 5: Store additional info for printing
  recommendation$data_info <- data_info
  recommendation$assumptions <- assumptions
  recommendation$output_type <- output
  
  # Set class and print
  class(recommendation) <- c("smartR_recommendation", "list")
  
  # Print based on output type
  if (output == "compact") {
    print_compact(recommendation)
  } else {
    print(recommendation)  # uses the S3 method defined in decision_logic.R
  }
  
  invisible(recommendation)
}

#' Compact print for smartR recommendations
#' @param x A smartR_recommendation object
#' @keywords internal
print_compact <- function(x) {
  cat("\n==================== smartR RECOMMENDATION ====================\n\n")
  
  cat("PRIMARY RECOMMENDATION\n")
  cat("  Method: ", x$method, "\n")
  cat("  Confidence: ", x$confidence, "/5\n\n")
  
  cat("READY-TO-USE CODE\n")
  cat("  ", x$code, "\n\n")
  
  if (length(x$notes) > 0) {
    cat("NOTES\n")
    for (n in x$notes) {
      cat("  • ", n, "\n")
    }
    cat("\n")
  }
  
  cat("================================================================\n")
  cat("Use summary() for detailed assumption checks.\n")
}
