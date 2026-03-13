#' Detect and classify the structure of input data
#'
#' This function standardizes various input formats (data.frame, tibble, matrix,
#' vector, list) into a consistent data.frame and extracts detailed information
#' about the dependent and independent variables.
#'
#' @param data Input data. Can be a data.frame, tibble, data.table, matrix,
#'   vector, or list coercible to data.frame.
#' @param var_dependent Character string; name of the dependent variable column.
#'   If NULL, the function assumes one-sample analysis.
#' @param var_independent Character string; name of the independent variable column.
#'   Optional; if NULL, only dependent variable info is returned.
#'
#' @return A list with components:
#'   \item{dependent}{List of details about the dependent variable}
#'   \item{independent}{List of details about the independent variable (if provided)}
#'   \item{overview}{Global information about the dataset}
#'
#' @examples
#' \dontrun{
#' data(iris)
#' detect_data_structure(iris, "Sepal.Length", "Species")
#' }
#' @export
detect_data_structure <- function(data, var_dependent = NULL, var_independent = NULL) {
  
  # Step 1: Standardize input to a plain data.frame
  data_std <- standardize_input(data)
  
  # If standardization fails, stop with a helpful error
  if (is.null(data_std)) {
    stop(
      "\n============================================================\n",
      "smartR Error: Unable to process input data format.\n\n",
      "Supported formats:\n",
      "• data.frame, tibble, data.table\n",
      "• matrix\n",
      "• vector (for one-sample tests)\n",
      "• list coercible to data.frame\n\n",
      "Please ensure your data is in one of these formats.\n",
      "============================================================"
    )
  }
  
  result <- list()
  
  # Step 2: Process dependent variable if specified
  if (!is.null(var_dependent)) {
    if (!var_dependent %in% names(data_std)) {
      stop(
        "\n============================================================\n",
        "smartR Error: Column '", var_dependent, "' not found in data.\n\n",
        "Available columns: ", paste(names(data_std), collapse = ", "), "\n",
        "============================================================"
      )
    }
    
    dep_var <- data_std[[var_dependent]]
    result$dependent <- list(
      name = var_dependent,
      data = dep_var,
      class = class(dep_var)[1],
      type_info = classify_variable_type(dep_var),
      n_unique = length(unique(stats::na.omit(dep_var))),
      n_missing = sum(is.na(dep_var)),
      levels = if (is.factor(dep_var)) levels(dep_var) else NULL
    )
  }
  
  # Step 3: Process independent variable if specified
  if (!is.null(var_independent)) {
    if (!var_independent %in% names(data_std)) {
      stop(
        "\n============================================================\n",
        "smartR Error: Column '", var_independent, "' not found in data.\n\n",
        "Available columns: ", paste(names(data_std), collapse = ", "), "\n",
        "============================================================"
      )
    }
    
    indep_var <- data_std[[var_independent]]
    result$independent <- list(
      name = var_independent,
      data = indep_var,
      class = class(indep_var)[1],
      type_info = classify_variable_type(indep_var),
      n_unique = length(unique(stats::na.omit(indep_var))),
      n_missing = sum(is.na(indep_var)),
      levels = if (is.factor(indep_var)) levels(indep_var) else unique(indep_var)
    )
  }
  
  # Step 4: Global overview
  result$overview <- list(
    nrow = nrow(data_std),
    ncol = ncol(data_std),
    complete_cases = sum(stats::complete.cases(data_std)),
    missing_cases = sum(!stats::complete.cases(data_std))
  )
  
  # Add a class for potential S3 methods
  class(result) <- "smartR_data_detection"
  
  return(result)
}

#' Standardize various R objects to a plain data.frame
#'
#' @param x An R object.
#' @return A data.frame or NULL if conversion is not possible.
#' @keywords internal
standardize_input <- function(x) {
  
  # Already a data.frame? Convert to plain data.frame (removes tibble/data.table attributes)
  if (is.data.frame(x)) {
    return(as.data.frame(x))
  }
  
  # Matrix
  if (is.matrix(x)) {
    return(as.data.frame(x))
  }
  
  # Vector or factor (single variable)
  if (is.vector(x) || is.factor(x)) {
    return(data.frame(value = x, stringsAsFactors = FALSE))
  }
  
  # List that can be coerced to data.frame
  if (is.list(x) && !is.data.frame(x)) {
    tryCatch({
      df <- as.data.frame(x, stringsAsFactors = FALSE)
      if (ncol(df) > 0) return(df)
    }, error = function(e) NULL)
  }
  
  # Unsupported type
  return(NULL)
}

#' Classify a variable into statistical types
#'
#' Determines whether a variable is NUMERIC (continuous) or CATEGORICAL
#' (nominal/ordinal), with subtypes.
#'
#' @param x A vector.
#' @return A list with components main_type, sub_type, and description.
#' @keywords internal
classify_variable_type <- function(x) {
  
  x_clean <- stats::na.omit(x)
  if (length(x_clean) == 0) {
    return(list(
      main_type = "UNKNOWN",
      sub_type = "UNKNOWN",
      description = "All values are missing"
    ))
  }
  
  # 1. Factor
  if (is.factor(x)) {
    if (is.ordered(x)) {
      return(list(
        main_type = "CATEGORICAL",
        sub_type = "ORDINAL",
        description = "Ordered factor"
      ))
    } else {
      return(list(
        main_type = "CATEGORICAL",
        sub_type = "NOMINAL",
        description = "Unordered factor"
      ))
    }
  }
  
  # 2. Character
  if (is.character(x)) {
    # Check if character actually represents numbers
    if (!any(is.na(suppressWarnings(as.numeric(x_clean))))) {
      return(list(
        main_type = "NUMERIC",
        sub_type = "INTERVAL",
        description = "Character but convertible to numeric"
      ))
    } else {
      n_unique <- length(unique(x_clean))
      if (n_unique <= 10) {
        return(list(
          main_type = "CATEGORICAL",
          sub_type = "NOMINAL",
          description = paste0("Character with ", n_unique, " categories")
        ))
      } else {
        warning("Character variable with >10 unique values. Treating as categorical, but verify your data.")
        return(list(
          main_type = "CATEGORICAL",
          sub_type = "NOMINAL",
          description = "Character with many unique values (possibly text?)"
        ))
      }
    }
  }
  
  # 3. Numeric (integer or double)
  if (is.numeric(x)) {
    n_unique <- length(unique(x_clean))
    # If only a few distinct values, it might be a disguised categorical variable
    if (n_unique <= 5) {
      return(list(
        main_type = "CATEGORICAL",
        sub_type = "NOMINAL",
        description = paste0("Numeric with only ", n_unique, " distinct values (could be categorical)")
      ))
    } else {
      return(list(
        main_type = "NUMERIC",
        sub_type = "INTERVAL",
        description = "Continuous numeric"
      ))
    }
  }
  
  # 4. Logical
  if (is.logical(x)) {
    return(list(
      main_type = "CATEGORICAL",
      sub_type = "NOMINAL",
      description = "Logical (TRUE/FALSE)"
    ))
  }
  
  # 5. Fallback
  return(list(
    main_type = "UNKNOWN",
    sub_type = "UNKNOWN",
    description = paste("Class:", paste(class(x), collapse = ", "))
  ))
}

#' Print method for smartR_data_detection objects (optional)
#' @param x An object of class smartR_data_detection.
#' @param ... Additional arguments.
#' @export
print.smartR_data_detection <- function(x, ...) {
  cat("\n--- smartR Data Detection ---\n")
  if (!is.null(x$dependent)) {
    cat("Dependent variable:\n")
    cat("  Name: ", x$dependent$name, "\n")
    cat("  Type: ", x$dependent$type_info$main_type, " (", x$dependent$type_info$sub_type, ")\n", sep="")
    cat("  Unique values: ", x$dependent$n_unique, "\n")
  }
  if (!is.null(x$independent)) {
    cat("Independent variable:\n")
    cat("  Name: ", x$independent$name, "\n")
    cat("  Type: ", x$independent$type_info$main_type, " (", x$independent$type_info$sub_type, ")\n", sep="")
    cat("  Unique values: ", x$independent$n_unique, "\n")
  }
  cat("Overview:\n")
  cat("  Rows: ", x$overview$nrow, ", Complete cases: ", x$overview$complete_cases, "\n")
  cat("---\n")
  invisible(x)
}
