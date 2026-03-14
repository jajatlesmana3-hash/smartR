#' Detect and classify the structure of input data
#'
#' @param data Data input (data.frame, vector, etc.)
#' @param var_dependent Nama, indeks, atau vektor variabel dependen
#' @param var_independent Nama, indeks, atau vektor variabel independen (opsional)
#' @return List informasi data
#' @export
detect_data_structure <- function(data, var_dependent = NULL, var_independent = NULL) {
  
  # Standarisasi input ke data.frame
  data_std <- standardize_input(data)
  
  if (is.null(data_std)) {
    stop("Format data tidak dikenali. Gunakan data.frame, matrix, atau vector.")
  }
  
  result <- list()
  
  # Fungsi untuk mendapatkan kolom dari berbagai tipe input
  get_column <- function(data, var) {
    if (is.null(var)) return(NULL)
    
    # Jika var adalah vektor/factor (data langsung)
    if (is.vector(var) || is.factor(var)) {
      return(var)
    }
    
    # Jika data adalah data.frame
    if (is.data.frame(data)) {
      # Jika var adalah string (nama kolom)
      if (is.character(var) && length(var) == 1) {
        if (var %in% names(data)) {
          return(data[[var]])
        } else {
          stop("Kolom '", var, "' tidak ditemukan dalam data.")
        }
      }
      # Jika var adalah numerik (indeks kolom)
      if (is.numeric(var) && length(var) == 1) {
        if (var >= 1 && var <= ncol(data)) {
          return(data[[var]])
        } else {
          stop("Indeks kolom ", var, " di luar jangkauan (1-", ncol(data), ").")
        }
      }
    }
    
    stop("Tidak dapat mengekstrak kolom dari input yang diberikan.")
  }
  
  # Proses variabel dependen
  if (!is.null(var_dependent)) {
    dep <- get_column(data_std, var_dependent)
    result$dependent <- list(
      name = if (is.character(var_dependent) && length(var_dependent) == 1) var_dependent else deparse(substitute(var_dependent)),
      data = dep,
      class = class(dep)[1],
      type_info = classify_variable_type(dep),
      n_unique = length(unique(stats::na.omit(dep))),
      n_missing = sum(is.na(dep)),
      levels = if (is.factor(dep)) levels(dep) else NULL
    )
  }
  
  # Proses variabel independen
  if (!is.null(var_independent)) {
    ind <- get_column(data_std, var_independent)
    result$independent <- list(
      name = if (is.character(var_independent) && length(var_independent) == 1) var_independent else deparse(substitute(var_independent)),
      data = ind,
      class = class(ind)[1],
      type_info = classify_variable_type(ind),
      n_unique = length(unique(stats::na.omit(ind))),
      n_missing = sum(is.na(ind)),
      levels = if (is.factor(ind)) levels(ind) else unique(ind)
    )
  }
  
  # Overview data
  result$overview <- list(
    nrow = nrow(data_std),
    ncol = ncol(data_std),
    complete_cases = sum(stats::complete.cases(data_std)),
    missing_cases = sum(!stats::complete.cases(data_std))
  )
  
  class(result) <- "smartR_data_detection"
  return(result)
}

#' Standardisasi berbagai input ke data.frame
#' @param x Input
#' @return data.frame atau NULL
#' @keywords internal
standardize_input <- function(x) {
  if (is.data.frame(x)) return(as.data.frame(x))
  if (is.matrix(x)) return(as.data.frame(x))
  if (is.vector(x) || is.factor(x)) return(data.frame(value = x))
  if (is.list(x) && !is.data.frame(x)) {
    tryCatch(as.data.frame(x), error = function(e) NULL)
  } else NULL
}

#' Klasifikasi tipe variabel
#' @param x Vektor
#' @return List dengan main_type, sub_type, description
#' @keywords internal
classify_variable_type <- function(x) {
  x_clean <- stats::na.omit(x)
  if (length(x_clean) == 0) {
    return(list(main_type = "UNKNOWN", sub_type = "UNKNOWN", description = "All values missing"))
  }
  
  # Factor
  if (is.factor(x)) {
    if (is.ordered(x)) return(list(main_type = "CATEGORICAL", sub_type = "ORDINAL", description = "Ordered factor"))
    else return(list(main_type = "CATEGORICAL", sub_type = "NOMINAL", description = "Unordered factor"))
  }
  
  # Character
  if (is.character(x)) {
    if (!any(is.na(suppressWarnings(as.numeric(x_clean))))) {
      return(list(main_type = "NUMERIC", sub_type = "INTERVAL", description = "Character but convertible to numeric"))
    } else {
      n_unique <- length(unique(x_clean))
      if (n_unique <= 10) {
        return(list(main_type = "CATEGORICAL", sub_type = "NOMINAL", description = paste0("Character with ", n_unique, " categories")))
      } else {
        warning("Character variable with >10 unique values. Treating as categorical, but verify your data.")
        return(list(main_type = "CATEGORICAL", sub_type = "NOMINAL", description = "Character with many unique values"))
      }
    }
  }
  
  # Numeric
  if (is.numeric(x)) {
    n_unique <- length(unique(x_clean))
    if (n_unique <= 5) {
      return(list(main_type = "CATEGORICAL", sub_type = "NOMINAL", description = paste0("Numeric with only ", n_unique, " distinct values (categorical?)")))
    } else {
      return(list(main_type = "NUMERIC", sub_type = "INTERVAL", description = "Continuous numeric"))
    }
  }
  
  # Logical
  if (is.logical(x)) {
    return(list(main_type = "CATEGORICAL", sub_type = "NOMINAL", description = "Logical (TRUE/FALSE)"))
  }
  
  list(main_type = "UNKNOWN", sub_type = "UNKNOWN", description = paste("Class:", paste(class(x), collapse = ", ")))
}

#' Print method
#' @export
print.smartR_data_detection <- function(x, ...) {
  cat("\nsmartR Data Detection\n")
  if (!is.null(x$dependent)) {
    cat("Dependent:\n")
    cat("  Name: ", x$dependent$name, "\n")
    cat("  Type: ", x$dependent$type_info$main_type, " (", x$dependent$type_info$sub_type, ")\n", sep = "")
    cat("  Unique: ", x$dependent$n_unique, "\n")
    cat("  Missing: ", x$dependent$n_missing, "\n")
  }
  if (!is.null(x$independent)) {
    cat("Independent:\n")
    cat("  Name: ", x$independent$name, "\n")
    cat("  Type: ", x$independent$type_info$main_type, " (", x$independent$type_info$sub_type, ")\n", sep = "")
    cat("  Unique: ", x$independent$n_unique, "\n")
    cat("  Missing: ", x$independent$n_missing, "\n")
  }
  cat("Overview: ", x$overview$nrow, " rows, ", x$overview$ncol, " cols, ", 
      x$overview$complete_cases, " complete cases\n")
}
