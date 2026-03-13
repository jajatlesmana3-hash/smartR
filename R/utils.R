#' Utility functions for smartR
#'
#' This file contains helper functions used internally by smartR.
#' 
#' @keywords internal
#' @name smartR-utils
NULL

#' Convert confidence score to star rating
#' @param conf Integer 1-5
#' @return Character string with stars
#' @keywords internal
star_rating <- function(conf) {
  stars <- paste(rep("★", conf), collapse = "")
  if (conf < 5) {
    stars <- paste0(stars, paste(rep("☆", 5 - conf), collapse = ""))
  }
  stars
}

#' Check if a package is installed, with optional installation prompt
#' @param pkg Package name
#' @param install Whether to prompt for installation if missing
#' @return Logical
#' @keywords internal
require_package <- function(pkg, install = FALSE) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(TRUE)
  }
  if (install) {
    ans <- readline(prompt = sprintf("Package '%s' is required. Install now? (y/n) ", pkg))
    if (tolower(ans) == "y") {
      install.packages(pkg)
      return(requireNamespace(pkg, quietly = TRUE))
    }
  }
  FALSE
}

#' Safely get column from data frame
#' @param data A data.frame
#' @param col Column name
#' @return Column vector or NULL if not exists
#' @keywords internal
safe_col <- function(data, col) {
  if (col %in% names(data)) data[[col]] else NULL
}

#' Format p-value for display
#' @param p Numeric p-value
#' @return Character string
#' @keywords internal
format_pval <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

#' Check if a variable is likely binary
#' @param x Vector
#' @return Logical
#' @keywords internal
is_binary <- function(x) {
  u <- unique(stats::na.omit(x))
  length(u) == 2
}

#' Get a brief description of variable type
#' @param type_info Output from classify_variable_type
#' @return Character string
#' @keywords internal
type_description <- function(type_info) {
  paste0(type_info$main_type, " (", type_info$sub_type, ")")
}
