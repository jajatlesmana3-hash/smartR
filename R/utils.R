#' Utility functions for smartR (internal)
#' @keywords internal
NULL

# Star rating (tidak dipakai sekarang, tapi bisa berguna)
star_rating <- function(conf) {
  paste(rep("★", conf), collapse = "")
}

# Format p-value
format_pval <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

# Cek apakah suatu vektor adalah binary
is_binary <- function(x) {
  length(unique(stats::na.omit(x))) == 2
}

# Cek apakah package tersedia
require_package <- function(pkg, install = FALSE) {
  if (requireNamespace(pkg, quietly = TRUE)) return(TRUE)
  if (install) {
    ans <- readline(prompt = sprintf("Package '%s' diperlukan. Install? (y/n) ", pkg))
    if (tolower(ans) == "y") {
      install.packages(pkg)
      return(requireNamespace(pkg, quietly = TRUE))
    }
  }
  FALSE
}
