#' .onAttatch hook for smartR
#'
#' This function runs when the package is attached via library(smartR).
#' It displays a welcome message with copyright and quick start information.
#'
#' @param libname Library name
#' @param pkgname Package name
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("
smartR version 0.1.0
Copyright (c) 2026 Jajat L
License: MIT

smartR helps you choose the right statistical test based on your data.

Quick start:
  result <- smart_recommend(iris, 'Sepal.Length', 'Species')
  result

For detailed documentation:
  vignette('introduction', package = 'smartR')
  https://github.com/jajatlesmana3-hash/smartR
")
}
