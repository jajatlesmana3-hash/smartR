.onAttach <- function(libname, pkgname) {
  packageStartupMessage("
smartR version 0.1.0
Copyright (c) 2026 Jajat L
License: MIT

smartR helps you choose the right statistical test.

Quick start:
  result <- smart_rec(iris, sample1 = 'Sepal.Length', sample2 = 'Species', 
                      test_type = 'comparison', paired = FALSE)
  result
  smart_dec(result)

For help: https://github.com/jajatlesmana3-hash/smartR
")
}
