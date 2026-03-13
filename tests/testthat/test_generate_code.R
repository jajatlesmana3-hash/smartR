test_that("generate_code creates valid R code", {
  
  df <- data.frame(y = rnorm(50), x = factor(rep(c("A","B"), 25)))
  info <- detect_data_structure(df, "y", "x")
  rec <- decide_method(info)
  
  code <- generate_code(rec, info, "df", "y", "x")
  expect_true(is.character(code))
  expect_true(grepl("t.test|wilcox.test", code))
})
