test_that("decide_method gives appropriate recommendations", {
  
  df <- data.frame(
    y = rnorm(100),
    group = factor(sample(c("A","B"), 100, replace = TRUE))
  )
  info <- detect_data_structure(df, "y", "group")
  assumptions <- check_assumptions(df, "y", "group", data_info = info)
  
  rec <- decide_method(info, assumptions)
  expect_true(grepl("t-test|Mann-Whitney", rec$method))
  
  # One-sample
  info2 <- detect_data_structure(df, "y")
  rec2 <- decide_method(info2)
  expect_true(grepl("One-Sample|Wilcoxon", rec2$method))
})
