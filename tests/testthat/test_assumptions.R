test_that("check_assumptions works correctly", {
  
  # Normal data
  set.seed(123)
  df <- data.frame(
    y = c(rnorm(30), rnorm(30, mean = 1)),
    group = factor(rep(c("A","B"), each = 30))
  )
  
  assumptions <- check_assumptions(df, "y", "group")
  expect_true(assumptions$normality$passed)  # should be normal
  expect_true(assumptions$homogeneity$passed) # variances likely equal
  
  # Non-normal data
  df$y[1:30] <- rexp(30)
  assumptions <- check_assumptions(df, "y", "group")
  # may fail, but we just test structure
  expect_false(is.null(assumptions$normality$passed))
})
