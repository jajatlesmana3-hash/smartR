test_that("detect_data_structure works with various inputs", {
  
  # data.frame
  df <- data.frame(
    num = rnorm(50),
    fac = factor(sample(letters[1:3], 50, replace = TRUE)),
    char = sample(c("x","y"), 50, replace = TRUE),
    int = 1:50
  )
  
  info <- detect_data_structure(df, "num", "fac")
  expect_equal(info$dependent$name, "num")
  expect_equal(info$dependent$type_info$main_type, "NUMERIC")
  expect_equal(info$independent$type_info$main_type, "CATEGORICAL")
  
  # vector
  vec <- rnorm(30)
  info <- detect_data_structure(vec, "value")
  expect_equal(info$dependent$name, "value")
  expect_equal(info$dependent$type_info$main_type, "NUMERIC")
  
  # missing column error
  expect_error(detect_data_structure(df, "nonexistent"), "not found")
})
