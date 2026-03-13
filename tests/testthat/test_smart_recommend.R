test_that("smart_recommend runs without errors", {
  
  result <- smart_recommend(iris, "Sepal.Length", "Species", output = "compact")
  expect_s3_class(result, "smartR_recommendation")
  
  result2 <- smart_recommend(iris, "Sepal.Length", "Species", output = "full")
  expect_s3_class(result2, "smartR_recommendation")
})
