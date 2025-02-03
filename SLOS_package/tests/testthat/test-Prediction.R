test_that("predict_and_evaluate works as expected", {
  data("SampledData")
  result <- predict_and_evaluate(sampled_data)
  
  expect_is(result, "list")
  expect_named(result, c("predictions", "comparison", "RMSE", "MAE", "R2"))
  expect_true(is.numeric(result$RMSE))
  expect_true(is.numeric(result$MAE))
  expect_true(is.numeric(result$R2))
})
