test_that("SLOS generates expected outputs", {
  data(SampledData)
  mock_data <- sampled_data
  
  result <- suppressWarnings(SLOS::SLOS(mock_data))
  
  expect_is(result, "list")
  expect_named(result, c("df_unit_slos", "plot_SLOS_obs_prev", "funnel_plot"))
})

test_that("SLOS handles empty or invalid data", {
  expect_error(SLOS(data.frame()), "Error: Input data must contain at least one row.")
  
  invalid_data <- data.frame(InvalidColumn = c(1, 2, 3))
  expect_error(SLOS(invalid_data), "Error: Missing required columns: UnitCode, UnitLengthStay_trunc")
})