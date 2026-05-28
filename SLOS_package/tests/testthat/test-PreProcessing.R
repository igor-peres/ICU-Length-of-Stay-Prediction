test_that("slos_pre_processing screens and (optionally) imputes", {
  
  set.seed(123)
  data(SampledData)
  
  df <- sampled_data
  
  pp <- slos_pre_processing(
    data            = df,
    outcome         = "UnitLengthStay_trunc",
    remove_nzv      = TRUE,
    nzv_freq_cut    = 50,
    remove_corr_num = TRUE,
    corr_cutoff_num = 0.8,
    remove_corr_cat = TRUE,
    corr_cutoff_cat = 0.8,
    do_impute       = TRUE,
    m               = 1,
    maxit           = 1,
    seed            = 42
  )
  
  expect_true(is.list(pp))
  expect_true(all(c("data_screened","removed","screening_metrics","split") %in% names(pp)))
  expect_true("UnitLengthStay_trunc" %in% names(pp$data_screened))
  
  expect_true(is.character(pp$removed$nzv))
  expect_true(is.character(pp$removed$corr_num))
  expect_true(is.character(pp$removed$corr_cat))
  
  expect_true(all(c("nzv_table","num_removed_table","cat_removed_table") %in% names(pp$screening_metrics)))
  
  expect_true("imputation" %in% names(pp))
  expect_true(is.data.frame(pp$imputation$train_completed))
  expect_true(is.data.frame(pp$imputation$test_completed))
  expect_true(is.matrix(pp$imputation$predictor_matrix) || is.null(pp$imputation$predictor_matrix))
  expect_true(is.numeric(pp$imputation$pm_density) || is.na(pp$imputation$pm_density))
})
