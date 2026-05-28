test_that("slos_feature_select_rfe returns a compact RFE object and plots", {

  set.seed(123)
  data(SampledData)
  dat <- sampled_data
  
  n <- 30
  
  res <- slos_feature_select_rfe(
    data      = dat,
    outcome   = "UnitLengthStay_trunc",
    method    = "treebag",
    sizes     = c(1, 2, 3),
    normalize = TRUE,
    cv_folds  = 3,
    seed      = 999,
    verbose   = FALSE
  )
  
  expect_s3_class(res, "slos_rfe_result")
  expect_true(is.character(res$selected_vars))
  expect_true(length(res$selected_vars) >= 1)
  expect_true(is.list(res$rfe))
})

test_that("slos_apply_feature_selection returns a pruned data.frame in correct order", {

  
  set.seed(321)
  
  n <- 20
  data(SampledData)
  dat <- sampled_data

  
  res <- slos_feature_select_rfe(
    data      = dat,
    outcome   = "UnitLengthStay_trunc",
    method    = "treebag",
    sizes     = c(1, 2),
    normalize = FALSE,
    cv_folds  = 3,
    verbose   = FALSE
  )
  
  Xsel <- slos_apply_feature_selection(dat, res)
  expect_true(is.data.frame(Xsel))
  expect_true(all(colnames(Xsel) %in% res$selected_vars))
  expect_identical(colnames(Xsel), intersect(res$selected_vars, names(dat)))
})
