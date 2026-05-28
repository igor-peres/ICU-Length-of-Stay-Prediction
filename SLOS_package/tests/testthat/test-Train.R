test_that("slos_train_new_model trains a stack and returns expected objects", {
  skip_if_not_installed("caret")
  skip_if_not_installed("caretEnsemble")
  skip_if_not_installed("ranger")
  
  set.seed(42)
  n <- 40
  df <- data.frame(
    UnitLengthStay_trunc = rnorm(n, 5, 2),
    x1 = rnorm(n),
    x2 = rnorm(n),
    cat = factor(sample(c("A","B"), n, TRUE))
  )
  
  base_grid <- data.frame(.mtry = 1, .splitrule = "variance", .min.node.size = 5)
  meta_grid <- expand.grid(mtry = 1, min.node.size = 5, splitrule = "variance")
  
  res <- slos_train_new_model(
    train = df,
    outcome = "UnitLengthStay_trunc",
    metric = "RMSE",
    cv_folds = 2,
    cv_method = "cv",
    verbose_iter = FALSE,
    seed = 42,
    base_ranger_grid = base_grid,
    meta_grid = meta_grid
  )
  
  expect_type(res, "list")
  expect_true(all(c("stacked_model", "base_models", "train_control") %in% names(res)))
  expect_s3_class(res$stacked_model, "caretStack")
  expect_s3_class(res$base_models, "caretList")
  # Some caret versions return a plain list here; check structure instead of class.
  expect_type(res$train_control, "list")
  expect_true(all(c("method", "number", "savePredictions") %in% names(res$train_control)))
  expect_true(length(res$base_models) >= 1)
})

test_that("slos_train_new_model errors with missing or non-numeric outcome", {
  df1 <- data.frame(a = 1:5, b = 5:1)
  expect_error(
    slos_train_new_model(df1, outcome = "UnitLengthStay_trunc", verbose_iter = FALSE, cv_folds = 2),
    "Outcome column 'UnitLengthStay_trunc' not found"
  )
  
  df2 <- data.frame(UnitLengthStay_trunc = factor(c("A","B","A","B","A")), x = rnorm(5))
  expect_error(
    slos_train_new_model(df2, outcome = "UnitLengthStay_trunc", verbose_iter = FALSE, cv_folds = 2),
    "must be numeric for regression"
  )
})

test_that("slos_train_new_model saves model when save_path is provided", {
  skip_if_not_installed("caret")
  skip_if_not_installed("caretEnsemble")
  skip_if_not_installed("ranger")
  
  set.seed(7)
  n <- 20
  df <- data.frame(
    UnitLengthStay_trunc = rnorm(n),
    x = rnorm(n),
    f = factor(sample(c("A","B"), n, TRUE))
  )
  
  tmp <- file.path(tempdir(), "stack_model_test") # no extension on purpose
  
  base_grid <- data.frame(.mtry = 1, .splitrule = "variance", .min.node.size = 5)
  meta_grid <- expand.grid(mtry = 1, min.node.size = 5, splitrule = "variance")
  
  res <- slos_train_new_model(
    train = df,
    save_path = tmp,
    verbose_iter = FALSE,
    cv_folds = 2,
    base_ranger_grid = base_grid,
    meta_grid = meta_grid
  )
  
  expect_s3_class(res$stacked_model, "caretStack")
  
  saved_path <- paste0(tmp, ".RData")
  expect_true(file.exists(saved_path))
  unlink(saved_path)
})
