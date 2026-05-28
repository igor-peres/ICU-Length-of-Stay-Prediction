#' Train and stack caret models for ICU LOS (LM + Ranger -> Ranger stack)
#'
#' @description
#' Fits a base ensemble (linear model and random forest) with `caretEnsemble::caretStack()` (default: `ranger`). Optionally evaluates on
#' a testing set and saves the final stacked model to disk.
#' 
#' @param train A data.frame with predictors and the outcome column.
#' @param outcome Character scalar. Name of the outcome column used for training.
#'   Default `"UnitLengthStay_trunc"`.
#' @param test Optional data.frame to evaluate on. Must contain the same
#'   predictors as `train` (at least the columns used by the final model) and
#'   the `outcome` column for metric computation.
#' @param metric Metric optimized during training (passed to caret). Default `"RMSE"`.
#' @param cv_folds Number of CV folds. Default `5`.
#' @param cv_method Resampling method for `trainControl()`. Default `"cv"`.
#' @param verbose_iter Logical; print training progress. Default `TRUE`.
#' @param seed Integer seed for reproducibility. Default `998`.
#' @param base_ranger_grid Optional `data.frame` for the base `ranger` model's
#'   grid (columns `.mtry`, `.splitrule`, `.min.node.size`). If `NULL`, uses:
#'   `.mtry = 5:10`, `.splitrule = "variance"`, `.min.node.size = 5`.
#' @param meta_method Stacking meta-learner method. Default `"ranger"`.
#' @param meta_grid Optional `data.frame` of hyperparameters for the meta-learner.
#'   If `NULL`, uses:
#'   `expand.grid(mtry = 2, min.node.size = c(5,10,15,20), splitrule = c("variance","extratrees","maxstat"))`.
#' @param save_path Optional file path to save the stacked model (`.RData` will
#'   be appended if missing). If `NULL`, the model is not saved. Default `NULL`.
#' @param drop_single_level Logical; drop factor predictors with <2 levels
#'   (computed on the full training data) before encoding. Default TRUE.
#' @param encode_categoricals Logical; if TRUE, use dummyVars(fullRank=TRUE) to
#'   one-hot encode non-numeric predictors. Default TRUE.
#' @param continue_on_fail Logical; pass through to caretEnsemble::caretList()
#'   so that failed base models don't abort the run. Default TRUE.
#' (All other parameters are unchanged.)
#'
#' @return A list with:
#' \describe{
#'   \item{stacked_model}{The fitted `caretStack` object.}
#'   \item{base_models}{The fitted `caretList` of base learners.}
#'   \item{train_control}{The `trainControl` used.}
#'   \item{metrics}{If `test` provided: data.frame with RMSE, MAE, R2.}
#'   \item{predictions}{If `test` provided: numeric vector of predictions.}
#' }
#' 
#' @examples
#' \donttest{
#' data(SampledData)
#' idx <- caret::createDataPartition(sampled_data$UnitLengthStay_trunc, p=.8, list=FALSE)
#' training  <- sampled_data[idx, ]
#' testing  <- sampled_data[-idx, ]
#'
#' fit <- slos_train_new_model(training, outcome = "UnitLengthStay_trunc", test = testing,
#'                         save_path = "slos_trained_stack.RData")
#' fit$metrics
#' 
#' unlink("slos_trained_stack.RData")
#' 
#' }
#' 
#' @export
#' @importFrom caret trainControl RMSE R2 MAE dummyVars nearZeroVar
#' @importFrom caretEnsemble caretList caretStack caretModelSpec
slos_train_new_model <- function(
    train,
    outcome = "UnitLengthStay_trunc",
    test = NULL,
    metric = "RMSE",
    cv_folds = 5,
    cv_method = "cv",
    verbose_iter = TRUE,
    seed = 998,
    base_ranger_grid = NULL,
    meta_method = "ranger",
    meta_grid = NULL,
    save_path = NULL,
    drop_single_level = TRUE,
    encode_categoricals = TRUE,
    continue_on_fail = TRUE
) {
  stopifnot(is.data.frame(train))
  if (!outcome %in% names(train)) {
    stop("Outcome column '", outcome, "' not found in 'train'.")
  }
  
  if (!is.numeric(train[[outcome]])) {
    stop("Outcome '", outcome, "' must be numeric for regression.")
  }
  
  fitControl <- caret::trainControl(
    method = cv_method,
    number = cv_folds,
    verboseIter = verbose_iter,
    returnData = FALSE,
    trim = TRUE,
    savePredictions = "final"
  )
  
  if (is.null(base_ranger_grid)) {
    base_ranger_grid <- data.frame(
      .mtry = 5:10,
      .splitrule = "variance",
      .min.node.size = 5
    )
  }
  
  if (is.null(meta_grid)) {
    meta_grid <- expand.grid(
      mtry = 2,
      min.node.size = c(5, 10, 15, 20),
      splitrule = c("variance", "extratrees", "maxstat")
    )
  }
  
  chr_cols <- names(train)[vapply(train, is.character, logical(1))]
  if (length(chr_cols)) train[chr_cols] <- lapply(train[chr_cols], factor)
  
  if (drop_single_level) {
    fac_cols <- setdiff(names(train)[vapply(train, is.factor, logical(1))], outcome)
    if (length(fac_cols)) {
      one_level <- vapply(train[fac_cols], function(f) length(levels(f)) < 2, logical(1))
      drop_cols <- fac_cols[one_level]
      if (length(drop_cols)) {
        train[drop_cols] <- NULL
      }
    }
  }
  
  y <- train[[outcome]]
  X <- train[, setdiff(names(train), outcome), drop = FALSE]
  
  dv <- NULL
  if (encode_categoricals) {
    dv <- caret::dummyVars(~ ., data = X, fullRank = TRUE, , na.action = na.pass)
    X_num <- as.data.frame(predict(dv, newdata = X))
    X_num[is.na(X_num)] <- 0
  } else {
    fac_left <- names(X)[vapply(X, is.factor, logical(1))]
    if (length(fac_left)) {
      stop("encode_categoricals=FALSE but there are factor predictors: ",
           paste(fac_left, collapse = ", "))
    }
    X_num <- X
  }
  
  if (ncol(X_num) > 0) {
    nzv_idx <- caret::nearZeroVar(X_num)
    if (length(nzv_idx)) X_num <- X_num[, -nzv_idx, drop = FALSE]
  }
  if (!ncol(X_num)) stop("No predictors remain after preprocessing.")
  
  X_test_num <- NULL; y_test <- NULL
  if (!is.null(test)) {
    stopifnot(is.data.frame(test))
    if (!outcome %in% names(test)) {
      stop("Outcome column '", outcome, "' not found in 'test'.")
    }
    chr_cols_te <- names(test)[vapply(test, is.character, logical(1))]
    if (length(chr_cols_te)) test[chr_cols_te] <- lapply(test[chr_cols_te], factor)
    
    keep_cols <- intersect(names(test), colnames(train))
    test <- test[, keep_cols, drop = FALSE]
    
    y_test <- test[[outcome]]
    X_te <- test[, setdiff(names(test), outcome), drop = FALSE]
    
    if (encode_categoricals) {
      if (is.null(dv)) stop("Internal error: dummyVars object missing.")
      for (col in names(X)) {
        if (is.factor(X[[col]])) {
          X_te[[col]] <- factor(X_te[[col]], levels = levels(X[[col]]))
        }
      }
      X_test_num <- as.data.frame(predict(dv, newdata = X_te))
    } else {
      fac_left_te <- names(X_te)[vapply(X_te, is.factor, logical(1))]
      if (length(fac_left_te)) {
        stop("encode_categoricals=FALSE but test has factor predictors: ",
             paste(fac_left_te, collapse = ", "))
      }
      X_test_num <- X_te
    }
    
    missing_cols <- setdiff(colnames(X_num), colnames(X_test_num))
    if (length(missing_cols)) {
      for (mc in missing_cols) X_test_num[[mc]] <- 0
    }
    X_test_num <- X_test_num[, colnames(X_num), drop = FALSE]
    X_test_num[is.na(X_test_num)] <- 0
  }
  
  set.seed(seed)
  model_list <- caretEnsemble::caretList(
    x = X_num,
    y = y,
    trControl = fitControl,
    metric = metric,
    tuneList = list(
      lm = caretEnsemble::caretModelSpec(method = "lm"),
      rf = caretEnsemble::caretModelSpec(
        method   = "ranger",
        tuneGrid = base_ranger_grid
      )
    ),
    continue_on_fail = continue_on_fail
  )
  
  if (!length(model_list)) stop("All base models failed to train.")
  
  set.seed(seed)
  stacked_model <- caretEnsemble::caretStack(
    model_list,
    trControl = fitControl,
    metric = metric,
    method = meta_method,
    tuneGrid = meta_grid
  )
  
  if (!is.null(save_path)) {
    if (!grepl("\\.RData$", save_path, ignore.case = TRUE)) {
      save_path <- paste0(save_path, ".RData")
    }
    save(stacked_model, file = save_path)
  }
  
  out <- list(
    stacked_model = stacked_model,
    base_models   = model_list,
    train_control = fitControl,
    dummyVars     = dv,
    feature_names = colnames(X_num)
  )
  
  if (!is.null(test)) {
    pred_raw <- stats::predict(stacked_model, newdata = X_test_num)
    
    # robust coercion
    if (is.list(pred_raw)) {
      pred <- as.numeric(unlist(pred_raw))
    } else if (is.data.frame(pred_raw)) {
      pred <- as.numeric(pred_raw[[1]])
    } else {
      pred <- as.numeric(pred_raw)
    }
    rmse <- caret::RMSE(pred, y_test)
    mae  <- caret::MAE(pred, y_test)
    r2   <- caret::R2(pred, y_test)
    out$predictions <- pred
    out$metrics <- data.frame(RMSE = rmse, MAE = mae, R2 = r2, row.names = NULL)
  }
  
  out
}
