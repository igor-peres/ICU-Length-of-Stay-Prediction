#' Load the pre-trained SLOS model
#'
#' Downloads the packaged pre-trained SLOS model from its GitHub release and
#' loads it into memory. The model is fetched over HTTP and may take a few
#' minutes to download depending on connection speed.
#'
#' @return The pre-trained \code{SLOS_model} object.
#' @importFrom httr GET write_disk status_code http_error
#' @examples
#' \donttest{
#' model <- slos_load_pretrained_model()
#' }
#' @export
slos_load_pretrained_model <- function() {
  
  old <- options()
  on.exit(options(old))
  print("Please expect up to 3 minutes to download the model, depending on your internet speed.")
  options(timeout = 6000)
  url <- "https://github.com/igor-peres/ICU-Length-of-Stay-Prediction/releases/download/v2.1.0/SLOS_model.RData"
  temp_file <- tempfile(fileext = ".RData")
  response <- GET(url, write_disk(temp_file, overwrite = TRUE))
  
  env <- new.env(parent = emptyenv())
  objs <- load(temp_file, envir = env)
  
  if (!"SLOS_model" %in% objs) {
    stop("Downloaded RData does not contain 'SLOS_model'. Contains: ", paste(objs, collapse = ", "))
  }
  
  return(env$SLOS_model)
}

#' Predict using the pre-trained SLOS model
#'
#' Makes predictions with the packaged pre-trained SLOS model and evaluates
#' RMSE, MAE, and R2 against the supplied outcome column. If \code{model} is
#' not provided, it is downloaded automatically via
#' \code{\link{slos_load_pretrained_model}}.
#'
#' @param data A data frame or matrix of new data for prediction. Must contain
#'   the \code{outcome} column.
#' @param model Optional. The pre-trained SLOS model object (as returned by
#'   \code{\link{slos_load_pretrained_model}}). If \code{NULL} (the default),
#'   the packaged pretrained model is downloaded and used.
#' @param outcome The name of the outcome column in \code{data}, as a string.
#'   Defaults to \code{"UnitLengthStay_trunc"}.
#' @return A list with:
#'   \itemize{
#'     \item \code{predictions}: data.frame with one column \code{predictions}
#'     \item \code{comparison}: data.frame with columns \code{Observations} and \code{pred}
#'     \item \code{RMSE}, \code{MAE}, \code{R2}: numeric evaluation metrics
#'   }
#' @importFrom MLmetrics RMSE MAE R2_Score
#' @importFrom stats predict
#' @examples
#' \donttest{
#' data(SampledData)
#' results <- slos_predict_pretrained(sampled_data)
#' }
#' @export
slos_predict_pretrained <- function(data, model = NULL, outcome = "UnitLengthStay_trunc") {
  if (!(outcome %in% names(data))) {
    stop("Error: Outcome column '", outcome, "' not found in `data`.")
  }
  
  if (is.null(model)) {
    model <- slos_load_pretrained_model()
  }
  
  preds_obj <- predict(model, newdata = data)
  pred_vec  <- preds_obj[["pred"]]
  
  Observations <- data.frame(Observations = data[[outcome]])
  Predictions  <- data.frame(pred = as.numeric(pred_vec))
  
  RMSE_value <- MLmetrics::RMSE(Predictions$pred, Observations$Observations)
  MAE_value  <- MLmetrics::MAE(Predictions$pred, Observations$Observations)
  R2_value   <- MLmetrics::R2_Score(Predictions$pred, Observations$Observations)
  comparison <- cbind(Observations, Predictions)
  
  list(
    predictions = data.frame(predictions = as.numeric(Predictions$pred)),
    comparison  = data.frame(comparison),
    RMSE = RMSE_value, MAE = MAE_value, R2 = R2_value
  )
}
#' Predict using a custom user-trained SLOS model
#'
#' Makes predictions with a user-trained SLOS model (e.g. the object returned
#' by \code{slos_train_new_model()}), handling factor/dummy encoding and
#' feature alignment against the model's training-time feature set, then
#' evaluates RMSE, MAE, and R2 against the supplied outcome column.
#'
#' @param data A data frame or matrix of new data for prediction. Must contain
#'   the \code{outcome} column.
#' @param model A user-trained SLOS model object, expected to contain
#'   \code{dummyVars} (a \code{caret} dummy-encoding object with \code{lvls}),
#'   \code{feature_names} (the character vector of expected predictor names),
#'   and \code{stacked_model} (the fitted model used for prediction).
#' @param outcome The name of the outcome column in \code{data}, as a string.
#'   Defaults to \code{"UnitLengthStay_trunc"}.
#' @return A list with:
#'   \itemize{
#'     \item \code{predictions}: data.frame with one column \code{predictions}
#'     \item \code{comparison}: data.frame with columns \code{Observations} and \code{pred}
#'     \item \code{RMSE}, \code{MAE}, \code{R2}: numeric evaluation metrics
#'   }
#' @importFrom MLmetrics RMSE MAE R2_Score
#' @importFrom stats predict
#' @examples
#' \donttest{
#' data(SampledData)
#' fit <- slos_train_new_model(train = sampled_data, test = sampled_data,
#'                              outcome = "UnitLengthStay_trunc", icu_column = "icuid")
#' results <- slos_predict_custom(sampled_data, model = fit)
#' }
#' @export
slos_predict_custom <- function(data, model, outcome = "UnitLengthStay_trunc") {
  
  if (!(outcome %in% names(data))) {
    stop("Outcome column '", outcome, "' not found in data.")
  }
  
  # separate
  y <- data[[outcome]]
  X <- data[, setdiff(names(data), outcome), drop = FALSE]
  
  # char -> factor
  chr_cols <- names(X)[vapply(X, is.character, logical(1))]
  if (length(chr_cols)) X[chr_cols] <- lapply(X[chr_cols], factor)
  
  # align factor levels (IMPORTANT)
  for (col in names(X)) {
    if (is.factor(X[[col]]) && col %in% names(model$dummyVars$lvls)) {
      X[[col]] <- factor(X[[col]], levels = model$dummyVars$lvls[[col]])
    }
  }
  
  # dummy encoding
  X_num <- as.data.frame(predict(model$dummyVars, newdata = X))
  
  # ensure same columns
  missing_cols <- setdiff(model$feature_names, names(X_num))
  if (length(missing_cols)) {
    for (mc in missing_cols) X_num[[mc]] <- 0
  }
  
  X_num <- X_num[, model$feature_names, drop = FALSE]
  
  # handle NA
  X_num[is.na(X_num)] <- 0
  
  # predict
  pred_raw <- predict(model$stacked_model, newdata = X_num)
  
  if (is.list(pred_raw)) {
    pred <- as.numeric(unlist(pred_raw))
  } else if (is.data.frame(pred_raw)) {
    pred <- as.numeric(pred_raw[[1]])
  } else {
    pred <- as.numeric(pred_raw)
  }
  
  Observations <- data.frame(Observations = y)
  Predictions  <- data.frame(pred = pred)
  
  RMSE_value <- MLmetrics::RMSE(Predictions$pred, Observations$Observations)
  MAE_value  <- MLmetrics::MAE(Predictions$pred, Observations$Observations)
  R2_value   <- MLmetrics::R2_Score(Predictions$pred, Observations$Observations)
  
  list(
    predictions = data.frame(predictions = pred),
    comparison  = cbind(Observations, Predictions),
    RMSE = RMSE_value, MAE = MAE_value, R2 = R2_value
  )
}