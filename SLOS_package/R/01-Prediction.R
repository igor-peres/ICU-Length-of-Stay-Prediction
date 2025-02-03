#' Load the SLOS model
#'
#' This function loads the pre-trained model from the package.It's available on GitHub
#'
#' @return The SLOS model
#' @importFrom httr GET write_disk status_code 
#' @importFrom utils download.file
#' @export
load_SLOSModel <- function() {
  
  old <- options()
  on.exit(options(old))
  
  options(timeout = 6000)
  url <- "https://github.com/igor-peres/ICU-Length-of-Stay-Prediction/releases/download/v2.0.0/SLOS_small.RData"
  temp_file <- tempfile(fileext = ".RData")
  download.file(url, temp_file, mode = "wb")
  load(temp_file)
  return(small_model)
}


#' Predict using the SLOS model
#'
#' This function makes predictions using the pre-trained SLOS model and evaluates it based on RMSE, MAE, and R2 values.
#'
#' @param data A data frame or matrix of new data for prediction.
#' @return A list containing the predictions made on the input data, a data frame combining the observed values and predictions side by side, and the RMSE, MAE, and R2.
#' @importFrom MLmetrics RMSE MAE R2_Score
#' @importFrom stats predict
#' @import caretEnsemble
#' @import ranger
#' @examples
#' \donttest{
#' # Load example data
#' data(SampledData)
#' 
#' # Make predictions and evaluate
#' results <- predict_and_evaluate(sampled_data)
#' 
#' # View results
#' print(results$RMSE)
#' print(results$MAE)
#' print(results$R2)
#' }
#' @export
predict_and_evaluate <- function(data) {
  small_model <- load_SLOSModel()
  
  predictions <- predict(small_model, newdata = data)
  
  Observations <- data.frame(Observations = data$UnitLengthStay_trunc)
  Predictions <- data.frame(pred = predictions)
  
  RMSE_value <- MLmetrics::RMSE(Predictions$pred, Observations$Observations)
  MAE_value <- MLmetrics::MAE(Predictions$pred, Observations$Observations)
  R2_value <- MLmetrics::R2_Score(Predictions$pred, Observations$Observations)
  comparison <- cbind(Observations, Predictions)
  
  return(list(predictions = data.frame(predictions), comparison = data.frame(comparison), 
              RMSE = RMSE_value, MAE = MAE_value, R2 = R2_value))
}
