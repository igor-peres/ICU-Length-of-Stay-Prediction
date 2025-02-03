#' SLOS function
#'
#' This function is the core of the SLOS package. It generates the prediction for each unit, a funnel plot for the SLOS analysis and a plot comparing observed vs predicted SLOS. To access the funnel plot, run ems::plot(result$funnel_plot).
#'
#' @param data Data frame or matrix containing testing data
#' @return Displays the funnel plot, returns the comparing plot as a ggplot object and the SLOS table.
#' @importFrom SLOS predict_and_evaluate
#' @importFrom dplyr mutate case_when bind_cols select group_by summarise ungroup n
#' @importFrom ggplot2 ggplot geom_point geom_smooth geom_abline aes labs theme_bw annotation_custom theme_void
#' @importFrom stats na.omit
#' @importFrom magrittr %>%
#' @examples
#' \donttest{
#' # Load example data
#' data(SampledData)
#' 
#' # Call the SLOS function on your data
#' result <- SLOS(sampled_data)
#' 
#' # Access the comparison plot
#' result$plot_SLOS_obv_prev
#' 
#' # Access the predictions for each unit
#' result$df_unit_slos
#' 
#' # The funnel plot will be displayed automatically, and you can access it again by calling
#' plot(result$funnel_plot)
#' }
#' 
#' @export
SLOS <- function(data) {
  # errors 
  required_columns <- c("UnitCode", "UnitLengthStay_trunc")
  missing_columns <- setdiff(required_columns, colnames(data))
  
  if (nrow(data) == 0) {
    stop("Error: Input data must contain at least one row.")
  }
  
  if (length(missing_columns) > 0) {
    stop("Error: Missing required columns: ", paste(missing_columns, collapse = ", "))
  }
  
  # SLOS function
  eval_results <- predict_and_evaluate(data)
  observations <- eval_results$comparison$Observations
  predictions <- eval_results$predictions$pred
  
  df_model_pred <- data.frame(observations = observations, predictions = predictions) %>%
    dplyr::mutate(predictions = dplyr::case_when(
      predictions < 0 ~ 0,
      predictions > 21 ~ 21,
      TRUE ~ predictions
    ))
  
  df_unit_slos <- df_model_pred %>%
    dplyr::bind_cols(dplyr::select(data, UnitCode)) %>%
    dplyr::group_by(UnitCode) %>%
    dplyr::summarise(admissoes = dplyr::n(),
                     soma_los_obs = sum(observations),
                     soma_los_esp = sum(predictions)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(SLOS = soma_los_obs / soma_los_esp) %>%
    stats::na.omit()
  
  plot_SLOS_obs_prev <- ggplot2::ggplot(df_unit_slos) +
    ggplot2::geom_point(ggplot2::aes(x = soma_los_esp, y = soma_los_obs), color = "gray40") +
    ggplot2::geom_smooth(ggplot2::aes(x = soma_los_esp, y = soma_los_obs)) +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1), linetype = "dashed") +
    ggplot2::labs(x = "Sum of predicted ICU LoS",
                  y = "Sum of observed ICU LoS",
                  title = "Grouped LoS per Unit (days)") +
    ggplot2::theme_bw()
  
  x_range <- range(df_unit_slos$admissoes, na.rm = TRUE)
  x_padding <- diff(x_range) * 0.1 
  xlim <- c(max(0, x_range[1] - x_padding), x_range[2] + x_padding)
  
  y_range <- range(df_unit_slos$SLOS, na.rm = TRUE)
  y_padding <- diff(y_range) * 0.1 
  ylim <- c(max(0, y_range[1] - y_padding), y_range[2] + y_padding)
  
  # Using ems::funnel to generate funnel plot
  funnel_plot <- ems::funnel(unit = df_unit_slos$UnitCode,
                             y = df_unit_slos$SLOS,
                             y.type = "SRU",
                             o = df_unit_slos$soma_los_obs,
                             e = df_unit_slos$soma_los_esp,
                             theta = sum(df_unit_slos$soma_los_obs) / sum(df_unit_slos$soma_los_esp),
                             n = df_unit_slos$admissoes,
                             method = "normal", option = "rate", plot = FALSE, direct = TRUE)
  
  plot(funnel_plot)

  return(list(df_unit_slos = df_unit_slos, plot_SLOS_obs_prev = plot_SLOS_obs_prev, funnel_plot = funnel_plot))
}
