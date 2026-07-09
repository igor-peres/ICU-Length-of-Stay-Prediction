set.seed(42)
if (!require("caret")) {
  install.packages("caret")
  library(caret)
}

if (!require("mice")) {
  install.packages("mice")
  library(mice)
}

if (!require("caretEnsemble")) {
  install.packages("caretEnsemble")
  library(caretEnsemble)
}

if (!require("ranger")) {
  install.packages("ranger")
  library(ranger)
}

if (!require("MLmetrics")) {
  install.packages("MLmetrics")
  library(MLmetrics)
}

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("ems")) {
  install.packages("ems")
  library(ems)
}

if(!require("httr")) {
  install.packages("httr")
  library(httr)
}

# Load your already pre-processed/treated data
# (must contain the same feature set the Brazilian model was trained on, plus the ICU identifier column)
brazil_data <- load("SampledData.RData")

outcome <- "UnitLengthStay_trunc"

# Run SLOS with the existing Brazilian model
results_brazil <- SLOS(
  data       = sampled_data,
  icu_column = "UnitCode",
  outcome    = outcome,
)

# Access the comparison plot
results_brazil$plot_SLOS_obs_prev

# Funnel plot is displayed automatically; recall it with:
plot(results_brazil$funnel_plot)

# EVALUATING EFFICIENCY

slos_values <- results_brazil$df_unit_slos$SLOS

general_slos <- sum(results_brazil$df_unit_slos$soma_los_obs) /
  sum(results_brazil$df_unit_slos$soma_los_esp)
general_slos

median_slos <- median(slos_values)
median_slos

q1_slos <- quantile(slos_values, 0.25)
q3_slos <- quantile(slos_values, 0.75)
q1_slos
q3_slos
