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

set.seed(42)

# Load your already pre-processed/treated Dutch data
# (must contain the same feature set the Dutch model was trained on,
# plus the ICU identifier column)
dutch_data <- read.csv("UMCdb_final.csv")

outcome <- "UnitLengthStay_trunc"

# Load the existing Dutch model shipped with the package
load("UMCdb_model.RData")

# Run SLOS with the existing Dutch model
results_dutch <- SLOS(
  data       = dutch_data,
  icu_column = "icuid",
  outcome    = outcome,
  model      = UMCdb_model
)

# Access the comparison plot
results_dutch$plot_SLOS_obs_prev

# Funnel plot is displayed automatically; recall it with:
plot(results_dutch$funnel_plot)

# EVALUATING EFFICIENCY

slos_values <- results_dutch$df_unit_slos$SLOS

general_slos <- sum(results_dutch$df_unit_slos$soma_los_obs) /
  sum(results_dutch$df_unit_slos$soma_los_esp)
general_slos

median_slos <- median(slos_values)
median_slos

q1_slos <- quantile(slos_values, 0.25)
q3_slos <- quantile(slos_values, 0.75)
q1_slos
q3_slos
