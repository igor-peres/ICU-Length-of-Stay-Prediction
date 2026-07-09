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

# Example dataset (adjust to your real object names)
d <- read.csv("UMCdb_final.csv")

outcome <- "UnitLengthStay_trunc"

# Colunas que NUNCA devem entrar como preditoras
# (definidas logo no início, pois são usadas antes do RFE)          # <<< MUDANÇA 1
non_predictors <- c(
  "UnitLengthStay_days",   # é praticamente o próprio outcome (leakage!)
  "admissionid",           # identificador, não é preditor
  "icuid"                  # identificador da unidade (usado só no SLOS final)
)

# PRE-PROCESSING:
#' \itemize{
#'   \item Near-zero variance detection via \code{caret::nearZeroVar} (records \emph{freqRatio} and \emph{percentUnique}).
#'   \item High pairwise correlation among numeric predictors (records the max partner correlation for removed vars).
#'   \item High pairwise association among categorical predictors using Cramér's V (records max partner association).
#'   \item Splits the data into train/test with \code{caret::createDataPartition
#'   \item Performs multiple imputation using \pkg{mice}, returning completed datasets plus diagnostics.
#'   }

pp <- slos_pre_processing(
  data              = d,
  outcome           = outcome,
  remove_nzv        = TRUE,
  nzv_freq_cut      = 50,
  remove_corr_num   = TRUE,
  corr_cutoff_num   = 0.80,
  remove_corr_cat   = TRUE,
  corr_cutoff_cat   = 0.50,
  do_impute         = TRUE,
  p_train           = 0.80,
  include_outcome   = TRUE,
  mincor            = 0.10,
  m                 = 1,
  maxit             = 5,
  method            = "pmm",
  seed              = 123L,
  return_mids       = FALSE,
  return_matrices   = FALSE
)

pp$removed
pp$screening_metrics$nzv_table
pp$screening_metrics$num_removed_table
pp$screening_metrics$cat_removed_table

pp$imputation$imputed_counts$train
pp$imputation$numeric_drift$train
pp$imputation$categorical_drift$train
pp$imputation$missingness_before$test
pp$imputation$missingness_after$test

train_imp <- pp$imputation$train_completed
test_imp  <- pp$imputation$test_completed

# ANTES (BUG): faltava o "s" em "UnitLengthStay_day", então a coluna
# UnitLengthStay_days continuava no RFE e contaminava a seleção.
# df_final_featureselect <- train_imp[, !names(train_imp) %in% c("UnitLengthStay_day")]

# AGORA: remove TODAS as não-preditoras antes do RFE                # <<< MUDANÇA 2
df_final_featureselect <- train_imp[, setdiff(names(train_imp), non_predictors)]

# FEATURE SELECTION
# Supports Treebag (`treebag`) and Random Forest (`ranger`), optional normalization (via `caret::preProcess`), and k-fold cross-validation.

res <- slos_feature_select_rfe(
  data = df_final_featureselect,
  outcome = "UnitLengthStay_trunc",
  method = "treebag",
  cv_folds = 5,
  verbose = FALSE
)

print("Selected Columns:")
res$selected_vars
print("Removed Columns:")
setdiff(setdiff(names(train_imp), res$selected_vars), outcome)

# ANTES (BUG): estas linhas usavam "df", que não existe (df é uma função do R),
# e y_train seria um vetor, não um data.frame. Foram REMOVIDAS:      # <<< MUDANÇA 3
# train_idx <- as.integer(pp$split$train_idx[, 1])
# test_idx  <- as.integer(pp$split$test_idx)
# y_train <- df[[outcome]][train_idx]
# y_test  <- df[[outcome]][test_idx]

# TRAINING

# AGORA: monta os conjuntos direto dos dados imputados,             # <<< MUDANÇA 4
# usando as variáveis selecionadas pelo RFE + o outcome
keep_cols <- union(setdiff(res$selected_vars, non_predictors), outcome)

train_fs <- train_imp[, keep_cols, drop = FALSE]
test_fs  <- test_imp[,  keep_cols, drop = FALSE]

fit <- slos_train_new_model(
  train   = train_fs,
  test    = test_imp,
  outcome = "UnitLengthStay_trunc",
  seed    = 998,
  icu_column = "icuid"
)

fit$metrics

# EVALUATING EFFICIENCY

# O SLOS() precisa da coluna "icuid", então criamos uma versão       # <<< MUDANÇA 5
# do teste que mantém o icuid junto das variáveis do modelo
test_slos <- test_imp[, union(keep_cols, "icuid"), drop = FALSE]

results <- SLOS(test_slos, "icuid", "UnitLengthStay_trunc", fit)

slos_values <- results$df_unit_slos$SLOS

general_slos <- sum(results$df_unit_slos$soma_los_obs) / sum(results$df_unit_slos$soma_los_esp)
general_slos

median_slos <- median(slos_values)
median_slos

q1_slos <- quantile(slos_values, 0.25)
q3_slos <- quantile(slos_values, 0.75)
q1_slos
q3_slos
