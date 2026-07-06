set.seed(42)

# Example dataset (adjust to your real object names)
d <- read.csv("database.csv")

outcome <- "UnitLengthStay_trunc"

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

df_final_featureselect <- train_imp[, !names(train_imp) %in% c("UnitLengthStay_day")]

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
setdiff(setdiff(names(train_imp),res$selected_vars), outcome)

selected <- c(res$selected_vars, outcome)

train_fs <- train_imp[, selected]
test_fs  <- test_imp[, selected]

# TRAINING 

non_predictors <- c(
  "UnitLengthStay_days",
  "admissionid",
  "icuid"
)

cols_to_remove <- intersect(non_predictors, names(train_fs))

train_final <- train_fs[, setdiff(names(train_fs), cols_to_remove)]
test_final  <- test_fs[, setdiff(names(test_fs), cols_to_remove)]

fit <- slos_train_new_model(
  train = train_final,
  test = test_final,
  outcome = outcome,
  seed = 998
)

fit$metrics

# EVALUATING EFFICIENCY

results <- SLOS(test_fs, "icuid", "UnitLengthStay_trunc", fit)

slos_values <- results$df_unit_slos$SLOS

general_slos <- sum(results$df_unit_slos$soma_los_obs) / sum(results$df_unit_slos$soma_los_esp)
general_slos

median_slos <- median(slos_values)
median_slos

q1_slos <- quantile(slos_values, 0.25)
q3_slos <- quantile(slos_values, 0.75)
q1_slos
q3_slos
