set.seed(42)

# Example dataset (adjust to your real object names)
d <- read.csv("UMCdb_final.csv")

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

train_idx <- as.integer(pp$split$train_idx[, 1])
test_idx  <- as.integer(pp$split$test_idx)

y_train <- df[[outcome]][train_idx]
y_test  <- df[[outcome]][test_idx]

# TRAINING 

non_predictors <- c(
  "UnitLengthStay_days",
  "admissionid",
  "icuid"
)

train_fs <- y_train[, setdiff(names(y_train), non_predictors)]
test_fs  <- y_test[,  setdiff(names(y_test),  non_predictors)]


fit <- slos_train_new_model(
  train   = train_fs,
  test    = test_fs,
  outcome = "UnitLengthStay_trunc",
  seed    = 998
)

fit$metrics

# EVALUATING EFFICIENCY

results <- SLOS(test_fs, "icuid", "UnitLengthStay_trunc", fit)