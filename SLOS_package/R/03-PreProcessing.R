#' End-to-end data pre-processing for ICU LOS modeling (screening + optional imputation)
#'
#' @description
#' Performs feature screening and optional multiple imputation in one step.
#' Screening includes:
#' \itemize{
#'   \item Near-zero variance detection via \code{caret::nearZeroVar} (records \emph{freqRatio} and \emph{percentUnique}).
#'   \item High pairwise correlation among numeric predictors (records the max partner correlation for removed vars).
#'   \item High pairwise association among categorical predictors using Cramér's V (records max partner association).
#' }
#' Optionally splits the data into train/test with \code{caret::createDataPartition} and performs
#' multiple imputation using \pkg{mice}, returning completed datasets plus diagnostics.
#'
#' @param data A \code{data.frame}/tibble with predictors and the outcome.
#' @param outcome Single string with the outcome column name.
#' @param remove_nzv Logical. Remove near-zero variance predictors? Default \code{TRUE}.
#' @param nzv_freq_cut Numeric \code{freqCut} passed to \code{caret::nearZeroVar}. Default \code{50}.
#' @param remove_corr_num Logical. Remove highly correlated numeric predictors? Default \code{TRUE}.
#' @param corr_cutoff_num Numeric absolute correlation cutoff for numeric predictors. Default \code{0.9}.
#' @param remove_corr_cat Logical. Remove highly associated categorical predictors (Cramér's V)? Default \code{TRUE}.
#' @param corr_cutoff_cat Numeric cutoff for Cramér's V among categorical predictors. Default \code{0.9}.
#' @param do_impute Logical. If \code{TRUE}, perform \pkg{mice} imputation on train and test partitions. Default \code{FALSE}.
#' @param p_train Proportion for training set when splitting with \code{createDataPartition}. Default \code{0.8}.
#' @param include_outcome Logical. If \code{TRUE}, allow the outcome to be used as predictor during imputation
#'   (included in \code{quickpred}). Default \code{FALSE}.
#' @param mincor Minimum absolute correlation/association threshold used by \code{mice::quickpred} to include predictors. Default \code{0.1}.
#' @param m Number of multiple imputations (\code{mice::mice}). Default \code{5}.
#' @param maxit Maximum iterations per imputation. Default \code{5}.
#' @param method Named vector or single string for \pkg{mice} methods (e.g., \code{"pmm"}, \code{"norm"}, \code{"rf"}). Default \code{"pmm"}.
#' @param seed Integer RNG seed for reproducibility. Default \code{420}.
#' @param return_mids Logical. If \code{TRUE}, returns the \code{mids} objects for train/test. Default \code{FALSE}.
#' @param return_matrices Logical. If \code{TRUE}, returns full correlation/Cramér's V matrices (can be large). Default \code{FALSE}.
#'
#' @return A list with:
#' \describe{
#'   \item{data_screened}{The screened dataset (with outcome).}
#'   \item{removed}{List with vectors \code{$nzv}, \code{$corr_num}, \code{$corr_cat}.}
#'   \item{screening_metrics}{
#'     \itemize{
#'       \item \code{$nzv_table}: data.frame with variables removed by NZV and their \emph{freqRatio}, \emph{percentUnique}.
#'       \item \code{$num_removed_table}: data.frame with \emph{variable}, \emph{max_abs_cor_partner}, \emph{max_abs_cor}.
#'       \item \code{$cat_removed_table}: data.frame with \emph{variable}, \emph{max_cramer_partner}, \emph{max_cramerV}.
#'       \item \code{$num_cor_mat} and/or \code{$cat_cramer_mat} if \code{return_matrices = TRUE}.
#'     }
#'   }
#'   \item{split}{List with \code{$train_idx} and \code{$test_idx}.}
#'   \item{imputation}{
#'     Present only when \code{do_impute=TRUE}. Contains:
#'     \itemize{
#'       \item \code{$train_completed}, \code{$test_completed} completed data.frames.
#'       \item \code{$predictor_matrix}: the \pkg{mice} predictor matrix (importance matrix).
#'       \item \code{$methods}: named vector of imputation methods actually used by \pkg{mice}.
#'       \item \code{$missingness_before}: per-variable missing-rate table (train/test).
#'       \item \code{$pm_density}: proportion of non-zeros in the predictor matrix.
#'       \item \code{$args}: \code{m}, \code{maxit}, \code{mincor}, \code{include_outcome}, \code{seed}.
#'       \item \code{$mids} (optional): \code{train_mids}, \code{test_mids} if \code{return_mids=TRUE}.
#'     }
#'   }
#' }
#'
#' @import caret DescTools mice stats
#' @examples
#' \donttest{
#' data(SampledData)
#' pp_imp <- slos_pre_processing(
#' data = sampled_data, outcome = "UnitLengthStay",
#' do_impute = TRUE, include_outcome = FALSE,
#' m = 2, maxit = 2, method = "pmm", seed = 42
#' )
#' 
#' head(pp_imp$imputation$missingness_before$train)
#' 
#' dim(pp_imp$imputation$train_completed)
#' dim(pp_imp$imputation$test_completed)
#' 
#' colSums(is.na(pp_imp$imputation$train_completed))
#' colSums(is.na(pp_imp$imputation$test_completed))
#' 
#' pp_imp$imputation$predictor_matrix[1:6, 1:6]
#' pp_imp$imputation$pm_density
#' }
#' @export
slos_pre_processing <- function(
    data,
    outcome,
    remove_nzv = TRUE,
    nzv_freq_cut = 50,
    remove_corr_num = TRUE,
    corr_cutoff_num = 0.90,
    remove_corr_cat = TRUE,
    corr_cutoff_cat = 0.90,
    do_impute = FALSE,
    p_train = 0.8,
    include_outcome = FALSE,
    mincor = 0.10,
    m = 5,
    maxit = 5,
    method = "pmm",
    seed = 420,
    return_mids = FALSE,
    return_matrices = FALSE
) {
  stopifnot(is.data.frame(data))
  if (!outcome %in% names(data)) {
    stop("Outcome column '", outcome, "' not found in 'data'.", call. = FALSE)
  }
  
  y <- data[[outcome]]
  X <- data
  X[[outcome]] <- NULL
  
  removed_nzv   <- character(0)
  removed_corrN <- character(0)
  removed_corrC <- character(0)
  
  nzv_table <- NULL
  num_removed_table <- NULL
  cat_removed_table <- NULL
  
  if (remove_nzv && ncol(X) > 0) {
    nzv <- caret::nearZeroVar(X, saveMetrics = TRUE, freqCut = nzv_freq_cut)
    if (!is.null(nzv) && nrow(nzv) > 0) {
      nzv$Variables <- rownames(nzv)
      desc_nzv <- nzv[nzv$nzv == TRUE, c("Variables", "freqRatio", "percentUnique")]
      if (nrow(desc_nzv)) {
        removed_nzv <- desc_nzv$Variables
        nzv_table <- desc_nzv
        X <- X[, setdiff(names(X), removed_nzv), drop = FALSE]
      }
    }
  }
  
  num_cor_mat <- NULL
  if (remove_corr_num) {
    num_cols <- names(X)[vapply(X, is.numeric, logical(1))]
    if (length(num_cols) > 1) {
      num_mat <- X[, num_cols, drop = FALSE]
      cor_mat <- stats::cor(num_mat, use = "pairwise.complete.obs")
      if (all(is.finite(cor_mat))) {
        idx <- caret::findCorrelation(cor_mat, cutoff = corr_cutoff_num)
        if (length(idx)) {
          removed_corrN <- colnames(cor_mat)[idx]
          # Record max partner correlation for removed vars
          if (length(removed_corrN)) {
            rec <- lapply(removed_corrN, function(v) {
              j <- which(colnames(cor_mat) == v)
              # Exclude self
              vals <- abs(cor_mat[j, -j])
              partners <- colnames(cor_mat)[-j]
              k <- which.max(vals)
              data.frame(
                variable = v,
                max_abs_cor_partner = partners[k],
                max_abs_cor = as.numeric(vals[k]),
                stringsAsFactors = FALSE
              )
            })
            num_removed_table <- do.call(rbind, rec)
          }
          X <- X[, setdiff(names(X), removed_corrN), drop = FALSE]
        }
        if (return_matrices) num_cor_mat <- cor_mat
      }
    }
  }
  
  cat_cramer_mat <- NULL
  if (remove_corr_cat) {
    fac_cols <- names(X)[vapply(X, is.factor, logical(1))]
    logi_cols <- names(X)[vapply(X, is.logical, logical(1))]
    if (length(logi_cols)) {
      X[logi_cols] <- lapply(X[logi_cols], factor)
      fac_cols <- unique(c(fac_cols, logi_cols))
    }
    
    if (length(fac_cols) > 1) {
      fac_df <- X[, fac_cols, drop = FALSE]
      # Pairwise Cramér's V (symmetric)
      cram <- DescTools::PairApply(fac_df, DescTools::CramerV, symmetric = TRUE)
      cram[is.na(cram)] <- 0
      idx <- caret::findCorrelation(cram, cutoff = corr_cutoff_cat)
      if (length(idx)) {
        removed_corrC <- colnames(cram)[idx]
        if (length(removed_corrC)) {
          rec <- lapply(removed_corrC, function(v) {
            j <- which(colnames(cram) == v)
            vals <- cram[j, -j]
            partners <- colnames(cram)[-j]
            k <- which.max(vals)
            data.frame(
              variable = v,
              max_cramer_partner = partners[k],
              max_cramerV = as.numeric(vals[k]),
              stringsAsFactors = FALSE
            )
          })
          cat_removed_table <- do.call(rbind, rec)
        }
        X <- X[, setdiff(names(X), removed_corrC), drop = FALSE]
      }
      if (return_matrices) cat_cramer_mat <- cram
    }
  }
  
  screened <- X
  screened[[outcome]] <- y
  
  set.seed(seed)
  inTraining <- caret::createDataPartition(screened[[outcome]], p = p_train, list = FALSE)
  train <- screened[inTraining, , drop = FALSE]
  test  <- screened[-inTraining, , drop = FALSE]
  
  miss_tbl <- function(df) {
    data.frame(
      variable = names(df),
      missing_n = vapply(df, function(z) sum(is.na(z)), integer(1)),
      missing_rate = vapply(df, function(z) mean(is.na(z)), numeric(1)),
      stringsAsFactors = FALSE
    )
  }
  
  imputation <- NULL
  
  if (do_impute) {
    miss_tbl <- function(df) {
      data.frame(
        variable = names(df),
        missing_n = vapply(df, function(z) sum(is.na(z)), integer(1)),
        missing_rate = vapply(df, function(z) mean(is.na(z)), numeric(1)),
        stringsAsFactors = FALSE
      )
    }
    
    miss_tbl <- function(df) {
      data.frame(
        variable = names(df),
        missing_n = vapply(df, function(z) sum(is.na(z)), integer(1)),
        missing_rate = vapply(df, function(z) mean(is.na(z)), numeric(1)),
        stringsAsFactors = FALSE
      )
    }
    
    numeric_drift_tbl <- function(df_orig, df_comp) {
      num_cols <- names(df_comp)[vapply(df_comp, is.numeric, logical(1))]
      if (!length(num_cols)) return(NULL)
      
      obs_only <- function(x) x[!is.na(x)]
      out <- lapply(num_cols, function(v) {
        x_obs <- obs_only(df_orig[[v]])
        x_cmp <- df_comp[[v]]
        
        m_obs <- if (length(x_obs)) mean(x_obs) else NA_real_
        s_obs <- if (length(x_obs)) stats::sd(x_obs) else NA_real_
        m_cmp <- mean(x_cmp)
        s_cmp <- stats::sd(x_cmp)
        smd <- if (!is.na(s_obs) && s_obs > 0) (m_cmp - m_obs) / s_obs else NA_real_
        
        data.frame(
          variable = v,
          mean_obs = m_obs, sd_obs = s_obs,
          mean_completed = m_cmp, sd_completed = s_cmp,
          smd_vs_observed = smd,
          min_obs = if (length(x_obs)) min(x_obs) else NA_real_,
          max_obs = if (length(x_obs)) max(x_obs) else NA_real_,
          min_completed = min(x_cmp), max_completed = max(x_cmp),
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, out)
    }
    
    categorical_drift_tbl <- function(df_orig, df_comp) {
      to_fac <- function(x) {
        if (is.factor(x)) return(x)
        if (is.logical(x) || is.character(x)) return(factor(x))
        NULL
      }
      fac_names <- names(df_comp)[vapply(df_comp, function(z) {
        is.factor(z) || is.logical(z) || is.character(z)
      }, logical(1))]
      if (!length(fac_names)) return(NULL)
      
      out <- lapply(fac_names, function(v) {
        x_obs <- to_fac(df_orig[[v]])
        x_cmp <- to_fac(df_comp[[v]])
        if (is.null(x_cmp)) return(NULL)
        
        # proportions on observed (drop NAs) vs completed
        lvls <- union(levels(x_obs), levels(x_cmp))
        x_obs <- factor(x_obs, levels = lvls)
        x_cmp <- factor(x_cmp, levels = lvls)
        
        p_obs <- if (length(na.omit(x_obs))) prop.table(table(na.omit(x_obs))) else rep(NA_real_, length(lvls))
        p_cmp <- prop.table(table(x_cmp))
        
        p_obs <- as.numeric(p_obs[lvls]); p_cmp <- as.numeric(p_cmp[lvls])
        
        tvd <- if (all(!is.na(p_obs))) sum(abs(p_cmp - p_obs)) / 2 else NA_real_
        
        deltas <- if (all(!is.na(p_obs))) abs(p_cmp - p_obs) else rep(NA_real_, length(lvls))
        k <- if (all(!is.na(deltas))) which.max(deltas) else NA_integer_
        
        data.frame(
          variable = v,
          n_levels = length(lvls),
          tvd_proportions = tvd,
          most_changed_level = if (!is.na(k)) lvls[k] else NA_character_,
          delta_prop_max = if (!is.na(k)) deltas[k] else NA_real_,
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, Filter(Negate(is.null), out))
    }
    
    do_impute_once <- function(df) {
      include <- if (include_outcome) outcome else NULL
      predM <- mice::quickpred(
        df,
        include = include,
        exclude = NULL,
        mincor = mincor
      )
      
      set.seed(seed)
      mids <- mice::mice(
        data = df,
        predictorMatrix = predM,
        m = m,
        maxit = maxit,
        method = method,
        diagnostics = TRUE,
        printFlag = FALSE
      )
      comp <- mice::complete(mids, 1)
      
      miss_before <- miss_tbl(df)
      miss_after  <- miss_tbl(comp)
      
      where_mat <- mids$where # TRUE quando foi imputado
      imp_counts <- data.frame(
        variable = colnames(where_mat),
        imputed_n = colSums(where_mat),
        imputed_rate = colMeans(where_mat),
        stringsAsFactors = FALSE
      )
      
      num_drift <- numeric_drift_tbl(df, comp)
      cat_drift <- categorical_drift_tbl(df, comp)
      
      list(
        comp = comp,
        mids = mids,
        predM = predM,
        miss_before = miss_before,
        miss_after  = miss_after,
        imp_counts  = imp_counts,
        num_drift   = num_drift,
        cat_drift   = cat_drift
      )
    }
    
    imp_tr <- do_impute_once(train)
    imp_te <- do_impute_once(test)
    
    pm <- imp_tr$predM
    pm_density <- if (length(pm)) mean(pm != 0) else NA_real_
    
    imputation <- list(
      train_completed    = imp_tr$comp,
      test_completed     = imp_te$comp,
      predictor_matrix   = imp_tr$predM,
      methods            = if (!is.null(imp_tr$mids$method)) imp_tr$mids$method else NULL,
      missingness_before = list(train = imp_tr$miss_before, test = imp_te$miss_before),
      missingness_after  = list(train = imp_tr$miss_after,  test = imp_te$miss_after),
      imputed_counts     = list(train = imp_tr$imp_counts,  test = imp_te$imp_counts),
      numeric_drift      = list(train = imp_tr$num_drift,   test = imp_te$num_drift),
      categorical_drift  = list(train = imp_tr$cat_drift,   test = imp_te$cat_drift),
      chains             = list(
        train = list(chainMean = imp_tr$mids$chainMean, chainVar = imp_tr$mids$chainVar),
        test  = list(chainMean = imp_te$mids$chainMean, chainVar = imp_te$mids$chainVar)
      ),
      pm_density         = pm_density,
      args = list(
        m = m, maxit = maxit, mincor = mincor,
        include_outcome = include_outcome, seed = seed
      )
    )
    
    if (return_mids) {
      imputation$mids <- list(
        train_mids = imp_tr$mids,
        test_mids  = imp_te$mids
      )
    }
  }
  
  list(
    data_screened = screened,
    removed = list(
      nzv      = removed_nzv,
      corr_num = removed_corrN,
      corr_cat = removed_corrC
    ),
    screening_metrics = list(
      nzv_table         = nzv_table,
      num_removed_table = num_removed_table,
      cat_removed_table = cat_removed_table,
      num_cor_mat       = if (return_matrices) num_cor_mat else NULL,
      cat_cramer_mat    = if (return_matrices) cat_cramer_mat else NULL
    ),
    split = list(
      train_idx = inTraining,
      test_idx  = setdiff(seq_len(nrow(screened)), inTraining)
    ),
    imputation = imputation
  )
}
