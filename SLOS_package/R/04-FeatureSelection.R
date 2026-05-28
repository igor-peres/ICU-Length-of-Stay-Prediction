# Helpers: coerce predictors & align factor levels

# Convert predictor columns to stable types:
# - character -> factor (levels fixed globally)
# - logical   -> factor(FALSE/TRUE)
# - integer   -> numeric
#' @keywords internal
#' @noRd
slos_coerce_predictors <- function(x, verbose = TRUE) {
  stopifnot(is.data.frame(x))
  
  x2 <- x
  levels_map <- list()
  type_map <- vapply(x2, function(col) class(col)[1], character(1))
  
  for (nm in names(x2)) {
    col <- x2[[nm]]
    
    if (is.character(col)) {
      # Make factor ONCE with global levels
      lvls <- sort(unique(col[!is.na(col)]))
      x2[[nm]] <- factor(col, levels = lvls)
      levels_map[[nm]] <- lvls
      type_map[[nm]] <- "factor"
      next
    }
    
    if (is.logical(col)) {
      lvls <- c(FALSE, TRUE)
      x2[[nm]] <- factor(col, levels = lvls)
      levels_map[[nm]] <- as.character(lvls)
      type_map[[nm]] <- "factor"
      next
    }
    
    if (is.integer(col)) {
      x2[[nm]] <- as.numeric(col)
      type_map[[nm]] <- "numeric"
      next
    }
    
    # If it's already a factor, record levels so we can reapply later
    if (is.factor(col)) {
      levels_map[[nm]] <- levels(col)
      type_map[[nm]] <- "factor"
      next
    }
    
    # Otherwise: numeric, Date, POSIXct, etc. keep as-is
  }
  
  if (verbose) {
    n_char <- sum(vapply(x, is.character, logical(1)))
    n_int  <- sum(vapply(x, is.integer,   logical(1)))
    n_log  <- sum(vapply(x, is.logical,   logical(1)))
    message(sprintf(
      "Coercion summary: %d character->factor | %d integer->numeric | %d logical->factor",
      n_char, n_int, n_log
    ))
  }
  
  list(x = x2, levels_map = levels_map, type_map = type_map)
}

# Align factor columns in newdata to training levels.
# Unseen levels become NA (and we warn with counts).
#' @keywords internal
#' @noRd
slos_align_factor_levels <- function(newdata, levels_map, verbose = TRUE) {
  stopifnot(is.data.frame(newdata))
  
  nd <- newdata
  
  for (nm in intersect(names(levels_map), names(nd))) {
    train_lvls <- levels_map[[nm]]
    if (is.null(train_lvls)) next
    
    col <- nd[[nm]]
    
    # Coerce characters/logicals into factor first
    if (is.character(col) || is.logical(col)) {
      col <- as.character(col)
    } else if (is.factor(col)) {
      col <- as.character(col)
    }
    
    # If col is now character, force into factor with training levels
    if (is.character(col)) {
      unseen <- setdiff(unique(col[!is.na(col)]), train_lvls)
      if (verbose && length(unseen) > 0) {
        # show counts for a few unseen levels
        tab <- sort(table(col[col %in% unseen]), decreasing = TRUE)
        top <- head(tab, 8)
        msg <- paste0(
          "Column '", nm, "': unseen levels converted to NA: ",
          paste(names(top), top, sep = "=", collapse = ", "),
          if (length(tab) > length(top)) " ..." else ""
        )
        warning(msg, call. = FALSE)
      }
      nd[[nm]] <- factor(col, levels = train_lvls)
      next
    }
    
    # If it's already factor but levels differ, re-factor with training levels
    if (is.factor(nd[[nm]])) {
      nd[[nm]] <- factor(as.character(nd[[nm]]), levels = train_lvls)
    }
  }
  
  nd
}



#' Recursive Feature Elimination (RFE) for ICU Length of Stay (LOS) Prediction
#'
#' @description
#' A wrapper around `caret::rfe()` to perform feature selection for
#' ICU Length of Stay modeling. Supports Treebag (`treebag`) and Random Forest
#' (`ranger`), optional normalization (via `caret::preProcess`), and k-fold cross-validation. 
#' Returns selected variables and artifacts so you can consistently transform new datasets.
#'
#' @param data Optional `data.frame`/`tibble` containing predictors **and** the outcome.
#' @param outcome Optional single string naming the outcome column in `data`.
#' @param x Optional predictor matrix/data.frame. If provided, `y` must be provided too.
#' @param y Optional numeric vector outcome. Required when `x` is provided.
#' @param method Character scalar: `"treebag"` (default) or `"ranger"`.
#' @param sizes Integer vector of subset sizes to evaluate during RFE.
#'   Defaults to `c(10:30, 35, 40, 45, 50, 60, 80)` for `"treebag"` and
#'   `c(10:30, 40, 60)` for `"rf"`, matching the original script.
#' @param normalize Logical; if `TRUE`, fit a `preProcess` object on predictors
#'   and use it inside RFE. Default `FALSE` for trees.
#' @param preproc_methods Character vector passed to `caret::preProcess`.
#'   Default `c("range")`.
#' @param cv_folds Integer; number of CV folds for RFE (`rfeControl(method="cv")`).
#'   Default `5`.
#' @param metric Optimization metric for RFE, usually `"RMSE"` for regression.
#'   Default `"RMSE"`.
#' @param seed Integer for reproducibility (passed to `set.seed()`).
#'   Default `420` for `"treebag"` and `100` for `"rf"`, mirroring the script.
#' @param verbose Logical; print progress. Default `TRUE`.
#' @param tune_length Integer; passed to `caret::train()` when `method = "ranger"`
#'   to control the number of `mtry` values tried. Default `5`.
#' @param num_trees Integer; number of trees for `ranger` (passed as `num.trees`).
#'   Default `500`.
#' @param importance Character; variable-importance type for `ranger`:
#'   `"none"`, `"impurity"`, or `"permutation"` (default).
#' @param respect_unordered_factors Character; how `ranger` handles unordered factors:
#'   e.g., `"order"` (default). See `ranger::ranger()`.
#' @param min_node_size Optional integer; `ranger` minimal node size
#'   (passed as `min.node.size`). If `NULL`, caret's default is used.
#' @param splitrule Optional character; `ranger` split rule (e.g., `"variance"` for regression).
#'   If `NULL`, caret's default is used.
#' @param ... Additional arguments forwarded to `caret::train()` (only relevant when
#'   `method = "ranger"`, e.g., `tuneGrid`, `maximize`, `metric`, etc.).
#'
#' @return An object of class `slos_rfe_result`, a list with:
#' \describe{
#'   \item{selected_vars}{Character vector of selected predictor names.}
#'   \item{rfe}{The `caret::rfe` result object.}
#'   \item{preproc}{The `caret::preProcess` object if normalization was used; else `NULL`.}
#'   \item{meta}{List of metadata (method, sizes, cv_folds, metric, normalize, timestamp).}
#' }
#' 
#' @examples
#' \donttest{
#' data(SampledData)
#'
#' res <- slos_feature_select_rfe(
#'   data    = sampled_data,
#'   outcome = "UnitLengthStay_trunc",
#'   method  = "treebag",
#'   normalize = TRUE
#' )
#'
#' res$selected_vars
#' }
#' @export
slos_feature_select_rfe <- function(
    data = NULL,
    outcome = NULL,
    x = NULL,
    y = NULL,
    method = c("ranger", "treebag", "rf"),
    sizes = NULL,
    normalize = FALSE,                 # trees don't need scaling; default FALSE
    preproc_methods = c("range"),
    cv_folds = 5,
    metric = "RMSE",
    seed = NULL,
    verbose = TRUE,
    # ---- ranger-specific defaults (can be overridden via ... ) ----
    tune_length = 5,                   # how many mtry values to try
    num_trees = 500,
    importance = "permutation",        # "none","impurity","permutation"
    respect_unordered_factors = "order",
    min_node_size = NULL,              # NULL = caret default
    splitrule = NULL,                  # NULL = caret default ("variance" for regression)
    ...
) {
  method <- match.arg(method)
  
  # Build x/y if needed
  if (is.null(x) || is.null(y)) {
    if (is.null(data) || is.null(outcome))
      stop("Provide either (x, y) or (data, outcome).", call. = FALSE)
    if (!is.character(outcome) || length(outcome) != 1)
      stop("`outcome` must be a single string with the outcome column name.", call. = FALSE)
    if (!outcome %in% names(data))
      stop(sprintf("`outcome` ('%s') not found in `data`.", outcome), call. = FALSE)
    
    y <- data[[outcome]]
    x <- data[, setdiff(names(data), outcome), drop = FALSE]
  }
  coerced <- slos_coerce_predictors(x, verbose = verbose)
  x <- coerced$x
  x_levels_map <- coerced$levels_map
  x_type_map   <- coerced$type_map
  
  if (!is.numeric(y))
    stop("Outcome `y` must be numeric for regression RFE (RMSE metric).", call. = FALSE)
  
  # Choose RFE function set
  rfe_funcs <- switch(
    method,
    ranger  = caret::caretFuncs,   # generic wrapper that calls train(method=...)
    treebag = caret::treebagFuncs,
    rf      = caret::rfFuncs
  )
  
  # Default subset sizes
  if (is.null(sizes)) {
    p <- ncol(x)
    sizes <- sort(unique(c(seq_len(min(30, p)), round(p * c(.25, .5, .75)), p)))
  }
  
  if (is.null(seed)) seed <- switch(method, ranger = 2025L, treebag = 420L, rf = 100L)
  
  # Optional preprocessing (generally unnecessary for trees)
  preproc_obj <- NULL
  x_proc <- x
  if (normalize) {
    if (verbose) message("Fitting preProcess: ", paste(preproc_methods, collapse = ", "))
    preproc_obj <- caret::preProcess(x, method = preproc_methods)
    x_proc <- stats::predict(preproc_obj, x)
  }
  
  # CV controls (inner train)
  tr_ctrl <- caret::trainControl(
    method = "cv",
    number = cv_folds,
    verboseIter = verbose,
    returnResamp = "final"
  )
  
  # RFE controls (outer wrapper)
  rfe_ctrl <- caret::rfeControl(
    functions    = rfe_funcs,
    method       = "cv",
    number       = cv_folds,
    returnResamp = "final",
    verbose      = verbose
  )
  
  if (!is.null(seed)) set.seed(seed)
  if (verbose) {
    message(sprintf(
      "Running RFE with method='%s', folds=%d, metric='%s'...",
      method, cv_folds, metric
    ))
  }
  
  # Assemble extra args for ranger (only when method == "ranger")
  extra_args <- list()
  if (method == "ranger") {
    extra_args <- c(
      list(
        method      = "ranger",
        trControl   = tr_ctrl,
        tuneLength  = tune_length,
        num.trees   = num_trees,
        importance  = importance,
        respect.unordered.factors = respect_unordered_factors
      ),
      # Optional tuning params if provided
      if (!is.null(min_node_size)) list(min.node.size = min_node_size) else NULL,
      if (!is.null(splitrule))     list(splitrule     = splitrule)     else NULL
    )
  }
  
  # Fit RFE (passes train args via ...)
  rfe_fit <- do.call(
    caret::rfe,
    c(
      list(
        x = x_proc,
        y = y,
        sizes = sizes,
        metric = metric,
        rfeControl = rfe_ctrl
      ),
      extra_args,
      list(...)  # allow user to pass anything else supported by caret::train for ranger
    )
  )
  
  selected <- caret::predictors(rfe_fit)
  
  res <- structure(
    list(
      selected_vars = selected,
      rfe           = rfe_fit,
      preproc       = preproc_obj,
      levels_map    = x_levels_map,
      type_map      = x_type_map,
      meta = list(
        method     = method,
        sizes      = sizes,
        cv_folds   = cv_folds,
        metric     = metric,
        seed       = seed,
        normalize  = normalize,
        preproc_methods = preproc_methods,
        ranger = list(
          tune_length = tune_length,
          num_trees   = num_trees,
          importance  = importance,
          respect_unordered_factors = respect_unordered_factors,
          min_node_size = min_node_size,
          splitrule = splitrule
        ),
        timestamp  = Sys.time()
      )
    ),
    class = "slos_rfe_result"
  )
  
  if (verbose) {
    message("RFE complete. Best subset size: ",
            rfe_fit$optsize, " | Variables: ",
            length(selected))
  }
  
  res
}


#' Apply SLOS Feature Selection to New Data
#'
#' @description
#' Given a fitted `slos_rfe_result`, apply the same pre-processing (if any) and
#' reduce `newdata` to the selected predictors in the correct order.
#'
#' @param newdata A `data.frame`/`tibble` with at least the selected columns.
#' @param fs_result An object returned by [slos_feature_select_rfe()].
#' @param strict Logical; if `TRUE`, error when selected columns are missing in
#'   `newdata`. If `FALSE`, silently drop missing and warn. Default `TRUE`.
#'
#' @return A data.frame with (optionally normalized) predictors restricted to
#'   the selected variables in the same order the RFE produced.
#'
#' @examples
#' \dontrun{
#' data(SampledData)
#' x_new <- slos_apply_feature_selection(sampled_data, res)
#' }
#' @export
slos_apply_feature_selection <- function(newdata, fs_result, strict = TRUE,fill_missing = c("none", "zero", "na"), verbose = TRUE) {
  fill_missing <- match.arg(fill_missing)
  
  if (!inherits(fs_result, "slos_rfe_result")) {
    stop("`fs_result` must be a 'slos_rfe_result' produced by slos_feature_select_rfe().", call. = FALSE)
  }
  coerced <- slos_coerce_predictors(newdata, verbose = FALSE)
  newdata2 <- coerced$x
  
  if (!is.null(fs_result$levels_map)) {
    newdata2 <- slos_align_factor_levels(newdata2, fs_result$levels_map, verbose = verbose)
  }
  
  sel <- fs_result$selected_vars
  missing_cols <- setdiff(sel, names(newdata))
  
  if (length(missing_cols) > 0) {
    msg <- sprintf(
      "Selected columns missing in `newdata`: %s",
      paste(missing_cols, collapse = ", ")
    )
    if (strict && fill_missing == "none") {
      stop(msg, call. = FALSE)
    }
    if (verbose) warning(msg, call. = FALSE)
    if (fill_missing != "none") {
      for (nm in missing_cols) {
        newdata2[[nm]] <- if (fill_missing == "zero") 0 else NA
      }
    }
  }
  x <- newdata2[, sel, drop = FALSE]
  
  if (!is.null(fs_result$preproc)) {
    x <- stats::predict(fs_result$preproc, x)
  }
  
  x
}
 