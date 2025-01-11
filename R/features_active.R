#' Predictors used in the model
#'
#' `.pluck_features_active()` enumerates all of the features/predictors that
#' have contributions to the prediction equation. This excludes variables that
#' were included in the model fit but have not affect on the prediction
#' equations. For example, if a decision tree never split on a particular
#' feature, it is not included in this list.
#'
#' @details
#' Note that the term "features" corresponds to predictors produced by any
#' pre-processing operations executed by the model (if any). For example, if
#' `lm(y + x + z, data)` is used and predictor `x` is a factor, some models
#' will convert `x` to a set of binary indicator columns. In that instance, this
#' function will list the indicator columns (instead of `x`).
#'
#' @inheritParams characterize
#' @export
#' @name pluck_features_active
#' @keywords internal
.pluck_features_active <- function(x, ...) {
  UseMethod(".pluck_features_active")
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.default <- function(x, ...) {
  nolla
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.workflow <- function(x, ...) {
  x <- workflows::extract_fit_engine(x)
  .pluck_features_active(x, ...)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.model_fit <- function(x, ...) {
  x <- parsnip::extract_fit_engine(x)
  .pluck_features_active(x, ...)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_features_active
#' @export
.pluck_features_active.terms <- function(x, ...) {
  facts <- attr(x, "factors")
  resp <- attr(x, "response")
  vars_used <- rownames(facts)[-resp]
  act_vars_to_tbl(vars_used)
}

terms_wrapper <- function(x, ...) {
  .pluck_features_active(x$terms, ...)
}

has_terms <- function(x) {
  if (isS4(x)) {
    any(methods::slotNames(x) == "terms")
  } else {
    any(names(x) == "terms")
  }
}

no_int_coefs <- function(x) {
  x <- x[!grepl("(Intercept)", names(x), fixed = TRUE)]
  names(x)
}

act_vars_to_tbl <- function(x) {
  tibble::tibble(statistic = "features_active", value = list(sort(unique(x))))
}

# ------------------------------------------------------------------------------

#' @rdname pluck_features_active
#' @export
.pluck_features_active.rpart <- function(x, ...) {
  terms <- x$frame$var
  vars_used <- terms[terms != "<leaf>"]
  act_vars_to_tbl(vars_used)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_features_active
#' @export
.pluck_features_active.ranger <- function(x, ...) {
  var_index <- sort(unique(unlist(x$forest$split.varIDs)))
  vars_used <- x$forest$independent.variable.names[var_index + 1]

  act_vars_to_tbl(sort(vars_used))
}

# ------------------------------------------------------------------------------

#' @rdname pluck_features_active
#' @export
.pluck_features_active.xgb.Booster <- function(x, nrounds = x$niter, ...) {
  rlang::check_installed("xgboost")
  cl <- rlang::call2(
    "xgb.importance",
    .ns = "xgboost",
    model = expr(x),
    trees = seq.int(0, nrounds - 1)
  )
  vars_used <- rlang::eval_tidy(cl)
  act_vars_to_tbl(unique(vars_used$Feature))
}

# ------------------------------------------------------------------------------

#' @rdname pluck_features_active
#' @export
.pluck_features_active.glmnet <- function(x, penalty = 0.001, ...) {
  rlang::check_installed("glmnet")

  coefs <- predict(x, s = penalty, type = "coefficients")
  coefs <- as.matrix(coefs)
  index_used <- apply(coefs, 1, function(x) any(x != 0))
  vars_used <- names(index_used)[index_used]
  vars_used <- vars_used[vars_used != "(Intercept)"]
  act_vars_to_tbl(vars_used)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.multnet <- function(x, penalty = 0.001, ...) {
  rlang::check_installed("glmnet")

  coefs <- predict(x, s = penalty, type = "coefficients")
  coefs <- lapply(coefs, as.matrix)
  coefs <- do.call("rbind", coefs)
  index_used <- coefs[, 1] != 0
  vars_used <- rownames(coefs)[index_used]
  vars_used <- vars_used[vars_used != "(Intercept)"]
  act_vars_to_tbl(vars_used)
}

# ------------------------------------------------------------------------------

cubist_vars <- function(committees, x) {
  if (!inherits(x, "tidy_cubist")) {
    x <- make_tidy_cubist(x)
  }
  x <- dplyr::filter(x, committee <= committees) %>%
    dplyr::mutate(
      rule_vars = purrr::map(rule, get_rule_vars)
    ) %>%
    dplyr::select(rule_vars, estimate) %>%
    tidyr::unnest(cols = c(rule_vars)) %>%
    tidyr::unnest(cols = c(estimate))
  res <- sort(unique(c(x$rule_vars, x$term)))
  res <- res[res != ""]
  res <- res[res != "(Intercept)"]
  res
}

get_rule_vars <- function(x) {
  if (x == "<no conditions>") {
    res <- ""
  } else {
    x <- as.formula(paste0("~", x))
    res <- all.vars(x)
  }
  res
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.tidy_cubist <- function(
  x,
  committees = max(x$committee),
  ...
) {
  terms <- cubist_vars(committees, x)
  act_vars_to_tbl(terms)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.cubist <- function(x, committees = x$committees, ...) {
  .pluck_features_active(make_tidy_cubist(x), committees = committees)
}

# ------------------------------------------------------------------------------

c5_vars <- function(iter, x) {
  if (!inherits(x, "tidy_C50")) {
    x <- make_tidy_c5(x)
  }
  x <- x %>%
    dplyr::filter(trial <= iter) %>%
    dplyr::mutate(
      rule_vars = purrr::map(rule, get_rule_vars)
    ) %>%
    dplyr::select(rule_vars) %>%
    tidyr::unnest(cols = c(rule_vars))
  res <- sort(unique(x$rule_vars))
  res <- res[res != ""]
  res
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.tidy_C50 <- function(x, trials = max(x$trial), ...) {
  terms <- c5_vars(trials, x)
  act_vars_to_tbl(terms)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.C5.0 <- function(x, trials = x$trials["Actual"], ...) {
  .pluck_features_active(make_tidy_c5(x), trials = trials)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_features_active
#' @export
.pluck_features_active.earth <- function(x, ...) {
  rlang::check_installed("earth")
  cl <- rlang::call2("evimp", .ns = "earth", object = expr(x), trim = TRUE)
  ev <- rlang::eval_tidy(cl)
  ev <- ev[ev[, "nsubsets"] > 0, ]
  vars_used <- rownames(ev)
  vars_used <- vars_used[!grepl("-unused$", vars_used)]
  act_vars_to_tbl(vars_used)
}

# ------------------------------------------------------------------------------

pls_features <- function(x, ...) {
  uses_y_loading <- apply(x$loadings$Y, 2, function(x) any(x != 0))
  loads <- x$loadings$X[, uses_y_loading]
  non_zero_loadings <- apply(loads, 1, function(x) any(x != 0))
  vars_used <- rownames(loads)[non_zero_loadings]
  act_vars_to_tbl(vars_used)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.mixo_spls <- pls_features

#' @rdname pluck_features_active
#' @export
.pluck_features_active.mixo_pls <- pls_features

#' @rdname pluck_features_active
#' @export
.pluck_features_active.mixo_splsda <- pls_features

#' @rdname pluck_features_active
#' @export
.pluck_features_active.mixo_plsda <- pls_features

# ------------------------------------------------------------------------------

#' @rdname pluck_features_active
#' @export
.pluck_features_active.bagger <- function(x, ...) {
  vars_used <- purrr::map_dfr(x$model_df$model, .pluck_features_active)
  vars_used <- unique(unlist(vars_used$value))
  act_vars_to_tbl(vars_used)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_features_active
#' @export
.pluck_features_active.bart <- function(x, ...) {
  nms <- colnames(x$varcount)
  is_used <- apply(x$varcount, 2, function(x) any(x > 0))
  vars_used <- nms[is_used]
  act_vars_to_tbl(vars_used)
}

# ------------------------------------------------------------------------------

get_party_var_index <- function(x) {
  rlang::check_installed("partykit")
  cl <- rlang::call2("nodeids", .ns = "partykit", obj = expr(x))
  nodes <- rlang::eval_tidy(cl)
  cl <- rlang::call2(
    "nodeapply",
    .ns = "partykit",
    obj = expr(x),
    ids = expr(nodes),
    function(x) x$split$varid,
    by_node = TRUE
  )
  var_index <- rlang::eval_tidy(cl)
  var_index <- unlist(var_index)
  var_index <- unique(var_index)
  var_index
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.party <- function(x, ...) {
  var_index <- get_party_var_index(x)
  var_names <- colnames(x$data)[var_index]
  act_vars_to_tbl(var_names)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.cforest <- function(x, ...) {
  var_index <- purrr::map(x$nodes, get_party_var_index)
  var_index <- unlist(var_index)
  var_index <- unique(var_index)
  var_names <- colnames(x$data)[var_index]
  act_vars_to_tbl(var_names)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_features_active
#' @export
.pluck_features_active.tidy_xrf <- function(x, ...) {
  vars_used <- purrr::map(x$rule, ~all.vars(rlang::parse_expr(.x)))
  act_vars_to_tbl(unlist(vars_used))
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.xrf <- function(x, penalty = 0.001, ...) {
  .pluck_features_active(make_tidy_xrf(x, penalty = penalty), penalty = penalty)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_features_active
#' @export
.pluck_features_active.bagger <- function(x, ...) {
  res <- purrr::map_dfr(x$model_df$model, ~.pluck_features_active(.x$fit)) %>%
    tidyr::unnest(value)
  act_vars_to_tbl(res$value)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_features_active
#' @export
.pluck_features_active.lgb_trees <- function(x, trees = max(x$trees), ...) {
  dat <- dplyr::filter(x, trees <= !!trees)
  act_vars_to_tbl(dat$split_feature)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.lgb.Booster <- function(
  x,
  trees = x$params$num_iterations,
  ...
) {
  .pluck_features_active(lgb_trees(x), trees = trees)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_features_active
#' @export
.pluck_features_active.nullmodel <- function(x, ...) {
  act_vars_to_tbl(character(0))
}

# ------------------------------------------------------------------------------
# Models based on simple terms objects
# TODO talk about the semantics of "Feature" not input variable
# TODO some of these need to prune unused values

#' @rdname pluck_features_active
#' @export
.pluck_features_active.lm <- function(x, ...) {
  vars_used <- no_int_coefs(x$coef)
  act_vars_to_tbl(unlist(vars_used))
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.glm <- .pluck_features_active.lm

#' @rdname pluck_features_active
#' @export
.pluck_features_active.kknn <- function(x, ...) {
  .pluck_features_active(x$terms)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.stanreg <- .pluck_features_active.lm

#' @rdname pluck_features_active
#' @export
.pluck_features_active.hurdle <- function(x, ...) {
  # different terms for different components
  vars_used <- purrr::map(x$coefficients, no_int_coefs) %>%
    unlist()
  act_vars_to_tbl(vars_used)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.zeroinfl <- .pluck_features_active.hurdle

#' @rdname pluck_features_active
#' @export
.pluck_features_active.gam <- function(x, ...) {
  vars_used <- no_int_coefs(x$coefficients)
  act_vars_to_tbl(vars_used)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.lda <- function(x, ...) {
  vars_used <- colnames(x$means)
  act_vars_to_tbl(vars_used)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.fda <- function(x, ...) {
  .pluck_features_active(x$fit)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.gen.ridge <- function(x, ...) {
  vars_used <- colnames(x$xmeans)
  act_vars_to_tbl(vars_used)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.qda <- function(x, ...) {
  vars_used <- colnames(x$means)
  act_vars_to_tbl(vars_used)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.rda <- function(x, ...) {
  vars_used <- rownames(x$means)
  act_vars_to_tbl(vars_used)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.randomForest <- function(x, ...) {
  var_index <- unique(unlist(x$forest$bestvar))
  var_nms <- names(x$forest$xlevels)[var_index]
  act_vars_to_tbl(var_nms)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.lda_diag <- function(x, ...) {
  vars_used <- x$col_names
  act_vars_to_tbl(vars_used)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.qda_diag <- .pluck_features_active.lda_diag

#' @rdname pluck_features_active
#' @export
.pluck_features_active._keras.engine.sequential.Sequential <- function(x, ...) {
  .pluck_features_active(x$preproc$terms)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.LiblineaR <- function(x, ...) {
  vars_used <- colnames(x$W)
  vars_used <- vars_used[vars_used != "Bias"]
  act_vars_to_tbl(vars_used)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.mda <- function(x, ...) {
  vars_used <- names(x$fit$xmeans)
  act_vars_to_tbl(vars_used)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.naive_bayes <- function(x, ...) {
  vars_used <- names(x$tables)
  act_vars_to_tbl(vars_used)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.NaiveBayes <- .pluck_features_active.naive_bayes

#' @rdname pluck_features_active
#' @export
.pluck_features_active.brulee_multinomial_reg <- function(x, ...) {
  vars_used <- x$dims$features
  act_vars_to_tbl(vars_used)
}

#' @rdname pluck_features_active
#' @export
.pluck_features_active.brulee_logistic_reg <- .pluck_features_active.brulee_multinomial_reg

#' @rdname pluck_features_active
#' @export
.pluck_features_active.brulee_linear_reg <- .pluck_features_active.brulee_multinomial_reg

#' @rdname pluck_features_active
#' @export
.pluck_features_active.brulee_mlp <- .pluck_features_active.brulee_multinomial_reg

#' @rdname pluck_features_active
#' @export
.pluck_features_active.sda <- function(x, ...) {
  vars_used <- names(x$beta)
  act_vars_to_tbl(vars_used)
}
