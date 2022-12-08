#' Predictors used in the model
#' @inheritParams characterize
#' @export
#' @name pluck_active_features
#' @keywords internal
.pluck_active_features <- function(x, ...) {
  UseMethod(".pluck_active_features")
}

#' @rdname pluck_active_features
#' @export
.pluck_active_features.default <- function(x, ...) {
  nolla
}

#' @rdname pluck_active_features
#' @export
.pluck_active_features.workflow <- function(x, ...) {
  x <- workflows::extract_fit_engine(x)
  .pluck_active_features(x, ...)
}

#' @rdname pluck_active_features
#' @export
.pluck_active_features.model_fit <- function(x, ...) {
  x <- parsnip::extract_fit_engine(x)
  .pluck_active_features(x, ...)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_active_features
#' @export
.pluck_active_features.terms <- function(x, ...) {
  facts <- attr(x, "factors")
  resp <- attr(x, "response")
  vars_used <- rownames(facts)[-resp]
  tibble::tibble(statistic = "active_features",
                 value = list(sort(unique(vars_used)))
  )
}

terms_wrapper <- function(x, ...) {
  .pluck_active_features(x$terms, ...)
}

has_terms <- function(x) {
  if (isS4(x)) {
    any(methods:slotNames(x) == "terms")
  } else {
    any(names(x) == "terms")
  }
}

# ------------------------------------------------------------------------------

#' @rdname pluck_active_features
#' @export
.pluck_active_features.rpart <- function(x, ...) {
  terms <- x$frame$var
  vars_used <- terms[terms != "<leaf>"]
  tibble::tibble(statistic = "active_features",
                 value = list(sort(unique(vars_used)))
  )
}

# ------------------------------------------------------------------------------

#' @rdname pluck_active_features
#' @export
.pluck_active_features.ranger <- function(x, ...) {
  var_index <- sort(unique(unlist(x$forest$split.varIDs)))
  var_index <- var_index[var_index > 0]
  vars_used <- x$forest$independent.variable.names[var_index]

  tibble::tibble(statistic = "active_features",
                 value = list(sort(unique(vars_used)))
  )
}

# ------------------------------------------------------------------------------

#' @rdname pluck_active_features
#' @export
.pluck_active_features.xgb.Booster <- function(x, nrounds = x$niter, ...) {
  rlang::check_installed("xgboost")
  cl <- rlang::call2("xgb.importance", .ns = "xgboost", model = expr(x),
                     trees = seq.int(0, nrounds - 1))
  vars_used <- rlang::eval_tidy(cl)

  tibble::tibble(statistic = "active_features",
                 value = list(sort(unique(vars_used$Feature)))
  )
}

# ------------------------------------------------------------------------------

#' @rdname pluck_active_features
#' @export
.pluck_active_features.glmnet <- function(x, penalty = 0.001, ...) {
  rlang::is_installed("glmnet")

  index_used <- unlist(predict(x, s = penalty, type = "nonzero"))
  if (inherits(x, "multnet")) {
    nms <- rownames(x$beta[[1]])
  } else {
    nms <- rownames(x$beta)
  }

  vars_used <- nms[index_used]
  tibble::tibble(statistic = "active_features",
                 value = list(sort(unique(vars_used)))
  )
}

# ------------------------------------------------------------------------------

cubist_vars <- function(committees, x) {
  rlang::check_installed("rules")
  if (!inherits(x, "tidy_cubist")) {
    x <- make_tidy_cubist(x)
  }
  x <-
    dplyr::filter(x, committee <= committees) %>%
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

#' @rdname pluck_active_features
#' @export
.pluck_active_features.tidy_cubist <- function(x, committees = max(x$committee), ...) {
  terms <- cubist_vars(committees, x)
  tibble::tibble(statistic = "active_features",
                 value = list(terms)
  )
}

#' @rdname pluck_active_features
#' @export
.pluck_active_features.cubist <- function(x, committees = x$committees, ...) {
  .pluck_active_features(make_tidy_cubist(x), committees = committees)
}

# ------------------------------------------------------------------------------


c5_vars <- function(iter, x) {
  rlang::check_installed("rules")
  if (!inherits(x, "tidy_C50")) {
    x <- make_tidy_c5(x)
  }
  x <-
    x %>%
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

#' @rdname pluck_active_features
#' @export
.pluck_active_features.tidy_C50 <- function(x, trials = max(x$trial), ...) {
  terms <- c5_vars(trials, x)
  tibble::tibble(statistic = "active_features",
                 value = list(terms)
  )
}

#' @rdname pluck_active_features
#' @export
.pluck_active_features.C5.0 <- function(x, trials =  x$trials["Actual"], ...) {
  .pluck_active_features(make_tidy_c5(x), trials = trials)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_active_features
#' @export
.pluck_active_features.earth <- function(x, ...) {
  rlang::check_installed("earth")
  cl <- rlang::call2("evimp", .ns = "earth", object = expr(x), trim = TRUE)
  ev <- rlang::eval_tidy(cl)
  ev <- ev[ ev[,"nsubsets"] > 0, ]
  vars_used <- rownames(ev)
  tibble::tibble(statistic = "active_features",
                 value = list(sort(unique(vars_used)))
  )
}

# ------------------------------------------------------------------------------

pls_features <- function(x, ...) {
  uses_y_loading <- apply(x$loadings$Y, 2, function(x) any(x != 0))
  loads <- x$loadings$X[, uses_y_loading]
  non_zero_loadings <- apply(loads, 1, function(x) any(x != 0))
  vars_used <- rownames(loads)[non_zero_loadings]
  tibble::tibble(statistic = "active_features",
                 value = list(sort(unique(vars_used)))
  )
}

#' @rdname pluck_active_features
#' @export
.pluck_active_features.mixo_spls <- pls_features

#' @rdname pluck_active_features
#' @export
.pluck_active_features.mixo_pls <- pls_features

#' @rdname pluck_active_features
#' @export
.pluck_active_features.mixo_splsda <- pls_features

#' @rdname pluck_active_features
#' @export
.pluck_active_features.mixo_plsda <- pls_features

# ------------------------------------------------------------------------------

#' @rdname pluck_active_features
#' @export
.pluck_active_features.bagger <- function(x, ...) {
  vars_used <- purrr::map_dfr(x$model_df$model, .pluck_active_features)
  vars_used <- unique(unlist(vars_used$value))
  tibble::tibble(statistic = "active_features",
                 value = list(sort(unique(vars_used)))
  )
}

# ------------------------------------------------------------------------------

#' @rdname pluck_active_features
#' @export
.pluck_active_features.bart <- function(x, ...) {
  nms <- colnames(x$varcount)
  is_used <- apply(x$varcount, 2, function(x) any(x > 0))
  vars_used <- nms[is_used]
  tibble::tibble(statistic = "active_features",
                 value = list(sort(unique(vars_used)))
  )
}

# ------------------------------------------------------------------------------

get_party_var_index <- function(x) {
  rlang::check_installed("partykit")
  cl <- rlang::call2("nodeids", .ns = "partykit", obj = expr(x))
  nodes <- rlang::eval_tidy(cl)
  cl <-
    rlang::call2(
      "nodeapply", .ns = "partykit",
      obj = expr(x), ids = expr(nodes),
      function(x) x$split$varid, by_node = TRUE
    )
  var_index <- rlang::eval_tidy(cl)
  var_index <- unlist(var_index)
  var_index <- unique(var_index)
  var_index
}

#' @rdname pluck_active_features
#' @export
.pluck_active_features.party <- function(x, ...) {
  var_index <- get_party_var_index(x)
  var_names <- colnames(x$data)[var_index]
  tibble::tibble(statistic = "active_features",
                 value = list(sort(var_names))
  )
}

#' @rdname pluck_active_features
#' @export
.pluck_active_features.cforest <- function(x, ...) {
  var_index <- purrr::map(x$nodes, get_party_var_index)
  var_index <- unlist(var_index)
  var_index <- unique(var_index)
  var_names <- colnames(x$data)[var_index]
  tibble::tibble(statistic = "active_features",
                 value = list(sort(var_names))
  )
}


# ------------------------------------------------------------------------------

#' @rdname pluck_active_features
#' @export
.pluck_active_features.tidy_xrf <- function(x, ...) {
  vars_used <- purrr::map(x$rule, ~ all.vars(rlang::parse_expr(.x)))

  tibble::tibble(statistic = "active_features",
                 value = list(sort(unique(unlist(vars_used))))
  )
}

#' @rdname pluck_active_features
#' @export
.pluck_active_features.xrf <- function(x, penalty = 0.001, ...) {
  .pluck_active_features(make_tidy_xrf(x, penalty = penalty), penalty = penalty)
}


# ------------------------------------------------------------------------------

#' @rdname pluck_active_features
#' @export
.pluck_active_features.bagger <- function(x, ...) {
  res <-
    purrr::map_dfr(x$model_df$model, ~ .pluck_active_features(.x$fit)) %>%
    tidyr::unnest(value)
  tibble::tibble(statistic = "active_features",
                 value = list(sort(unique(res$value))))
}


