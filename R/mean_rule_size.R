#' Compute the mean number of predictor in rules in a rule-based model
#' @inheritParams characterize
#' @keywords internal
#' @name pluck_mean_rule_size
#' @export
.pluck_mean_rule_size <- function(x, ...) {
  UseMethod(".pluck_mean_rule_size")
}

#' @rdname pluck_mean_rule_size
#' @export
.pluck_mean_rule_size.default <- function(x, ...) {
  niente
}

#' @rdname pluck_mean_rule_size
#' @export
.pluck_mean_rule_size.workflow <- function(x, ...) {
  x <- workflows::extract_fit_engine(x)
  .pluck_mean_rule_size(x, ...)
}

#' @rdname pluck_mean_rule_size
#' @export
.pluck_mean_rule_size.model_fit <- function(x, ...) {
  x <- parsnip::extract_fit_engine(x)
  .pluck_mean_rule_size(x, ...)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_mean_rule_size
#' @export
.pluck_mean_rule_size.tidy_C50 <- function(x, trials = max(x$trial), ...) {
  if (all(names(x) != "rule_num")) { #<- a tree-based model
    return(niente)
  }
  rlang::check_installed("rules")

  x <- dplyr::filter(x, trial <= trials)
  vars_used <- purrr::map_int(x$rule, ~ length(all.vars(rlang::parse_expr(.x))))
  if (length(vars_used) == 0) {
    vars_used <- 0
  }

  tibble::tibble(statistic = "mean_rule_size",
                 value = mean(vars_used, na.rm = TRUE))
}

#' @rdname pluck_mean_rule_size
#' @export
.pluck_mean_rule_size.C5.0 <- function(x, trials =  x$trials["Actual"], ...) {
  .pluck_mean_rule_size(make_tidy_c5(x), trials = trials)
}

# ------------------------------------------------------------------------------


#' @rdname pluck_mean_rule_size
#' @export
.pluck_mean_rule_size.tidy_cubist <- function(x, committees = max(x$committee), ...) {
  rlang::check_installed("rules")
  x <- dplyr::filter(x, committee <= committees)
  vars_used <- purrr::map_int(x$rule, ~ length(all.vars(rlang::parse_expr(.x))))
  if (length(vars_used) == 0) {
    vars_used <- 0
  }

  tibble::tibble(statistic = "mean_rule_size",
                 value = mean(vars_used, na.rm = TRUE))
}

#' @rdname pluck_mean_rule_size
#' @export
.pluck_mean_rule_size.cubist <- function(x, committees = x$committees, ...) {
  .pluck_mean_rule_size(make_tidy_cubist(x), committees = committees)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_mean_rule_size
#' @export
.pluck_mean_rule_size.tidy_xrf <- function(x, ...) {
  # Exclude intercept and naked predictor terms
  x <- x[grep("^r[0-9]*_", x$rule_id), ]
  vars_used <- purrr::map_int(x$rule, ~ length(all.vars(rlang::parse_expr(.x))))
  if (length(vars_used) == 0) {
    vars_used <- 0
  }
  tibble::tibble(statistic = "mean_rule_size", value = mean(vars_used, na.rm = TRUE))
}

#' @rdname pluck_mean_rule_size
#' @export
.pluck_mean_rule_size.xrf <- function(x, penalty =  0.001, ...) {
  .pluck_mean_rule_size(make_tidy_xrf(x, penalty = penalty), penalty = penalty)
}
