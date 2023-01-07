#' Compute the mean number of conditional statements in rules
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

rule_size <- function(x) {
  amp_ind <- gregexpr("&", x, fixed = TRUE)
  num_amp <- purrr::map_int(amp_ind, ~ sum(.x > 0)) + 1L
  num_amp
}

#' @rdname pluck_mean_rule_size
#' @export
.pluck_mean_rule_size.tidy_C50 <- function(x, trials = max(x$trial), ...) {
  x <- dplyr::filter(x, trial <= trials)
  rules <- rule_size(x$rule)
  tibble::tibble(statistic = "mean_rule_size",
                 value = mean(rules, na.rm = TRUE))
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
  x <- dplyr::filter(x, committee <= committees)
  rules <- rule_size(x$rule)

  tibble::tibble(statistic = "mean_rule_size",
                 value = mean(rules, na.rm = TRUE))
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
  rules <- rule_size(x$rule)
  tibble::tibble(statistic = "mean_rule_size", value = mean(rules, na.rm = TRUE))
}

#' @rdname pluck_mean_rule_size
#' @export
.pluck_mean_rule_size.xrf <- function(x, penalty =  0.001, ...) {
  .pluck_mean_rule_size(make_tidy_xrf(x, penalty = penalty), penalty = penalty)
}
