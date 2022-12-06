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
.pluck_mean_rule_size.C5.0 <- function(x, trials = x$trials["Actual"], ...) {
  if (all(x$rules == "")) { #<- a tree-based model
    return(niente)
  }
  rlang::check_installed("rules")

  x <-
    tidy(x) %>%
    dplyr::filter(trial <= trials)

    vars_used <- purrr::map_int(x$rule, ~ length(all.vars(rlang::parse_expr(.x))))

  tibble::tibble(statistic = "mean_rule_size",
                 value = mean(vars_used, na.rm = TRUE))
}

#' @rdname pluck_mean_rule_size
#' @export
.pluck_mean_rule_size.cubist <- function(x, committees = x$committees, ...) {
  rlang::check_installed("rules")
  x <-
    tidy(x) %>%
    dplyr::filter(committee <= committees)
  vars_used <- purrr::map_int(x$rule, ~ length(all.vars(rlang::parse_expr(.x))))

  tibble::tibble(statistic = "mean_rule_size",
                 value = mean(vars_used, na.rm = TRUE))
}

#' @rdname pluck_mean_rule_size
#' @export
.pluck_mean_rule_size.xrf <- function(x, penalty = 0.001, ...) {
  rlang::check_installed("rules")
  x <- tidy(x, penalty = penalty)
  # Exclude intercept and naked predictor terms
  x <- x[grep("^r[0-9]*_", x$rule_id), ]
  vars_used <- purrr::map_int(x$rule, ~ length(all.vars(rlang::parse_expr(.x))))

  tibble::tibble(statistic = "mean_rule_size",
                 value = mean(vars_used, na.rm = TRUE)
  )
}
