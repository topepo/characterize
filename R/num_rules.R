#' Compute the number of rules in a rule-based model
#' @inheritParams characterize
#' @keywords internal
#' @name pluck_num_rules
#' @export
.pluck_num_rules <- function(x, ...) {
  UseMethod(".pluck_num_rules")
}

#' @rdname pluck_num_rules
#' @export
.pluck_num_rules.default <- function(x, ...) {
  niente
}

#' @rdname pluck_num_rules
#' @export
.pluck_num_rules.workflow <- function(x, ...) {
  x <- workflows::extract_fit_engine(x)
  .pluck_num_rules(x, ...)
}

#' @rdname pluck_num_rules
#' @export
.pluck_num_rules.model_fit <- function(x, ...) {
  x <- parsnip::extract_fit_engine(x)
  .pluck_num_rules(x, ...)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_num_rules
#' @export
.pluck_num_rules.tidy_C50 <- function(x, trials = max(x$trial), ...) {
  if (all(names(x) != "rule_num")) { #<- a tree-based model
    return(niente)
  }

  x <- dplyr::filter(x, trial <= trials)

  tibble::tibble(statistic = "num_rules",
                 value = nrow(x))
}

#' @rdname pluck_num_rules
#' @export
.pluck_num_rules.C5.0 <- function(x, trials =  x$trials["Actual"], ...) {
  .pluck_num_rules(make_tidy_c5(x), trials = trials)
}


#' @rdname pluck_num_rules
#' @export
.pluck_num_rules.tidy_cubist <- function(x, committees = max(x$committee), ...) {
  x <- dplyr::filter(x, committee <= committees)

  tibble::tibble(statistic = "num_rules", value = nrow(x))
}

#' @rdname pluck_num_rules
#' @export
.pluck_num_rules.cubist <- function(x, committees = x$committees, ...) {
  .pluck_num_rules(make_tidy_cubist(x), committees = committees)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_num_rules
#' @export
.pluck_num_rules.tidy_xrf <- function(x, ...) {
  num_rules <- length(grep("^r[0-9]*_", x$rule_id))
  tibble::tibble(statistic = "num_rules", value = num_rules)
}

#' @rdname pluck_num_rules
#' @export
.pluck_num_rules.xrf <- function(x, penalty =  0.001, ...) {
  .pluck_num_rules(make_tidy_xrf(x, penalty = penalty), penalty = penalty)
}
