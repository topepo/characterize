#' Number of predictors given to the model
#' @inheritParams characterize
#' @export
#' @keywords internal
#' @name pluck_num_features_input
.pluck_num_features_input <- function(x, ...) {
  UseMethod(".pluck_num_features_input")
}

#' @rdname pluck_num_features_input
#' @export
.pluck_num_features_input.default <- function(x, ...) {
  vars_used <- .pluck_features_input(x, ...)
  if (identical(vars_used$statistic, character(0))) {
    res <- niente
  } else {
    res <-
      tibble::tibble(statistic = "num_features_input",
                     value = length(vars_used$value[[1]]))
  }
  res
}

#' @rdname pluck_num_features_input
#' @export
.pluck_num_features_input.workflow <- function(x, ...) {
  x <- workflows::extract_fit_engine(x)
  .pluck_num_features_input(x, ...)
}

#' @rdname pluck_num_features_input
#' @export
.pluck_num_features_input.model_fit <- function(x, ...) {
  x <- parsnip::extract_fit_engine(x, ...)
  .pluck_num_features_input(x, ...)
}
