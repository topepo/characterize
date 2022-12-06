#' Number of total support vectors used by kernel model
#' @inheritParams characterize
#' @export
#' @keywords internal
#' @name pluck_num_support_vectors
.pluck_num_support_vectors <- function(x, ...) {
  UseMethod(".pluck_num_support_vectors")
}

#' @rdname pluck_num_support_vectors
#' @export
.pluck_num_support_vectors.default <- function(x, ...) {
  niente
}

#' @rdname pluck_num_support_vectors
#' @export
.pluck_num_support_vectors.workflow <- function(x, ...) {
  x <- workflows::extract_fit_engine(x)
  .pluck_num_support_vectors(x, ...)
}

#' @rdname pluck_num_support_vectors
#' @export
.pluck_num_support_vectors.model_fit <- function(x, ...) {
  x <- parsnip::extract_fit_engine(x)
  .pluck_num_support_vectors(x, ...)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_num_support_vectors
#' @export
.pluck_num_support_vectors.ksvm <- function(x, ...) {
  svs <- unique(unlist(x@alphaindex))
  tibble::tibble(statistic = "num_support_vectors",
                 value = length(svs)
  )
}
