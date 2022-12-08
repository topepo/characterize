#' Compute the number of estimated model parameters
#' @inheritParams characterize
#' @keywords internal
#' @export
#' @name pluck_num_parameters
.pluck_num_parameters <- function(x, ...) {
  UseMethod(".pluck_num_parameters")
}

#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.default <- function(x, ...) {
  niente
}

#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.workflow <- function(x, ...) {
  x <- workflows::extract_fit_engine(x)
  .pluck_num_parameters(x, ...)
}

#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.model_fit <- function(x, ...) {
  x <- parsnip::extract_fit_engine(x)
  .pluck_num_parameters(x, ...)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.tidy_cubist <- function(x, committees = max(x$committee), ...) {
  rlang::check_installed("rules")
  x <-
    dplyr::filter(x, committee <= committees) %>%
    dplyr::mutate(num_param = purrr::map_int(estimate, nrow))


  tibble::tibble(statistic = "num_parameters",
                 value = sum(x$num_param)
  )
}

#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.cubist <- function(x, committees = x$committees, ...) {
  .pluck_num_parameters(make_tidy_cubist(x), committees = committees)
}

# ------------------------------------------------------------------------------



#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.multinom <- function(x, ...) {
  tibble::tibble(statistic = "num_parameters",
                 value = x$n[1] * (x$n[3] - 1)
  )
}

#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.nnet <- function(x, ...) {
  tibble::tibble(statistic = "num_parameters",
                 value = length(x$wts)
  )
}

#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.glmnet <- function(x, penalty = 0.001, ...) {
  rlang::is_installed("glmnet")
  vars_used <- unlist(predict(x, s = penalty, type = "nonzero"))

  tibble::tibble(statistic = "num_parameters",
                 value = length(vars_used)
  )
}

#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.earth <- function(x, ...) {
  tibble::tibble(statistic = "num_parameters",
                 value = prod(dim(x$coefficients))
  )
}

# TODO add bagged mars


count_pls_loadings <- function(x, ...) {
  uses_y_loading <- apply(x$loadings$Y, 2, function(x) any(x != 0))
  x_loads <- x$loadings$X[, uses_y_loading]

  num_loads <-
    sum(x_loads != 0) +
    # Look for zeros in Y due to sparsity but also exclude values of 1 for cases
    # where there is a single outcome (so no real loading was estimated, 1 is a
    # placeholder)
    sum(x$loadings$Y != 0 & x$loadings$Y != 1)
  tibble::tibble(statistic = "num_parameters",
                 value = num_loads
  )
}

#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.mixo_spls <- count_pls_loadings

#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.mixo_pls <- count_pls_loadings

#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.mixo_splsda <- count_pls_loadings

#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.mixo_plsda <- count_pls_loadings


#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.xrf <- function(x, penalty = 0.001, ...) {
  rlang::check_installed("rules")

  res <- tidy(x, penalty = penalty)

  tibble::tibble(statistic = "num_parameters",
                 value = nrow(res)
  )
}

#' @rdname pluck_num_parameters
#' @export
.pluck_num_parameters.bagger <- function(x, ...) {

  if (!("MARS" %in% x$base_model)) {
    return(niente)
  }
  rlang::check_installed("earth")

  num_param <- sum(purrr::map_int(x$model_df$model, ~ length(coef(.x$fit))))
  tibble::tibble(statistic = "num_parameters",
                 value = num_param)
}
