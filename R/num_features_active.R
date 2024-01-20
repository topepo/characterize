#' Number of predictors used in the model
#' @inheritParams characterize
#' @export
#' @keywords internal
#' @name pluck_num_features_active
.pluck_num_features_active <- function(x, ...) {
  UseMethod(".pluck_num_features_active")
}

#' @rdname pluck_num_features_active
#' @export
.pluck_num_features_active.default <- function(x, ...) {
  vars_used <- .pluck_features_active(x, ...)
  if (identical(vars_used$statistic, character(0))) {
    res <- niente
  } else {
    res <-
      tibble::tibble(statistic = "num_features_active",
                     value = length(vars_used$value[[1]])
      )
  }
  res
}

# maybe rename to
# num_features_active
# num_features_inputs

#' @rdname pluck_num_features_active
#' @export
.pluck_num_features_active.workflow <- function(x, ...) {
  x <- workflows::extract_fit_engine(x)
  .pluck_num_features_active(x, ...)
}

#' @rdname pluck_num_features_active
#' @export
.pluck_num_features_active.model_fit <- function(x, ...) {
  x <- parsnip::extract_fit_engine(x, ...)
  .pluck_num_features_active(x, ...)
}


#' @rdname pluck_num_features_active
#' @export
.pluck_num_features_active.glmnet <- function(x, penalty = 0.001, ...) {
  vars_used <- .pluck_features_active(x, penalty = penalty)
  if (identical(vars_used$statistic, character(0))) {
    res <- niente
  } else {
    res <-
      tibble::tibble(statistic = "num_features_active",
                     value = length(vars_used$value[[1]])
      )
  }
  res
}

#' @rdname pluck_num_features_active
#' @export
.pluck_num_features_active.tidy_cubist <- function(x, committees = max(x$committee), ...) {
  vars_used <- .pluck_features_active(x, committees = committees)
  if (identical(vars_used$statistic, character(0))) {
    res <- niente
  } else {
    res <-
      tibble::tibble(statistic = "num_features_active",
                     value = length(vars_used$value[[1]])
      )
  }
  res
}

#' @rdname pluck_num_features_active
#' @export
.pluck_num_features_active.cubist <- function(x, committees = x$committees, ...) {
  .pluck_num_features_active(make_tidy_cubist(x), committees = committees)
}


#' @rdname pluck_num_features_active
#' @export
.pluck_num_features_active.tidy_C50 <- function(x, trials = max(x$trial), ...) {
  vars_used <- .pluck_features_active(x, trials = trials)
  if (identical(vars_used$statistic, character(0))) {
    res <- niente
  } else {
    res <-
      tibble::tibble(statistic = "num_features_active",
                     value = length(vars_used$value[[1]])
      )
  }
  res
}

#' @rdname pluck_num_features_active
#' @export
.pluck_num_features_active.C5.0 <- function(x, trials =  x$trials["Actual"], ...) {
  .pluck_num_features_active(make_tidy_c5(x), trials = trials)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_num_features_active
#' @export
.pluck_num_features_active.tidy_xrf <- function(x, ...) {
  vars_used <- .pluck_features_active(x)
  if (identical(vars_used$statistic, character(0))) {
    res <- niente
  } else {
    res <-
      tibble::tibble(statistic = "num_features_active",
                     value = length(vars_used$value[[1]])
      )
  }
  res
}

#' @rdname pluck_num_features_active
#' @export
.pluck_num_features_active.xrf <- function(x, penalty =  0.001, ...) {
  .pluck_num_features_active(make_tidy_xrf(x, penalty = penalty), penalty = penalty)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_num_features_active
#' @export
.pluck_num_features_active.lgb_trees <-
  function(x, trees = max(x$num_iterations), ...) {
    vars_used <- .pluck_features_active(x, trees = trees)
    if (identical(vars_used$statistic, character(0))) {
      res <- niente
    } else {
      res <-
        tibble::tibble(statistic = "num_features_active",
                       value = length(vars_used$value[[1]]))
    }
    res
  }

#' @rdname pluck_num_features_active
#' @export
.pluck_num_features_active.lgb.Booster <-
  function(x, trees = x$params$num_iterations, ...) {
    .pluck_num_features_active(lgb_trees(x), trees = trees)
  }
