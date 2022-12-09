#' Number of predictors used in the model
#' @inheritParams characterize
#' @export
#' @keywords internal
#' @name pluck_num_active_features
.pluck_num_active_features <- function(x, ...) {
  UseMethod(".pluck_num_active_features")
}

#' @rdname pluck_num_active_features
#' @export
.pluck_num_active_features.default <- function(x, ...) {
  vars_used <- .pluck_active_features(x, ...)
  if (identical(vars_used$statistic, character(0))) {
    res <- niente
  } else {
    res <-
      tibble::tibble(statistic = "num_active_features",
                     value = length(vars_used$value[[1]])
      )
  }
  res
}

#' @rdname pluck_num_active_features
#' @export
.pluck_num_active_features.workflow <- function(x, ...) {
  x <- workflows::extract_fit_engine(x)
  .pluck_num_active_features(x, ...)
}

#' @rdname pluck_num_active_features
#' @export
.pluck_num_active_features.model_fit <- function(x, ...) {
  x <- parsnip::extract_fit_engine(x, ...)
  .pluck_num_active_features(x, ...)
}


#' @rdname pluck_num_active_features
#' @export
.pluck_num_active_features.glmnet <- function(x, penalty = 0.001, ...) {
  vars_used <- .pluck_active_features(x, penalty = penalty)
  if (identical(vars_used$statistic, character(0))) {
    res <- niente
  } else {
    res <-
      tibble::tibble(statistic = "num_active_features",
                     value = length(vars_used$value[[1]])
      )
  }
  res
}

#' @rdname pluck_num_active_features
#' @export
.pluck_num_active_features.tidy_cubist <- function(x, committees = max(x$committee), ...) {
  vars_used <- .pluck_active_features(x, committees = committees)
  if (identical(vars_used$statistic, character(0))) {
    res <- niente
  } else {
    res <-
      tibble::tibble(statistic = "num_active_features",
                     value = length(vars_used$value[[1]])
      )
  }
  res
}

#' @rdname pluck_num_active_features
#' @export
.pluck_num_active_features.cubist <- function(x, committees = x$committees, ...) {
  .pluck_num_active_features(make_tidy_cubist(x), committees = committees)
}


#' @rdname pluck_num_active_features
#' @export
.pluck_num_active_features.tidy_C50 <- function(x, trials = max(x$trial), ...) {
  vars_used <- .pluck_active_features(x, trials = trials)
  if (identical(vars_used$statistic, character(0))) {
    res <- niente
  } else {
    res <-
      tibble::tibble(statistic = "num_active_features",
                     value = length(vars_used$value[[1]])
      )
  }
  res
}

#' @rdname pluck_num_active_features
#' @export
.pluck_num_active_features.C5.0 <- function(x, trials =  x$trials["Actual"], ...) {
  .pluck_num_active_features(make_tidy_c5(x), trials = trials)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_num_active_features
#' @export
.pluck_num_active_features.tidy_xrf <- function(x, ...) {
  vars_used <- .pluck_active_features(x)
  if (identical(vars_used$statistic, character(0))) {
    res <- niente
  } else {
    res <-
      tibble::tibble(statistic = "num_active_features",
                     value = length(vars_used$value[[1]])
      )
  }
  res
}

#' @rdname pluck_num_active_features
#' @export
.pluck_num_active_features.xrf <- function(x, penalty =  0.001, ...) {
  .pluck_num_active_features(make_tidy_xrf(x, penalty = penalty), penalty = penalty)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_num_active_features
#' @export
.pluck_num_active_features.lgb_trees <-
  function(x, trees = max(x$num_iterations), ...) {
    vars_used <- .pluck_active_features(x, trees = trees)
    if (identical(vars_used$statistic, character(0))) {
      res <- niente
    } else {
      res <-
        tibble::tibble(statistic = "num_active_features",
                       value = length(vars_used$value[[1]]))
    }
    res
  }

#' @rdname pluck_num_active_features
#' @export
.pluck_num_active_features.lgb.Booster <-
  function(x, trees = x$params$num_iterations, ...) {
    .pluck_num_active_features(lgb_trees(x), trees = trees)
  }
