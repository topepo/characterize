#' Model characterizations across multiple configurations
#'
#' @description
#' Some models can make predictions for multiple configurations using the same
#' model object. For example, many boosted tree models using `m` trees can also
#' make predictions when fewer than `m` trees were used. These are sometimes
#' called "sub-model parameters".
#'
#' For such a model, `multi_characterize()` can produce multiple results from
#' the same object. If the model does not allow for this, the results of
#' [characterize()] are returned.
#'
#' @details
#' For `glmnet` models, there are methods for the different classes of that model
#' (a.k.a. `lognet` class for Poisson models) as well as a variation of that
#' class (`_lognet`). The latter case corresponds to the object produced by the
#' \pkg{parsnip} package for that model.
#'
#' @inheritParams characterize
#' @param object A `model_fit` object.
#' @param ... Optional arguments (not currently used)
#' @param penalty A numeric vector of penalty values (a.k.a. `lambda` for some
#' models).
#' @return A tibble of characterized results (`results`) with extra columns for the
#' sub-model parameter. If there is no specific `multi_characterize()` method, there
#' are not parameter columns and the `results` column has the results of
#' using a basic [characterize()] call.
#' @seealso [characterize()]
#' @examplesIf characterize:::is_rcmd_check()
#' if (rlang::is_installed(c("modeldata", "ggplot2", "Cubist"))) {
#'   library(Cubist)
#'   library(dplyr)
#'   library(ggplot2)
#'   library(tidyr)
#'
#'   # ----------------------------------------------------------------------------
#'
#'   data(ames, package = "modeldata")
#'   ames$Sale_Price <- log10(ames$Sale_Price)
#'
#'   cubist_fit <-
#'     cubist(x = ames %>% select(-Sale_Price), y = ames$Sale_Price, committees = 5)
#'
#'   # ----------------------------------------------------------------------------
#'   # Get results for a specific model configuration
#'
#'   characterize(cubist_fit, committees = 3)
#'
#'   # ----------------------------------------------------------------------------
#'   # Get results for many model configurations
#'
#'   res <- multi_characterize(cubist_fit, committees = 1:5)
#'   res
#'
#'   # unnest to use them:
#'   res %>%
#'     unnest(cols = results) %>%
#'     ggplot(aes(committees, .estimate)) +
#'     geom_point() +
#'     facet_wrap(~ .metric, scale = "free_y")
#' }
#' @export
multi_characterize <- function(object, ...) {
  if (inherits(object, "try-error")) {
    rlang::warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }
  UseMethod("multi_characterize")
}

#' @rdname multi_characterize
#' @export
multi_characterize.default <- function(object, ...) {
  res <- characterize(object)
  tibble::tibble(results = list(res))
}


#' @rdname multi_characterize
#' @export
multi_characterize.workflow <- function(object, ...) {
  x <- workflows::extract_fit_parsnip(object)
  multi_characterize(x, ...)
}

#' @rdname multi_characterize
#' @export
multi_characterize.model_fit <- function(object, ...) {
  if (inherits(object$fit, "try-error")) {
    rlang::warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }
  multi_characterize(object$fit, ...)
}

# ------------------------------------------------------------------------------
# Specific methods. These should all be able to produce self-contained results.
# This means that they _can_ take their parameter arguments but should be able
# to be used without that argument (based on the original invocation).


#' @rdname multi_characterize
#' @export
multi_characterize._elnet <- function(object, penalty = NULL, ...) {
  if (is.null(penalty)) {
    penalty <- object$fit$lambda
  }

  res <-
    tibble::tibble(penalty = penalty) %>%
    dplyr::mutate(results = purrr::map(penalty, ~ characterize(object, penalty = .x)))
  res
}

#' @rdname multi_characterize
#' @export
multi_characterize.glmnet <- function(object, penalty = NULL, ...) {
  rlang::check_installed("glmnet")

  if (is.null(penalty)) {
    penalty <- object$lambda
  }

  res <-
    tibble::tibble(penalty = penalty) %>%
    dplyr::mutate(results = purrr::map(penalty, ~ characterize(object, penalty = .x)))
  res
}

#' @rdname multi_characterize
#' @export
multi_characterize._multnet <- multi_characterize._elnet
#' @rdname multi_characterize
#' @export
multi_characterize.multnet <- multi_characterize.glmnet


#' @rdname multi_characterize
#' @export
multi_characterize._lognet <- multi_characterize._elnet
#' @rdname multi_characterize
#' @export
multi_characterize.lognet <- multi_characterize.glmnet


#' @rdname multi_characterize
#' @export
multi_characterize._fishnet <- multi_characterize._elnet
#' @rdname multi_characterize
#' @export
multi_characterize.fishnet <- multi_characterize.glmnet

#' @rdname multi_characterize
#' @export
multi_characterize.cubist <- function(object, committees = object$committees, ...) {
  # TODO tidy once and then subset across committees; may require re-writes
  res <-
    tibble::tibble(committees = committees) %>%
    dplyr::mutate(results = purrr::map(committees, ~ characterize(object, committees = .x)))
  res
}

# ------------------------------------------------------------------------------

# TODO do this for tidy_{x} as well (as done with lgb)
#' @rdname multi_characterize
#' @export
multi_characterize.C5.0 <- function(object, trials = object$trials["Actual"], ...) {
  # TODO tidy once and then subset across committees; may require re-writes
  res <-
    tibble::tibble(trials = trials) %>%
    dplyr::mutate(results = purrr::map(trials, ~ characterize(object, trials = .x)))
  res
}

# ------------------------------------------------------------------------------

#' @rdname multi_characterize
#' @export
multi_characterize.xgb.Booster <- function(object, nrounds = object$niter, ...) {
  res <-
    tibble::tibble(nrounds = nrounds) %>%
    dplyr::mutate(results = purrr::map(nrounds, ~ characterize(object, nrounds = .x)))
  res
}

# ------------------------------------------------------------------------------


# TODO earth

#' @rdname multi_characterize
#' @export
multi_characterize.xrf <- function(object, penalty = NULL, ...) {
  if (is.null(penalty)) {
    penalty <- object$fit$glm$model$lambda
  }

  res <-
    tibble::tibble(penalty = penalty) %>%
    dplyr::mutate(results = purrr::map(penalty, ~ characterize(object, penalty = .x)))
  res
}

# ------------------------------------------------------------------------------

#' @rdname multi_characterize
#' @export
multi_characterize.lgb_trees <- function(object, trees = NULL, ...) {
  if (is.null(trees)) {
    trees <- max(x$trees)
  }
  res <-
    tibble::tibble(trees = trees) %>%
    dplyr::mutate(results = purrr::map(trees, ~ characterize(object, trees = .x)))
  res
}


#' @rdname multi_characterize
#' @export
multi_characterize.lgb.Booster <- function(object, trees = NULL, ...) {
  if (is.null(trees)) {
    trees <- object$params$num_iterations
  }
  multi_characterize(lgb_trees(object), trees = trees)
}

