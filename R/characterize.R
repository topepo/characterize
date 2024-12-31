#' Compute model characteristics
#'
#' @description
#' `characterize()` attempts to report data driven characteristics of a model fit
#' that describe the model. For example, the number of predictors used, the
#' number of terminal nodes, etc. These are interesting characteristics of the
#' model that often reflect complexity of the model.
#'
#' @param x An object.
#' @param penalty A numeric parameter value for the amount of
#' regularization/penalty of the model.
#' @param committees The number of Cubist committees to use in the characterization.
#' @param trials,nrounds,trees The number of boosting to use in the characterization.
#' @param num_terms The number of retained terms in the model.
#' @param ... Not currently used.
#' @details
#' For a list of object types supported, see [list_characteristics()].
#'
#' @return A tibble with columns `.metric`, `.estimator`, and `.estimate`. If
#' there are no statistics to compute, a zero-row tibble is returned. Integer
#' values are converted to double.
#' @seealso [multi_characterize()]
#' @examples
#'
#' library(rpart)
#' cart_mod <- rpart(mpg ~ ., data = mtcars)
#' characterize(cart_mod)
#'
#' # When there is nothing to report:
#' ppr_mod <- ppr(mpg ~ ., data = mtcars, nterms = 5)
#' characterize(ppr_mod)
#' @export
characterize <- function(x, ...) {
  UseMethod("characterize")
}

#' @rdname characterize
#' @export
characterize.default <- function(x, ...) {
  # Update as new methods are added
  dplyr::bind_rows(
    # This is an exhaustive list of the attributes that are calculated from
    # model objects. Many of these are inappropriate for any given model and,
    # in these cases, an empty tibble is returned. There are specific characterize
    # methods below and those have a reduced set of pluck calls.
    .pluck_num_features_input(x, ...),
    .pluck_num_features_active(x, ...),
    .pluck_num_parameters(x, ...),
    .pluck_num_rules(x, ...),
    .pluck_num_support_vectors(x, ...),
    .pluck_num_term_nodes(x, ...),
    .pluck_mean_rule_size(x, ...)
  ) %>%
    yardstick_like()
}

#' @rdname characterize
#' @export
characterize.model_fit <- function(x, ...) {
  characterize(parsnip::extract_fit_engine(x), ...)
}

#' @rdname characterize
#' @export
characterize.workflow <- function(x, ...) {
  characterize(workflows::extract_fit_engine(x), ...)
}

# ------------------------------------------------------------------------------
# Specific methods needed (usually due to needing sub-model parameters)

# TODO add specific glmnet methods as well as _glmnet methods to use the default

#' @rdname characterize
#' @export
characterize.glmnet <- function(x, penalty = 0.001, ...) {
  dplyr::bind_rows(
    .pluck_num_features_input(x, ...),
    .pluck_num_features_active(x, penalty = penalty),
    .pluck_num_parameters(x, penalty = penalty)
  ) %>%
    yardstick_like()
}

# ------------------------------------------------------------------------------

# To avoid re-running the tidy method many times
make_tidy_cubist <- function(x, ...) {
  rlang::check_installed("rules")

  res <- tidy(x, ...)
  class(res) <- c("tidy_cubist", class(res))
  res
}

#' @rdname characterize
#' @export
characterize.cubist <- function(x, committees = NULL, ...) {
  if (is.null(committees)) {
    committees <- x$committees
  }
  x <- make_tidy_cubist(x)
  dplyr::bind_rows(
    .pluck_num_features_input(x, ...),
    .pluck_num_features_active(x, committees = committees),
    .pluck_num_parameters(x, committees = committees),
    .pluck_num_rules(x, committees = committees),
    .pluck_mean_rule_size(x, committees = committees)
  ) %>%
    yardstick_like()
}

#' @rdname characterize
#' @export
characterize.tidy_cubist <- function(x, committees = NULL, ...) {
  if (is.null(committees)) {
    committees <- max(x$committee)
  }
  x <- dplyr::filter(x, committee <= !!committees)

  dplyr::bind_rows(
    .pluck_num_features_input(x, ...),
    .pluck_num_features_active(x, committees = committees),
    .pluck_num_parameters(x, committees = committees),
    .pluck_num_rules(x, committees = committees),
    .pluck_mean_rule_size(x, committees = committees)
  ) %>%
    yardstick_like()
}

#' @rdname characterize
#' @export
characterize.cubist <- function(x, committees = NULL, ...) {
  if (is.null(committees)) {
    committees <- x$committees
  }
  characterize(make_tidy_cubist(x), committees = committees)
}

# ------------------------------------------------------------------------------

# To avoid re-running the tidy method many times
make_tidy_c5 <- function(x, ...) {
  rlang::check_installed("rules")

  res <- tidy(x, ...)
  class(res) <- c("tidy_C50", class(res))
  res
}

# ------------------------------------------------------------------------------

#' @rdname characterize
#' @export
characterize.tidy_C50 <- function(x, trials = max(x$trials), ...) {
  x <- dplyr::filter(x, trial <= !!trials)
  dplyr::bind_rows(
    .pluck_num_features_input(x, ...),
    .pluck_num_features_active(x, trials = trials),
    .pluck_num_rules(x, trials = trials),
    .pluck_mean_rule_size(x, trials = trials),
    .pluck_num_term_nodes(x, trials = trials)
  ) %>%
    yardstick_like()
}

#' @rdname characterize
#' @export
characterize.C5.0 <- function(x, trials =  x$trials["Actual"], ...) {
  characterize(make_tidy_c5(x), trials = trials)
}

# ------------------------------------------------------------------------------


#' @rdname characterize
#' @export
characterize.xgb.Booster <- function(x, nrounds = x$niter, ...) {
  dplyr::bind_rows(
    .pluck_num_features_input(x, ...),
    .pluck_num_features_active(x, nrounds = nrounds),
    .pluck_num_term_nodes(x, nrounds = nrounds)
  ) %>%
    yardstick_like()
}

# ------------------------------------------------------------------------------

# To avoid re-running the tidy method many times
make_tidy_xrf <- function(x, penalty = 0.001, ...) {
  rlang::check_installed("rules")

  res <- tidy(x, penalty = penalty, ...)
  class(res) <- c("tidy_xrf", class(res))
  res
}

#' @rdname characterize
#' @export
characterize.tidy_xrf <- function(x, penalty = 0.001, ...) {
  dplyr::bind_rows(
    .pluck_num_features_input(x, ...),
    .pluck_num_features_active(x, penalty = penalty),
    .pluck_num_rules(x, penalty = penalty),
    .pluck_mean_rule_size(x, penalty = penalty)
  ) %>%
    yardstick_like()
}

#' @rdname characterize
#' @export
characterize.xrf <- function(x, penalty = 0.001, ...) {
  characterize(make_tidy_xrf(x, penalty = penalty))
}

# ------------------------------------------------------------------------------

# This is expensive so do it as few times as possible. Give the results a class
# and S3 dispatch on that.
lgb_trees <- function(x) {
  rlang::is_installed("lightgbm")
  cl <- rlang::call2("lgb.model.dt.tree", .ns = "lightgbm", model = rlang::expr(x))
  dat <- rlang::eval_tidy(cl)
  # error trap for saved model
  res <-
    tibble::as_tibble(dat) %>%
    dplyr::mutate(trees = tree_index + 1)
  class(res) <- c("lgb_trees", class(res))
  res
}


#' @rdname characterize
#' @export
characterize.lgb.Booster <- function(x, trees = NULL, ...) {
  if (is.null(trees)) {
    trees <- x$params$num_iterations
  }
  x <- lgb_trees(x)
  dplyr::bind_rows(
    .pluck_num_features_input(x, ...),
    .pluck_num_features_active(x, trees = trees),
    .pluck_num_term_nodes(x, trees = trees)
  ) %>%
    yardstick_like()
}


#' @rdname characterize
#' @export
characterize.fda <- function(x, ...) {
  characterize(x$fit)
}

# ------------------------------------------------------------------------------
# MARS via earth package

# earth uses the call object to prune the model
repair_mars_call <- function(x) {
  fit_call <- x$call
  needs_eval <- purrr::map_lgl(as.list(fit_call), rlang::is_quosure)
  if (any(needs_eval)) {
    eval_args <- names(needs_eval)[needs_eval]
    for (arg in eval_args) {
      # Use of calls and pairlists in map functions was deprecated in purrr 1.0.0.
      # TODO make parsnip issue for repair_call()
      fit_call[[arg]] <- rlang::eval_tidy(fit_call[[arg]])
    }
    x$call <- fit_call
  }


  # So earth:::update.earth() evaluate the call object. If the package is not
  # attached, it will fail since earth() isn't loaded
  if ( identical(x$call[1], rlang::call2("earth")) ) {
    x$call[1] <- rlang::call2("earth", .ns = "earth")
  }

  x
}


#' @rdname characterize
#' @export
characterize.earth <- function(x, num_terms = NULL, ...) {
  x <- repair_mars_call(x)
  max_num_terms <- x$nk
  if (is.null(num_terms)) {
    num_terms <- max_num_terms
  }

  if (num_terms < max_num_terms) {
    # TODO use a call handler to remove constant message of
    # 'glm.fit: fitted probabilities numerically 0 or 1 occurred'
    x <- update(x, nprune = num_terms, pmethod = x$pmethod)
  }

  dplyr::bind_rows(
    .pluck_num_features_input(x, ...),
    .pluck_num_features_active(x, num_terms = num_terms),
    .pluck_num_parameters(x, num_terms = num_terms),
  ) %>%
    yardstick_like()
}
