#' Predictors given in the model
#'
#' `.pluck_features_input()` enumerates all of the features/predictors that
#' have or will be passed to a modeling function.
#'
#' @inheritParams characterize
#' @param data A data frame of the source data.
#' @export
#' @name pluck_features_input
#' @keywords internal
.pluck_features_input <- function(x, ...) {
  UseMethod(".pluck_features_input")
}

#' @rdname pluck_features_input
#' @export
.pluck_features_input.default <- function(x, ...) {
  if (has_terms(x)) {
    res <- .pluck_features_input(x$terms)
  } else if (has_blueprint(x)) {
    res <- .pluck_features_input(x$blueprint)
  } else{
    res <- nolla
  }
  res
}

#' @rdname pluck_features_input
#' @export
.pluck_features_input.workflow <- function(x, include_intercept = FALSE, ...) {
  check_wflow_fit(x)
  vars_chr <- names(x$pre$mold$predictors)
  input_vars_to_tbl(vars_chr, include_intercept)
}

#' @rdname pluck_features_input
#' @export
.pluck_features_input.model_fit <- function(x, ...) {
  if (has_preproc_terms(x)) {
    res <- .pluck_features_input(x$preproc$terms)
  } else {
    x <- parsnip::extract_fit_engine(x)
    if (has_blueprint(x)) {
      res <- .pluck_features_input(x$blueprint, ...)
    } else  if (has_terms(x)) {
      res <- .pluck_features_input(x$terms, ...)
    } else {
      res <- .pluck_features_input(x, ...)
    }
  }
  res
}

#' @rdname pluck_features_input
#' @export
.pluck_features_input.recipe <- function(x, ...) {
  check_recipe_fit(x)
  var_info <- x$last_term_info
  max_steps <- max(var_info$number)
  var_info <- var_info[var_info$number == max_steps, c("variable", "role")]
  var_info <- var_info[is_predictor_role(var_info), ]
  input_vars_to_tbl(var_info$variable)
}


#' @rdname pluck_num_features_input
#' @export
.pluck_features_input.hardhat_blueprint <- function(x, include_intercept = FALSE, ...) {
  res <- names(x$ptypes$predictors)
  input_vars_to_tbl(res, include_intercept)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_features_input
#' @export
.pluck_features_input.terms <- function(x, ...) {
  facts <- attr(x, "factors")
  resp <- attr(x, "response")
  vars_chr <- rownames(facts)[-resp]
  input_vars_to_tbl(vars_chr)
}

#' @rdname pluck_features_input
#' @export
.pluck_features_input.formula <- function(x, data, ...) {
  trms <- stats::terms(x, data = data[0,])
  .pluck_features_input(trms)
}

# ------------------------------------------------------------------------------

#' @rdname pluck_features_input
#' @export
.pluck_features_input.keras.engine.sequential.Sequential <- function(x, ...) {
  .pluck_features_input(x$preproc$terms)
}
#' @rdname pluck_features_input
#' @export
.pluck_features_input._keras.engine.sequential.Sequential <- function(x, ...) {
  .pluck_features_input(x$preproc$terms)
}

#' @rdname pluck_features_input
#' @export
.pluck_features_input.glmnet <- function(x, ...) {
  input_vars_to_tbl(rownames(x$beta))
}

#' @rdname pluck_features_input
#' @export
.pluck_features_input.multnet <- function(x, ...) {
  res <- purrr::map(x$beta, rownames)
  res <- unlist(res)
  input_vars_to_tbl(res)
}


# ------------------------------------------------------------------------------

input_vars_to_tbl <- function(x, include_intercept = FALSE) {
  if (!include_intercept) {
    x <- x[x != "(Intercept)"] # TODO generalize this with doubles
  }

  tibble::tibble(statistic = "active_input",
                 value = list(sort(unique(x))))
}

has_preproc_terms <- function(x) {
  res <- FALSE
  if (any(names(x) == "preproc")) {
    if (any(names(x$preproc) == "terms")) {
      res <- TRUE
    }
  }
  res
}

has_blueprint <- function(x) {
  any(names(x) == "blueprint")
}
