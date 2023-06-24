#' tune helpers
#' @inheritParams tune::collect_predictions
#' @param wflow A fitted model workflow.
#' @param add_metrics A logical to combine the results the associated
#' performance metrics.
#' @param wide A logical for making some information in wide format.
#' @return A tibble.
#' @details
#' When using the \pkg{tune} or \pkg{finetune} packages,
#' `retain_characteristics()` can be used with the resampling and tuning functions
#' to return the characteristics from multiple model configurations. These
#' results are contained in a column called `.extracts`.
#'
#' `collect_characteristics()` can be used to easily wrangle the results in the
#' `.extracts` column into a tibble of usable results.
#'
#' The vignette _Using characterize with tidymodels_ shows an example.
#'
#' @export
retain_characteristics <- function(wflow) {
  multi_characterize(wflow)
}

# TODO this won't work since extracts only get the workflow
# and not the grid (or param info). We can derive the submodel
# points used in the grid from the workflow alone.

#' @export
#' @rdname retain_characteristics
collect_characteristics <-
  function(x, summarize = TRUE, add_metrics = FALSE, wide = FALSE) {
    tune_param <- .get_tune_parameter_names(x)
    tune_key <- c(".config", tune_param) # .iter
    id_cols<- grep("^id", names(x), value = TRUE)

    extracts <- tune::collect_extracts(x)
    extracts <- rm_dup_param_cols(extracts, tune_param)
    extracts <- tidyr::unnest(extracts, c(.extracts))
    # check for 'results' column
    extracts <- tidyr::unnest(extracts, c(results))

    # --------------------------------------------------------------------------

    if (summarize) {

      group_cols <- c(tune_key, ".metric", ".estimator")
      extracts <-
        extracts %>%
        summarize(
          mean = mean(.estimate, na.rm = TRUE),
          n = sum(!is.na(.estimate)),
          std_err = sd(.estimate, na.rm = TRUE) / sqrt(n),
          .by = c(dplyr::all_of(group_cols)))

    }

    # --------------------------------------------------------------------------

    if (add_metrics) {

      if (wide) {
        extracts <-
          tune::pivot_metrics(x, summarize = summarize) %>%
          full_join(extracts, by = tune_key) %>%
          dplyr::relocate(!!!tune_param) %>%
          dplyr::relocate(.config, .after = "std_err")
      } else {
        metric_res <- tune::collect_metrics(x, summarize = summarize)
        extracts <- dplyr::bind_rows(extracts, metric_res) %>% dplyr::arrange(.config, .metric)
      }

    } else {
      extracts <- dplyr::arrange(extracts, .config, .metric)
    }

    # --------------------------------------------------------------------------

    extracts
  }

# ------------------------------------------------------------------------------
# these should go in tune

# Return a list of vectors for sub-models (if any)
sub_model_param <- function(x, grid) {
  # get model type
  # find parameters
  # get names of submodel parameters
  # see if they are in the grid
  # get unique values

}

# does not return id
sub_model_params <- function(spec) {
  get_from_env(paste0(class(spec)[1], "_args")) %>%
    dplyr::filter(has_submodel) %>%
    dplyr::select(-func, -has_submodel)
}


# Some models have "sub-model parameters". When these exist, there are likely
# tuning parameter columns in the extracted results that have duplicate columns.
# One is at the top-level result and another inside of the '.extracts' list
# column. The latter is the more accurate data. If this occurs, we remove the
# column(s) at the top-level.
rm_dup_param_cols <- function(x, param_names, target_col = ".extracts") {
  sub_names <- names(x[[target_col]][[1]])
  common_names <- intersect(param_names, sub_names)
  if ( length(common_names) > 0 ) {
    x <- x[, !(names(x) %in% common_names)]
  }
  x
}

