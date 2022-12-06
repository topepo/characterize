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

#' @export
#' @rdname retain_characteristics
collect_characteristics <-
  function(x, summarize = TRUE, parameters = NULL, add_metrics = FALSE, wide = FALSE) {
    if (summarize) {
      res <- tune::estimate_tune_results(x, col_name = ".extracts")
    } else {
      res <-
        # TODO pull out the extract code from estimate_tune_results
        x %>%
        dplyr::select(dplyr::starts_with("id"), .extracts) %>%
        # expands the main results
        tidyr::unnest(.extracts) %>%
        # extracts the characteristic results
        tidyr::unnest(.extracts)
    }
    if (add_metrics) {
      metric_res <- tune::collect_metrics(x, summarize = summarize, parameters = parameters)
      if (wide) {
        metrics <- unique(metric_res$.metric)
        res <-
          metric_res %>%
          dplyr::select(.config, mean, .metric) %>%
          tidyr::pivot_wider(id_cols = .config, names_from = .metric, values_from = mean) %>%
          dplyr::full_join(res, by = ".config") %>%
          dplyr::relocate(!!!metrics, .after = dplyr::last_col())
      } else {
        res <- dplyr::bind_rows(res, metric_res)
      }
    }
    dplyr::arrange(res, .config, .metric)
  }
