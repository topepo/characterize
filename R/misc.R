# ------------------------------------------------------------------------------
# Reformat the results to resemble what is produced by yardstick metrics

yardstick_like <- function(x) {
  x %>%
    dplyr::mutate(
      .estimator = "model",
      value = as.numeric(value),
    ) %>%
    dplyr::select(.metric = statistic, .estimator, .estimate = value)
}

rien <-
  tibble::tibble(
    .metric = character(0),
    .estimator = character(0),
    .estimate = numeric(0)
  )

niente <-
  tibble::tibble(
    statistic = character(0),
    value = numeric(0)
  )

nolla <-
  tibble::tibble(
    statistic = character(0),
    value = list(character(0))
  )

# ------------------------------------------------------------------------------

num_uniq <- function(x) length(unique(x))

num_coef <- function(x) {
  res <- coef(x)

  if (is.list(res)) {
    res <- unlist(res)
  }
  if (!is.vector(res)) {
    res <- as.vector(res)
  }
  length(res)
}

coef_param <- function(x, ...) {
  res <- tibble::tibble(statistic = "num_parameters",
                        value = num_coef(x))
}

# ------------------------------------------------------------------------------
#' Show lists of models and characteristics
#' @return `list_characteristics()` returns a tibble with columns
#' `characteristic` and `object`. `object_list()` returns a character string.
#' @export
list_characteristics <- function() {
  char_funs <- asNamespace("characterize")
  plucks <- utils::ls.str(envir = char_funs, pattern = "^\\.pluck_", all.names = TRUE)
  split_up <- strsplit(plucks, "\\.")

  gnric_nm <- purrr::map_chr(split_up, ~ gsub("pluck_", "", .x[2]))
  mthd_nm <- purrr::map_chr(split_up, ~ paste(.x[-(1:2)], collapse = "."))

  info <-
    tibble::tibble(characteristic = gnric_nm, object = mthd_nm) %>%
    dplyr::filter(!(object %in% c("", "default")) & !(object %in% c("workflow", "model_fit"))) %>%
    # tidy_* are intermediary methods; there are no objects that get these classes
    dplyr::filter(!grepl("^tidy_", object)) %>%
    dplyr::filter(!grepl("^lgb_tree", object))
  info
}

#' @export
#' @rdname list_characteristics
object_list <- function() {
  res <- list_characteristics()
  models <- sort(unique(res$object))
  paste0("`", models, "`", collapse = ", ")
}

# ------------------------------------------------------------------------------

check_wflow_fit <- function(x, call = rlang::env_parent()) {
  is_trained <- workflows::is_trained_workflow(x)
  if (!is_trained) {
    cli::cli_abort("The workflow should be trainined.", call = call)
  }
  invisible(NULL)
}

is_predictor_role <- function(x) {
  vapply(x$role, function(x) any(x == "predictor"), logical(1))
}

check_recipe_fit <- function(x, call = rlang::env_parent()) {
  is_trained <- vapply(x$steps, function(x) x$trained, logical(1))
  if (!all(is_trained)) {
    cli::cli_abort("All recipe steps should be trainined.", call = call)
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------

is_rcmd_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}
