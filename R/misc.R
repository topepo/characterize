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
    # tidy_cubist is an intermediary method; there are no objects that get that class
    dplyr::filter(object != "tidy_cubist")
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

is_rcmd_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}