suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(parsnip))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(workflows))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(characterize))

check_characterize_object <- function(x) {
  ptype <-
    tibble(
    .metric = character(0),
    .estimator = character(0),
    .estimate = numeric(0)
  )
  expect_equal(x[0,], ptype)
  expect_true(all(x$.estimator == "model"))
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  invisible(NULL)
}

load(test_path("test_cases.RData"))

# ------------------------------------------------------------------------------
# helpers for specific tests

expr_vars <- function(x) all.vars(parse_expr(x))

rpart_term_nodes <- function(x) {
  if (inherits(x, "model_fit")) {
    x <- extract_fit_engine(x)
  }
  sum(x$frame$var == "<leaf>")
}

rpart_active_vars <- function(x) {
  if (inherits(x, "model_fit")) {
    x <- extract_fit_engine(x)
  }
  x$frame$var[x$frame$var != "<leaf>"]
}
