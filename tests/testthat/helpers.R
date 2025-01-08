suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(dplyr))
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

terms_vars <- function(x) {
  sort(colnames(attr(x$terms, "factors")))
}

ranger_vars <- function(x) {
  vars <- unique(unlist(x$forest$split.varIDs))
  used <- x$forest$independent.variable.names[vars + 1]
  sort(used)
}

rf_vars <- function(x) {
  var_index <- unique(unlist(x$forest$bestvar))
  var_nms <- names(x$forest$xlevels)[var_index]
  sort(unique(var_nms))
}

non_zero_load <- function(x) {
  nz_x <- apply(x$loadings$X, 1, function(x) any(x != 0.0))
  sort(rownames(x$loadings$X)[nz_x])
}

# ------------------------------------------------------------------------------

if (rlang::is_installed(c("modeldata"))) {

  # Make test data sets that mirror those in inst/test_objects.R
  data(ames, package = "modeldata")
  ames$Sale_Price <- log10(ames$Sale_Price)
  ames <-
    ames %>%
    dplyr::mutate(Sale_Price <- log10(Sale_Price)) %>%
    dplyr::slice(1:100) %>%
    dplyr::select(Sale_Price, Neighborhood, Longitude, Latitude)

  data("penguins", package = "modeldata")
  penguins <- penguins[complete.cases(penguins),]

  set.seed(1)
  cls_dat <- modeldata::sim_classification(50)
  reg_dat <- modeldata::sim_regression(50)
  mnl_dat <-
    modeldata::sim_multinomial(
      100,
      ~  -0.5    +  0.6 * abs(A),
      ~ ifelse(A > 0 & B > 0, 1.0 + 0.2 * A / B, - 2),
      ~ -0.6 * A + 0.50 * B -  A * B)

  count_dat <- reg_dat
  count_dat$outcome <- rpois(nrow(reg_dat), exp(reg_dat$outcome / 10))
}
