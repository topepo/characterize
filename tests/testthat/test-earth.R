test_that("earth - regression", {
  skip_if_not_installed("earth")
  suppressPackageStartupMessages(library(earth))

  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_reg_earth)
  check_characterize_object(bag_chr)

  coefs <- coef(fit_reg_earth)
  expect_equal(
    .pluck_num_parameters(fit_reg_earth)$value,
    length(coefs)
  )

  act_vars <-
    coefs %>%
    names() %>%
    purrr::map(expr_vars) %>%
    unlist() %>%
    unique()

  act_vars <- sort(act_vars[!grepl("(Intercept)|(unused)", act_vars)])

  expect_equal(
    .pluck_features_active(fit_reg_earth)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_earth)$value,
    length(act_vars)
  )
})

test_that("earth - classification", {
  skip_if_not_installed("earth")
  suppressPackageStartupMessages(library(earth))

  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_cls_earth)
  check_characterize_object(bag_chr)

  coefs <- coef(fit_cls_earth)
  expect_equal(
    .pluck_num_parameters(fit_cls_earth)$value,
    length(coefs)
  )

  act_vars <-
    coefs %>%
    names() %>%
    purrr::map(expr_vars) %>%
    unlist() %>%
    unique()

  act_vars <- sort(act_vars[!grepl("(Intercept)|(unused)", act_vars)])

  expect_equal(
    .pluck_features_active(fit_cls_earth)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_earth)$value,
    length(act_vars)
  )
})
