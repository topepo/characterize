test_that("fda - earth - classification", {
  skip_if_not_installed("earth")
  suppressPackageStartupMessages(library(earth))

  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_cls_fda_earth)
  check_characterize_object(bag_chr)

  coefs <- fit_cls_fda_earth$fit$coefficients[,1]

  # not implemented
  # expect_equal(
  #   .pluck_num_parameters(fit_cls_fda_earth)$value,
  #   length(coefs)
  # )

  act_vars <-
    coefs %>%
    names() %>%
    purrr::map(expr_vars) %>%
    unlist() %>%
    unique()

  act_vars <- sort(act_vars[!grepl("(Intercept)|(unused)", act_vars)])

  expect_equal(
    .pluck_features_active(fit_cls_fda_earth)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_fda_earth)$value,
    length(act_vars)
  )
})
