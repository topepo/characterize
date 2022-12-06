library(testthat)
library(rlang)


test_that("earth", {
  skip_if_not_installed("earth")

  load(test_path("test_cases.RData"))

  # regression
  expect_snapshot(characterize(earth_mod))
  expect_equal(
    .pluck_num_parameters(earth_mod)$value,
    nrow(earth_mod$fit$coefficients)
  )
  expect_equal(
    .pluck_num_active_features(earth_mod)$value,
    exp_act_feat_earth_reg
  )

  # classification
  expect_snapshot(characterize(earth_cls_mod))
  expect_equal(
    .pluck_num_parameters(earth_cls_mod)$value,
    nrow(earth_cls_mod$fit$coefficients)
  )
  expect_equal(
    .pluck_num_active_features(earth_cls_mod)$value,
    exp_act_feat_earth_cls
  )
})
