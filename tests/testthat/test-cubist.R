library(testthat)
library(rlang)

test_that("cubist", {
  skip_if_not_installed("rules")
  skip_if_not_installed("Cubist")

  load(test_path("test_cases.RData"))

  expect_snapshot(characterize(cubist_mod))

  expect_equal(
    .pluck_num_active_features(cubist_mod)$value,
    nrow(cubist_mod$fit$usage)
  )

  expect_equal(
    .pluck_num_parameters(cubist_mod)$value,
    exp_num_prm_cb_2
  )

  expect_equal(
    .pluck_num_rules(cubist_mod)$value,
    exp_num_rules_cb_2
  )

  expect_equal(
    .pluck_mean_rule_size(cubist_mod)$value,
    exp_rule_size_cb_2
  )

  expect_snapshot(characterize(cubist_mod, committees = 1))

  expect_equal(
    .pluck_num_active_features(cubist_mod, committees = 1)$value,
    nrow(cubist_mod$fit$usage)
  )

  expect_equal(
    .pluck_num_parameters(cubist_mod, committees = 1)$value,
    exp_num_prm_cb_1
  )

  expect_equal(
    .pluck_num_rules(cubist_mod, committees = 1)$value,
    exp_num_rules_cb_1
  )

  expect_equal(
    .pluck_mean_rule_size(cubist_mod, committees = 1)$value,
    exp_rule_size_cb_1
  )


})
