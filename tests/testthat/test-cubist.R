test_that("Cubist - single rule set", {
  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_cb)
  check_characterize_object(chr_res)

  # Computed in "inst/test_objects.R"
  expect_equal(
    .pluck_num_features_active(fit_reg_cb)$value,
    exp_reg_cb$num_features_active
  )
  expect_equal(
    .pluck_features_active(fit_reg_cb)$value[[1]],
    exp_reg_cb$features_active
  )
  expect_equal(
    .pluck_num_rules(fit_reg_cb)$value,
    exp_reg_cb$num_rules
  )
  expect_equal(
    .pluck_mean_rule_size(fit_reg_cb)$value,
    exp_reg_cb$mean_rule_size
  )
})

test_that("Cubist - ensemble", {
  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_cb_ens)
  check_characterize_object(chr_res)

  # Computed in "inst/test_objects.R"
  expect_equal(
    .pluck_num_features_active(fit_reg_cb_ens)$value,
    exp_reg_cb_ens$num_features_active
  )
  expect_equal(
    .pluck_features_active(fit_reg_cb_ens)$value[[1]],
    exp_reg_cb_ens$features_active
  )
  expect_equal(
    .pluck_num_rules(fit_reg_cb_ens)$value,
    exp_reg_cb_ens$num_rules
  )
  expect_equal(
    .pluck_mean_rule_size(fit_reg_cb_ens)$value,
    exp_reg_cb_ens$mean_rule_size
  )
})
