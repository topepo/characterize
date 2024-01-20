library(testthat)
library(rlang)


test_that("C5.0", {
  skip_if_not_installed("C50")
  skip_if_not_installed("rules")


  library(rules)
  load(test_path("test_cases.RData"))

  # single tree
  expect_snapshot(characterize(c5_mod))
  expect_equal(
    .pluck_num_features_active(c5_mod)$value,
    exp_act_feat_c5
  )
  tidy_c5_mod <- tidy(c5_mod)
  expect_equal(
    .pluck_num_term_nodes(c5_mod)$value,
    nrow(tidy_c5_mod)
  )
  expect_equal(
    .pluck_mean_rule_size(c5_mod)$value,
    mean(characterize:::rule_size(tidy_c5_mod$rule))
  )

  # single rule sets
  expect_snapshot(characterize(c5_rules_mod))
  expect_equal(
    .pluck_num_features_active(c5_rules_mod)$value,
    exp_act_feat_c5_rules
  )
  tidy_c5_rules_mod <- tidy(c5_rules_mod)
  expect_equal(
    .pluck_mean_rule_size(c5_rules_mod)$value,
    mean(characterize:::rule_size(tidy_c5_rules_mod$rule))
  )

  # boosted trees
  expect_snapshot(characterize(c5_boost_mod))
  expect_equal(
    .pluck_num_features_active(c5_boost_mod)$value,
    exp_act_feat_c5_boost
  )
  tidy_c5_boost_mod <- tidy(c5_boost_mod)
  expect_equal(
    .pluck_mean_rule_size(c5_boost_mod)$value,
    mean(characterize:::rule_size(tidy_c5_boost_mod$rule))
  )

  # boosted rules
  expect_snapshot(characterize(c5_rules_boost_mod))
  expect_equal(
    .pluck_num_features_active(c5_rules_boost_mod)$value,
    exp_act_feat_c5_rules_boost
  )
  tidy_c5_rules_boost_mod <- tidy(c5_rules_boost_mod)
  expect_equal(
    .pluck_mean_rule_size(c5_rules_boost_mod)$value,
    mean(characterize:::rule_size(tidy_c5_rules_boost_mod$rule))
  )

})
