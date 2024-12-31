test_that("C5.0 - single classification tree", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_cls_tree_c5)
  check_characterize_object(bag_chr)

  # Computed in "inst/test_objects.R"
  expect_equal(
    .pluck_num_features_active(fit_cls_tree_c5)$value,
    exp_cls_tree_c5$num_features_active
  )
  expect_equal(
    .pluck_features_active(fit_cls_tree_c5)$value[[1]],
    exp_cls_tree_c5$features_active
  )
  expect_equal(
    .pluck_num_term_nodes(fit_cls_tree_c5)$value,
    exp_cls_tree_c5$num_term_nodes
  )
})

test_that("C5.0 - single rule set", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_cls_rule_c5)
  check_characterize_object(bag_chr)

  # Computed in "inst/test_objects.R"
  expect_equal(
    .pluck_num_features_active(fit_cls_rule_c5)$value,
    exp_cls_rule_c5$num_features_active
  )
  expect_equal(
    .pluck_features_active(fit_cls_rule_c5)$value[[1]],
    exp_cls_rule_c5$features_active
  )
  expect_equal(
    .pluck_num_rules(fit_cls_rule_c5)$value,
    exp_cls_rule_c5$num_rules
  )
})

test_that("C5.0 - boosted trees", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_cls_tree_bst_c5)
  check_characterize_object(bag_chr)

  # Computed in "inst/test_objects.R"
  expect_equal(
    .pluck_num_features_active(fit_cls_tree_bst_c5)$value,
    exp_cls_tree_bst_c5$num_features_active
  )
  expect_equal(
    .pluck_features_active(fit_cls_tree_bst_c5)$value[[1]],
    exp_cls_tree_bst_c5$features_active
  )
  expect_equal(
    .pluck_num_term_nodes(fit_cls_tree_bst_c5)$value,
    exp_cls_tree_bst_c5$num_term_nodes
  )
})

test_that("C5.0 - boosted rule set", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_cls_rule_bst_c5)
  check_characterize_object(bag_chr)

  # Computed in "inst/test_objects.R"
  expect_equal(
    .pluck_num_features_active(fit_cls_rule_bst_c5)$value,
    exp_cls_rule_bst_c5$num_features_active
  )
  expect_equal(
    .pluck_features_active(fit_cls_rule_bst_c5)$value[[1]],
    exp_cls_rule_bst_c5$features_active
  )
  expect_equal(
    .pluck_num_rules(fit_cls_rule_bst_c5)$value,
    exp_cls_rule_bst_c5$num_rules
  )
})
