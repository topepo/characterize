test_that("C5.0 - single classification tree", {
  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_tree_c5)
  check_characterize_object(chr_res)
  expect_snapshot(error = TRUE,
                  characterize(fit_cls_tree_c5, trials = 1:2)
  )


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

  chr_res <- characterize(fit_cls_rule_c5)
  check_characterize_object(chr_res)

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

  chr_res <- characterize(fit_cls_tree_bst_c5)
  check_characterize_object(chr_res)

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

  chr_res <- characterize(fit_cls_rule_bst_c5)
  check_characterize_object(chr_res)

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

test_that("C5.0 - multi_characterize", {
  # tests objects in "test_cases.RData"

  vals <- 1:3

  res <- multi_characterize(fit_cls_tree_c5_3)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  expect_named(res, c("trials", "results"))

  res <- multi_characterize(fit_cls_tree_c5_3, trials = vals)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3L)
  expect_named(res, c("trials", "results"))

  for (i in seq_along(vals)) {
    expt <- characterize(fit_cls_tree_c5_3, trials = vals[i])
    expect_equal(res$results[[i]], expt)
    expect_equal(res$trials[i], vals[i])
  }
})

