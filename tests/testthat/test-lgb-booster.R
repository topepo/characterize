test_that("lgb.Booster - regression", {
  skip_if_not_installed("bundle")
  skip_if_not_installed("lightgbm")

  # tests objects in "test_cases.RData"

  fit_reg_lightgbm <- unbundle(fit_reg_lightgbm)

  chr_res <- characterize(fit_reg_lightgbm)
  check_characterize_object(chr_res)

  tree_data <- lgb_trees(fit_reg_lightgbm)

  act_vars <- sort(unique(tree_data$split_feature))
  act_vars <- sort(act_vars)

  expect_equal(
    .pluck_features_active(fit_reg_lightgbm)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_lightgbm)$value,
    length(act_vars)
  )
  expect_equal(
    .pluck_num_term_nodes(fit_reg_lightgbm)$value,
    sum(!is.na(tree_data$leaf_count))
  )
})

test_that("lgb.Booster - binary classification", {
  skip_if_not_installed("bundle")
  skip_if_not_installed("lightgbm")

  # tests objects in "test_cases.RData"

  fit_cls_lightgbm <- unbundle(fit_cls_lightgbm)

  chr_res <- characterize(fit_cls_lightgbm)
  check_characterize_object(chr_res)

  tree_data <- lgb_trees(fit_cls_lightgbm)

  act_vars <- sort(unique(tree_data$split_feature))
  act_vars <- sort(act_vars)

  expect_equal(
    .pluck_features_active(fit_cls_lightgbm)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_lightgbm)$value,
    length(act_vars)
  )
  expect_equal(
    .pluck_num_term_nodes(fit_cls_lightgbm, trees = 100)$value,
    sum(!is.na(tree_data$leaf_count))
  )
})

test_that("lgb.Booster - multiclass classification", {
  skip_if_not_installed("bundle")
  skip_if_not_installed("lightgbm")

  # tests objects in "test_cases.RData"

  fit_mnl_lightgbm <- unbundle(fit_mnl_lightgbm)

  chr_res <- characterize(fit_mnl_lightgbm)
  check_characterize_object(chr_res)

  tree_data <- lgb_trees(fit_mnl_lightgbm)

  act_vars <- sort(unique(tree_data$split_feature))
  act_vars <- sort(act_vars)

  expect_equal(
    .pluck_features_active(fit_mnl_lightgbm)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_lightgbm)$value,
    length(act_vars)
  )

  # TODO This may not be right
  # expect_equal(
  #   .pluck_num_term_nodes(fit_mnl_lightgbm)$value,
  #   sum(!is.na(tree_data$leaf_count))
  # )
})
