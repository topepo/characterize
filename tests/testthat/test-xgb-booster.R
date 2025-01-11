test_that("xgb.Booster - regression", {
  skip_if_not_installed("bundle")
  skip_if_not_installed("xgboost")

  # tests objects in "test_cases.RData"

  fit_reg_xgboost <- bundle::unbundle(fit_reg_xgboost)

  chr_res <- characterize(fit_reg_xgboost)
  check_characterize_object(chr_res)

  tree_data <- xgboost::xgb.importance(model = fit_reg_xgboost)
  tree_splits <- xgboost::xgb.model.dt.tree(model = fit_reg_xgboost)

  act_vars <- sort(unique(tree_data$Feature))
  act_vars <- sort(act_vars)

  expect_equal(
    .pluck_features_active(fit_reg_xgboost)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_xgboost)$value,
    length(act_vars)
  )
  expect_equal(
    .pluck_num_term_nodes(fit_reg_xgboost)$value,
    sum(tree_splits$Feature == "Leaf")
  )
})

test_that("xgb.Booster - binary classification", {
  skip_if_not_installed("bundle")
  skip_if_not_installed("xgboost")

  # tests objects in "test_cases.RData"

  fit_cls_xgboost <- bundle::unbundle(fit_cls_xgboost)

  chr_res <- characterize(fit_cls_xgboost)
  check_characterize_object(chr_res)

  tree_data <- xgboost::xgb.importance(model = fit_cls_xgboost)
  tree_splits <- xgboost::xgb.model.dt.tree(model = fit_cls_xgboost)

  act_vars <- sort(unique(tree_data$Feature))
  act_vars <- sort(act_vars)

  expect_equal(
    .pluck_features_active(fit_cls_xgboost)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_xgboost)$value,
    length(act_vars)
  )
  expect_equal(
    .pluck_num_term_nodes(fit_cls_xgboost)$value,
    sum(tree_splits$Feature == "Leaf")
  )
})

test_that("xgb.Booster - multiclass classification", {
  skip_if_not_installed("bundle")
  skip_if_not_installed("xgboost")

  # tests objects in "test_cases.RData"

  fit_mnl_xgboost <- bundle::unbundle(fit_mnl_xgboost)

  chr_res <- characterize(fit_mnl_xgboost)
  check_characterize_object(chr_res)

  tree_data <- xgboost::xgb.importance(model = fit_mnl_xgboost)
  tree_splits <- xgboost::xgb.model.dt.tree(model = fit_mnl_xgboost)

  act_vars <- sort(unique(tree_data$Feature))
  act_vars <- sort(act_vars)

  expect_equal(
    .pluck_features_active(fit_mnl_xgboost)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_xgboost)$value,
    length(act_vars)
  )

  expect_equal(
    .pluck_num_term_nodes(fit_mnl_xgboost)$value,
    sum(tree_splits$Feature == "Leaf")
  )
})
