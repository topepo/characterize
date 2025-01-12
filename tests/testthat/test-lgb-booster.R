test_that("lgb.Booster - regression", {
  skip_if_not_installed("bundle")
  skip_if_not_installed("lightgbm")

  # tests objects in "test_cases.RData"

  fit_reg_lightgbm <- bundle::unbundle(fit_reg_lightgbm)

  chr_res <- characterize(fit_reg_lightgbm)
  check_characterize_object(chr_res)
  expect_snapshot(error = TRUE,
                  characterize(fit_reg_lightgbm, trees = 1:2)
  )

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

  fit_cls_lightgbm <- bundle::unbundle(fit_cls_lightgbm)

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

  fit_mnl_lightgbm <- bundle::unbundle(fit_mnl_lightgbm)

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

test_that("lgb.Booster - multi_characterize", {
  skip_if_not_installed("bundle")
  skip_if_not_installed("lightgbm")

  # tests objects in "test_cases.RData"
  vals <- c(1, 20, 100)

  res <- multi_characterize(fit_mnl_lightgbm)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  expect_named(res, c("trees", "results"))

  res <- multi_characterize(fit_mnl_lightgbm, trees = vals)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3L)
  expect_named(res, c("trees", "results"))

  fit_mnl_lightgbm <- bundle::unbundle(fit_mnl_lightgbm)

  res <- multi_characterize(fit_mnl_lightgbm, trees = vals)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3L)
  expect_named(res, c("trees", "results"))

  for (i in seq_along(vals)) {
    expt <- characterize(fit_mnl_lightgbm, trees = vals[i])
    expect_equal(res$results[[i]], expt)
    expect_equal(res$trees[i], vals[i])
  }
})
