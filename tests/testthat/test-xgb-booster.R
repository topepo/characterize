test_that("xgb.Booster - regression", {
  skip_if_not_installed("bundle")
  skip_if_not_installed("xgboost")

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_xgboost)

  fit_reg_xgboost <- bundle::unbundle(fit_reg_xgboost)

  chr_res <- characterize(fit_reg_xgboost)
  check_characterize_object(chr_res)
  expect_snapshot(error = TRUE,
                  characterize(fit_reg_xgboost, nrounds = 1:2)
  )

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

  # TODO this isn't correct; maybe multiple entries per node with multiclass
  # expect_equal(
  #   .pluck_num_term_nodes(fit_mnl_xgboost)$value,
  #   sum(tree_splits$Feature == "Leaf")
  # )
})

test_that("xgb.Booster - multi_characterize", {
  skip_if_not_installed("bundle")
  skip_if_not_installed("xgboost")
  # With xgboost 1.7.8.1, this code seg faults when run non-interactively. The
  # message is "address 0xa0, cause 'invalid permissions'"
  skip_if(!interactive(), message = "xgboost 1.7.8.1 seg faults interactively")

  # tests objects in "test_cases.RData"

  vals <- c(1, 2, 10)

  res <- multi_characterize(fit_mnl_xgboost)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  expect_named(res, c("nrounds", "results"))

  res <- multi_characterize(fit_mnl_xgboost, nrounds = vals)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3L)
  expect_named(res, c("nrounds", "results"))
  #
  fit_mnl_xgboost <- bundle::unbundle(fit_mnl_xgboost)

  res <- multi_characterize(fit_mnl_xgboost, nrounds = vals)
    expect_s3_class(res, "tbl_df")
    expect_equal(nrow(res), 3L)
    expect_named(res, c("nrounds", "results"))

    for (i in seq_along(vals)) {
      expt <- characterize(fit_mnl_xgboost, nrounds = vals[i])
      expect_equal(res$results[[i]], expt)
      expect_equal(res$nrounds[i], vals[i])
    }
})
