test_that("randomForest - regression", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_randomForest)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_reg_randomForest)$value[[1]],
    rf_vars(fit_reg_randomForest)
  )

  expect_equal(
    .pluck_num_features_active(fit_reg_randomForest)$value[[1]],
    length(rf_vars(fit_reg_randomForest))
  )

  expect_equal(
    .pluck_num_term_nodes(fit_reg_randomForest)$value,
    sum(fit_reg_randomForest$forest$nodestatus == -1)
  )
})

test_that("randomForest - binary classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_randomForest)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_cls_randomForest)$value[[1]],
    rf_vars(fit_cls_randomForest)
  )

  expect_equal(
    .pluck_num_features_active(fit_cls_randomForest)$value[[1]],
    length(rf_vars(fit_cls_randomForest))
  )

  expect_equal(
    .pluck_num_term_nodes(fit_cls_randomForest)$value,
    sum(fit_cls_randomForest$forest$nodestatus == -1)
  )
})

test_that("randomForest - multinomial classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_mnl_randomForest)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_mnl_randomForest)$value[[1]],
    rf_vars(fit_mnl_randomForest)
  )

  expect_equal(
    .pluck_num_features_active(fit_mnl_randomForest)$value[[1]],
    length(rf_vars(fit_mnl_randomForest))
  )

  expect_equal(
    .pluck_num_term_nodes(fit_mnl_randomForest)$value,
    sum(fit_mnl_randomForest$forest$nodestatus == -1)
  )
})

