test_that("nnet - regression", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_nnet)
  check_characterize_object(chr_res)

  act_vars <- terms_vars(fit_reg_nnet)

  expect_equal(
    .pluck_num_features_input(fit_reg_nnet)$value,
    length(act_vars)
  )

  expect_equal(
    .pluck_num_parameters(fit_reg_nnet)$value,
    length(fit_reg_nnet$wts)
  )
})

test_that("nnet - binary classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_nnet)
  check_characterize_object(chr_res)

  act_vars <- terms_vars(fit_cls_nnet)

  expect_equal(
    .pluck_num_features_input(fit_cls_nnet)$value,
    length(act_vars)
  )

  expect_equal(
    .pluck_num_parameters(fit_cls_nnet)$value,
    length(fit_cls_nnet$wts)
  )
})

test_that("nnet - mutinomial classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_mnl_nnet)
  check_characterize_object(chr_res)

  act_vars <- terms_vars(fit_mnl_nnet)

  expect_equal(
    .pluck_num_features_input(fit_mnl_nnet)$value,
    length(act_vars)
  )

  expect_equal(
    .pluck_num_parameters(fit_mnl_nnet)$value,
    length(fit_mnl_nnet$wts)
  )
})
