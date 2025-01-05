test_that("kknn - regression", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_kknn)
  check_characterize_object(chr_res)

  act_vars <- terms_vars(fit_reg_kknn)

  expect_equal(
    .pluck_features_active(fit_reg_kknn)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_kknn)$value,
    length(act_vars)
  )
  expect_equal(
    .pluck_num_features_input(fit_reg_kknn)$value,
    length(act_vars)
  )
})

test_that("kknn - classification", {
  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_kknn)
  check_characterize_object(chr_res)

  act_vars <- terms_vars(fit_cls_kknn)

  expect_equal(
    .pluck_features_active(fit_cls_kknn)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_kknn)$value,
    length(act_vars)
  )
  expect_equal(
    .pluck_num_features_input(fit_cls_kknn)$value,
    length(act_vars)
  )
})
