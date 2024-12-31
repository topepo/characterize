test_that("brulee - linear regression", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_reg_bru_lin)
  check_characterize_object(bag_chr)

  predictors <- all.vars(fit_reg_bru_lin$blueprint$terms$predictors)
  predictors <- sort(predictors)

  expect_equal(
    .pluck_num_features_active(fit_reg_bru_lin)$value,
    length(predictors)
  )
  expect_equal(
    .pluck_features_active(fit_reg_bru_lin)$value[[1]],
    predictors
  )

})
