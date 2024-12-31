test_that("brulee - multinomial regression", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_cls_bru_multi)
  check_characterize_object(bag_chr)

  predictors <- all.vars(fit_cls_bru_multi$blueprint$terms$predictors)
  predictors <- sort(predictors)

  expect_equal(
    .pluck_num_features_active(fit_cls_bru_multi)$value,
    length(predictors)
  )
  expect_equal(
    .pluck_features_active(fit_cls_bru_multi)$value[[1]],
    predictors
  )

})
