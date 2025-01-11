test_that("brulee - classification - neural network", {
  load(test_path("test_cases.RData"))

  brulee_res <- characterize(fit_cls_bru_mlp)

  check_characterize_object(brulee_res)

  predictors <- all.vars(fit_cls_bru_mlp$blueprint$terms$predictors)
  predictors <- sort(predictors)

  expect_equal(
    .pluck_num_features_active(fit_cls_bru_mlp)$value,
    length(predictors)
  )
  expect_equal(
    .pluck_features_active(fit_cls_bru_mlp)$value[[1]],
    predictors
  )

  expect_equal(
    .pluck_num_parameters(fit_cls_bru_mlp)$value,
    length(unlist(fit_cls_bru_mlp$estimates[[1]]))
  )
})

test_that("brulee - regression - neural network", {
  load(test_path("test_cases.RData"))

  brulee_res <- characterize(fit_reg_bru_mlp)

  check_characterize_object(brulee_res)

  predictors <- all.vars(fit_reg_bru_mlp$blueprint$terms$predictors)
  predictors <- sort(predictors)

  expect_equal(
    .pluck_num_features_active(fit_reg_bru_mlp)$value,
    length(predictors)
  )
  expect_equal(
    .pluck_features_active(fit_reg_bru_mlp)$value[[1]],
    predictors
  )

  expect_equal(
    .pluck_num_parameters(fit_reg_bru_mlp)$value,
    length(unlist(fit_reg_bru_mlp$estimates[[1]]))
  )
})

