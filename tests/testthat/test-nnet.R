library(testthat)
library(rlang)


test_that("nnet", {
  load(test_path("test_cases.RData"))

  expect_snapshot(characterize(nnet_mod))
  expect_equal(
    .pluck_num_parameters(nnet_mod)$value,
    length(nnet_mod$fit$wts)
  )
})
