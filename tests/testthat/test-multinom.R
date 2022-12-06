library(testthat)
library(rlang)


test_that("multinom", {
  load(test_path("test_cases.RData"))

  expect_snapshot(characterize(mtn_mod))
  expect_equal(
    .pluck_num_parameters(mtn_mod)$value,
    length(mtn_mod$fit$wts[mtn_mod$fit$wts != 0])
  )
})
