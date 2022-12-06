library(testthat)
library(rlang)


test_that("ranger", {
  load(test_path("test_cases.RData"))
  expect_snapshot(characterize(ranger_mod))

  # TODO Add expected results to test case file
})
