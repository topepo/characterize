library(testthat)
library(rlang)


test_that("kknn", {
  load(test_path("test_cases.RData"))

  expect_snapshot(characterize(knn_mod))
})
