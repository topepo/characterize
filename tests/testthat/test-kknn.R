library(testthat)
library(rlang)


test_that("kknn", {
  skip_if_not_installed("kknn")

  load(test_path("test_cases.RData"))

  expect_snapshot(characterize(knn_mod))
})
