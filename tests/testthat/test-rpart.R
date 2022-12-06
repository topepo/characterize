library(testthat)
library(rlang)


test_that("rpart", {
  skip_if_not_installed("rpart")

  load(test_path("test_cases.RData"))
  expect_snapshot(characterize(rpart_mod))
})
