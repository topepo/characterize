library(testthat)
library(rlang)


test_that("rpart", {
  load(test_path("test_cases.RData"))
  expect_snapshot(characterize(rpart_mod))
})
