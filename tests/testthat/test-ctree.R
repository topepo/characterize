library(testthat)
library(rlang)


test_that("ctree", {
  load(test_path("test_cases.RData"))
  expect_snapshot(characterize(ctree_mod))
})
