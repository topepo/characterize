library(testthat)
library(rlang)


test_that("ctree", {
  skip_if_not_installed("bonsai")
  skip_if_not_installed("partykit")
  skip("refactoring")

  load(test_path("test_cases.RData"))
  expect_snapshot(characterize(ctree_mod))
})
