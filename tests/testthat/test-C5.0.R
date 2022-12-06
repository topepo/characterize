library(testthat)
library(rlang)


test_that("C5.0", {
  skip_if_not_installed("C50")

  load(test_path("test_cases.RData"))

  # single tree
  # TODO characterize(c5_mod) doesn't work for trees

  # single rule sets
  expect_snapshot(characterize(c5_rules_mod))

  # boosted
  # TODO characterize(c5_boost_mod) doesn't work for trees
})
