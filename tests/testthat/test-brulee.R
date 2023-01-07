library(testthat)
library(rlang)


test_that("brulee", {
  load(test_path("test_cases.RData"))

  expect_snapshot(characterize(brulee_mlp_mod))
  expect_equal(
    .pluck_num_parameters(brulee_mlp_mod)$value,
    length(unlist(brulee_mlp_mod$estimates[[1]]))
  )
})
