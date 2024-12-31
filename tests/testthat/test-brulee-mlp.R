test_that("brulee - neural network", {
  load(test_path("test_cases.RData"))
  skip("refactoring")

  brulee_res <- characterize(brulee_mlp_mod)

  check_characterize_object(brulee_res)

  expect_equal(
    .pluck_num_parameters(brulee_mlp_mod)$value,
    length(unlist(brulee_mlp_mod$fit$estimates[[1]]))
  )
})
