library(testthat)
library(rlang)


test_that("characterizing an object", {
  skip_if_not_installed("kernlab")

  load(test_path("test_cases.RData"))
  nls_fit <- nls(demand ~ SSasympOrig(Time, A, lrc), data = BOD)

  expect_equal(
    characterize(svm_mod),
    tibble::tibble(
      .metric = "num_support_vectors",
      .estimator = "model",
      .estimate = 42
    )
  )
  expect_equal(characterize(nls_fit), characterize:::rien)
})
