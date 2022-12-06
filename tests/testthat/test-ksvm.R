library(testthat)
library(rlang)


test_that("kernlab SVM", {
  load(test_path("test_cases.RData"))

  expect_snapshot(characterize(svm_mod))
  expect_equal(
    .pluck_num_support_vectors(svm_mod)$value,
    length(svm_mod$fit@alphaindex[[1]])
  )
})
