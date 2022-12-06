library(testthat)
library(rlang)


test_that("pls via mixOmics", {
  load(test_path("test_cases.RData"))

  # regression
  expect_snapshot(characterize(pls_mod))
  expect_equal(
    .pluck_num_active_features(pls_mod)$value,
    nrow(pls_mod$fit$loadings$X)
  )
  expect_equal(
    .pluck_num_parameters(pls_mod)$value,
    prod(dim(pls_mod$fit$loadings$X))
  )
  expect_equal(
    .pluck_num_active_features(spls_mod)$value,
    sum(!apply(spls_mod$fit$loadings$X, 1, function(x) all(x == 0)))
  )
  expect_equal(
    .pluck_num_parameters(spls_mod)$value,
    sum(spls_mod$fit$loadings$X != 0)
  )


  # classification
  expect_snapshot(characterize(plsda_mod))
  expect_equal(
    .pluck_num_active_features(plsda_mod)$value,
    nrow(plsda_mod$fit$loadings$X)
  )
  expect_equal(
    .pluck_num_parameters(plsda_mod)$value,
    prod(dim(plsda_mod$fit$loadings$X)) + prod(dim(plsda_mod$fit$loadings$Y))
  )
  expect_equal(
    .pluck_num_active_features(splsda_mod)$value,
    sum(!apply(splsda_mod$fit$loadings$X, 1, function(x) all(x == 0)))
  )
  expect_equal(
    .pluck_num_parameters(splsda_mod)$value,
    sum(splsda_mod$fit$loadings$X != 0) + sum(splsda_mod$fit$loadings$Y != 0)
  )
})
