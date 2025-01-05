test_that("ksvm - regression", {
  skip_if_not_installed("kernlab")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(kernlab))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_reg_ksvm <-
    ksvm(Sale_Price ~ Neighborhood + Longitude, data = ames) %>%
    butcher()

  chr_res <- characterize(fit_reg_ksvm)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_num_support_vectors(fit_reg_ksvm)$value,
    length(unique(unlist(fit_reg_ksvm@alphaindex)))
  )
  expect_equal(
    .pluck_num_features_input(fit_reg_ksvm)$value,
    ncol(attr(fit_reg_ksvm@terms, "factors"))
  )
})

test_that("ksvm - binary classification", {
  skip_if_not_installed("kernlab")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(kernlab))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_cls_ksvm <-
    ksvm(class ~ ., data = cls_dat, family = binomial) %>%
    butcher()

  chr_res <- characterize(fit_cls_ksvm)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_num_support_vectors(fit_cls_ksvm)$value,
    length(unique(unlist(fit_cls_ksvm@alphaindex)))
  )
  expect_equal(
    .pluck_num_features_input(fit_cls_ksvm)$value,
    ncol(attr(fit_cls_ksvm@terms, "factors"))
  )
})

test_that("ksvm - multinomial classification", {
  skip_if_not_installed("kernlab")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(kernlab))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_mnl_ksvm <-
    ksvm(class ~ ., data = mnl_dat, family = binomial) %>%
    butcher()

  chr_res <- characterize(fit_mnl_ksvm)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_num_support_vectors(fit_mnl_ksvm)$value,
    length(unique(unlist(fit_mnl_ksvm@alphaindex)))
  )
  expect_equal(
    .pluck_num_features_input(fit_mnl_ksvm)$value,
    ncol(attr(fit_mnl_ksvm@terms, "factors"))
  )
})

