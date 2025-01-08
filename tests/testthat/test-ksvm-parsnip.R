
test_that("ksvm - via parsnip - multinomial classification", {
  skip_if_not_installed("kernlab")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(kernlab))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_mnl_ksvm_spec <-
    svm_poly(degree = 3) %>%
    set_mode("classification")

  fit_mnl_ksvm_parsnip <-
    fit_mnl_ksvm_spec %>%
    fit(class ~ ., data = mnl_dat) %>%
    butcher()

  chr_res <- characterize(fit_mnl_ksvm_parsnip)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_num_support_vectors(fit_mnl_ksvm_parsnip)$value,
    length(unique(unlist(fit_mnl_ksvm_parsnip$fit@alphaindex)))
  )
  expect_equal(
    .pluck_num_features_input(fit_mnl_ksvm_parsnip)$value,
    ncol(attr(fit_mnl_ksvm_parsnip$fit@terms, "factors"))
  )
})

