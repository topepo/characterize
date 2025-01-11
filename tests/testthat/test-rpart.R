test_that("rpart - regression", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(rpart))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_reg_rpart <-
    rpart(Sale_Price ~ ., data = ames) %>%
    butcher()

  chr_res <- characterize(fit_reg_rpart)
  check_characterize_object(chr_res)

  input_vars <- terms_vars(fit_reg_rpart)

  expect_equal(
    .pluck_num_features_input(fit_reg_rpart)$value,
    length(input_vars)
  )

  expect_equal(
    .pluck_features_active(fit_reg_rpart)$value[[1]],
    sort(unique(rpart_active_vars(fit_reg_rpart)))
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_rpart)$value,
    length(unique(rpart_active_vars(fit_reg_rpart)))
  )

  expect_equal(
    .pluck_num_term_nodes(fit_reg_rpart)$value,
    rpart_term_nodes(fit_reg_rpart)
  )
})

test_that("rpart - binary classification", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(rpart))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_cls_rpart <-
    rpart(class ~ ., data = cls_dat) %>%
    butcher()

  chr_res <- characterize(fit_cls_rpart)
  check_characterize_object(chr_res)

  input_vars <- terms_vars(fit_cls_rpart)

  expect_equal(
    .pluck_num_features_input(fit_cls_rpart)$value,
    length(input_vars)
  )

  expect_equal(
    .pluck_features_active(fit_cls_rpart)$value[[1]],
    sort(unique(rpart_active_vars(fit_cls_rpart)))
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_rpart)$value,
    length(unique(rpart_active_vars(fit_cls_rpart)))
  )

  expect_equal(
    .pluck_num_term_nodes(fit_cls_rpart)$value,
    rpart_term_nodes(fit_cls_rpart)
  )
})

test_that("rpart - multinomial classification", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(rpart))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_mnl_rpart <-
    rpart(class ~ ., data = mnl_dat) %>%
    butcher()

  chr_res <- characterize(fit_mnl_rpart)
  check_characterize_object(chr_res)

  input_vars <- terms_vars(fit_mnl_rpart)

  expect_equal(
    .pluck_num_features_input(fit_mnl_rpart)$value,
    length(input_vars)
  )

  expect_equal(
    .pluck_features_active(fit_mnl_rpart)$value[[1]],
    sort(unique(rpart_active_vars(fit_mnl_rpart)))
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_rpart)$value,
    length(unique(rpart_active_vars(fit_mnl_rpart)))
  )

  expect_equal(
    .pluck_num_term_nodes(fit_mnl_rpart)$value,
    rpart_term_nodes(fit_mnl_rpart)
  )
})

