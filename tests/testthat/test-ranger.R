test_that("ranger - regression", {
  skip_if_not_installed("ranger")
  suppressPackageStartupMessages(library(ranger))

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_ranger)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_reg_ranger)$value[[1]],
    ranger_vars(fit_reg_ranger)
  )

  expect_equal(
    .pluck_num_features_active(fit_reg_ranger)$value[[1]],
    length(ranger_vars(fit_reg_ranger))
  )

  expect_equal(
    .pluck_num_term_nodes(fit_reg_ranger)$value,
    sum(unlist(fit_reg_ranger$forest$child.nodeIDs) == 0) / 2
  )
})

test_that("ranger - binary classification", {
  skip_if_not_installed("ranger")
  suppressPackageStartupMessages(library(ranger))

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_ranger)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_cls_ranger)$value[[1]],
    ranger_vars(fit_cls_ranger)
  )

  expect_equal(
    .pluck_num_features_active(fit_cls_ranger)$value[[1]],
    length(ranger_vars(fit_cls_ranger))
  )

  expect_equal(
    .pluck_num_term_nodes(fit_cls_ranger)$value,
    sum(unlist(fit_cls_ranger$forest$child.nodeIDs) == 0) / 2
  )
})

test_that("ranger - multinomial classification", {
  skip_if_not_installed("ranger")
  suppressPackageStartupMessages(library(ranger))

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_mnl_ranger)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_mnl_ranger)$value[[1]],
    ranger_vars(fit_mnl_ranger)
  )

  expect_equal(
    .pluck_num_features_active(fit_mnl_ranger)$value[[1]],
    length(ranger_vars(fit_mnl_ranger))
  )

  expect_equal(
    .pluck_num_term_nodes(fit_mnl_ranger)$value,
    sum(unlist(fit_mnl_ranger$forest$child.nodeIDs) == 0) / 2
  )
})

