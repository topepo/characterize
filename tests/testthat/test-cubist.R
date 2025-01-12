test_that("Cubist - single rule set", {
  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_cb)
  check_characterize_object(chr_res)

  expect_snapshot(error = TRUE,
                  characterize(fit_reg_cb, committees = 1:2)
  )

  # Computed in "inst/test_objects.R"
  expect_equal(
    .pluck_num_features_active(fit_reg_cb)$value,
    exp_reg_cb$num_features_active
  )
  expect_equal(
    .pluck_features_active(fit_reg_cb)$value[[1]],
    exp_reg_cb$features_active
  )
  expect_equal(
    .pluck_num_rules(fit_reg_cb)$value,
    exp_reg_cb$num_rules
  )
  expect_equal(
    .pluck_mean_rule_size(fit_reg_cb)$value,
    exp_reg_cb$mean_rule_size
  )
})

test_that("Cubist - ensemble", {
  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_cb_ens)
  check_characterize_object(chr_res)

  # Computed in "inst/test_objects.R"
  expect_equal(
    .pluck_num_features_active(fit_reg_cb_ens)$value,
    exp_reg_cb_ens$num_features_active
  )
  expect_equal(
    .pluck_features_active(fit_reg_cb_ens)$value[[1]],
    exp_reg_cb_ens$features_active
  )
  expect_equal(
    .pluck_num_rules(fit_reg_cb_ens)$value,
    exp_reg_cb_ens$num_rules
  )
  expect_equal(
    .pluck_mean_rule_size(fit_reg_cb_ens)$value,
    exp_reg_cb_ens$mean_rule_size
  )
})

test_that("Cubist - empty rule", {
  skip_if_not_installed("Cubist")
  suppressPackageStartupMessages(library(Cubist))
  cb_mod <- cubist(mtcars[,-1], mtcars$mpg)
  expect_equal(
    .pluck_mean_rule_size(cb_mod)$value,
    0L
  )
})


test_that("Cubist - multi_characterize", {
  # tests objects in "test_cases.RData"

  vals <- 1:3

  res <- multi_characterize(fit_reg_cb_10)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  expect_named(res, c("committees", "results"))

  res <- multi_characterize(fit_reg_cb_10, committees = vals)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3L)
  expect_named(res, c("committees", "results"))

  for (i in seq_along(vals)) {
    expt <- characterize(fit_reg_cb_10, committees = vals[i])
    expect_equal(res$results[[i]], expt)
    expect_equal(res$committees[i], vals[i])
  }

  # test overall generic for failed models
  bad_mod <- fit_reg_cb_10
  class(bad_mod) <- c("try-error", class(bad_mod))
  expect_snapshot_warning(bad_res <- multi_characterize(bad_mod))
  expect_null(bad_res)
})

