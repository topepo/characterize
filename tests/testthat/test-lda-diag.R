test_that("lda_diag - binary classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_lda_diag)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_cls_lda_diag)$value[[1]],
    sort(fit_cls_lda_diag$col_names)
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_lda_diag)$value,
    length(fit_cls_lda_diag$col_names)
  )
})

test_that("lda_diag - multinomial classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_mnl_lda_diag)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_mnl_lda_diag)$value[[1]],
    sort(fit_mnl_lda_diag$col_names)
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_lda_diag)$value,
    length(fit_mnl_lda_diag$col_names)
  )
})
