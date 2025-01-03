test_that("lda - binary classification", {

  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_cls_lda)
  check_characterize_object(bag_chr)

  expect_equal(
    .pluck_features_active(fit_cls_lda)$value[[1]],
    sort(colnames(attr(fit_cls_lda$terms, "factors")))
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_lda)$value,
    ncol(attr(fit_cls_lda$terms, "factors"))
  )
  expect_equal(
    .pluck_num_features_input(fit_cls_lda)$value,
    ncol(attr(fit_cls_lda$terms, "factors"))
  )
})

test_that("lda - multinomial classification", {

  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_mnl_lda)
  check_characterize_object(bag_chr)

  expect_equal(
    .pluck_features_active(fit_mnl_lda)$value[[1]],
    sort(colnames(fit_mnl_lda$means))
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_lda)$value,
    ncol(fit_mnl_lda$means)
  )
  # bug; not working for non-formula interface
  # expect_equal(
  #   .pluck_num_features_input(fit_mnl_lda)$value,
  #   ncol(fit_mnl_lda$means)
  # )
})
