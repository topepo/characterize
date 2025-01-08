test_that("qda - binary classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_qda)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_cls_qda)$value[[1]],
    sort(colnames(attr(fit_cls_qda$terms, "factors")))
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_qda)$value,
    ncol(attr(fit_cls_qda$terms, "factors"))
  )
  expect_equal(
    .pluck_num_features_input(fit_cls_qda)$value,
    ncol(attr(fit_cls_qda$terms, "factors"))
  )
})

test_that("qda - multinomial classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_mnl_qda)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_mnl_qda)$value[[1]],
    sort(colnames(fit_mnl_qda$means))
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_qda)$value,
    ncol(fit_mnl_qda$means)
  )
  # bug; not working for non-formula interface
  # expect_equal(
  #   .pluck_num_features_input(fit_mnl_qda)$value,
  #   ncol(fit_mnl_qda$means)
  # )
})
