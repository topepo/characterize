test_that("rda - binary classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_rda)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_cls_rda)$value[[1]],
    sort(colnames(attr(fit_cls_rda$terms, "factors")))
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_rda)$value,
    ncol(attr(fit_cls_rda$terms, "factors"))
  )
  expect_equal(
    .pluck_num_features_input(fit_cls_rda)$value,
    ncol(attr(fit_cls_rda$terms, "factors"))
  )
})

test_that("rda - multinomial classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_mnl_rda)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_mnl_rda)$value[[1]],
    sort(rownames(fit_mnl_rda$means))
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_rda)$value,
    nrow(fit_mnl_rda$means)
  )
  # bug; not working for non-formula interface
  # expect_equal(
  #   .pluck_num_features_input(fit_mnl_rda)$value,
  #   ncol(fit_mnl_rda$means)
  # )
})
