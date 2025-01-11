test_that("naive_bayes - binary classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_naive_bayes)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_cls_naive_bayes)$value[[1]],
    sort(names(fit_cls_naive_bayes$tables))
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_naive_bayes)$value,
    length(fit_cls_naive_bayes$tables)
  )
})

test_that("naive_bayes - multinomial classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_mnl_naive_bayes)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_mnl_naive_bayes)$value[[1]],
    sort(names(fit_mnl_naive_bayes$tables))
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_naive_bayes)$value,
    length(fit_mnl_naive_bayes$tables)
  )
})

