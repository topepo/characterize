test_that("NaiveBayes - binary classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_NaiveBayes)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_cls_NaiveBayes)$value[[1]],
    sort(fit_cls_NaiveBayes$varnames)
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_NaiveBayes)$value,
    length(fit_cls_NaiveBayes$varnames)
  )
})

test_that("NaiveBayes - multinomial classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_mnl_NaiveBayes)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_mnl_NaiveBayes)$value[[1]],
    sort(fit_mnl_NaiveBayes$varnames)
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_NaiveBayes)$value,
    length(fit_mnl_NaiveBayes$varnames)
  )
})

