test_that("nnet - mutinomial classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_mnl_nnet)
  check_characterize_object(chr_res)

  act_vars <- terms_vars(fit_mnl_nnet)

  expect_equal(
    .pluck_num_features_input(fit_mnl_nnet)$value,
    length(act_vars)
  )

  expect_equal(
    .pluck_num_parameters(fit_mnl_nnet)$value,
    sum(fit_mnl_nnet$wts != 0)
  )
})
