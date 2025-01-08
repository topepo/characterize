test_that("spls - regression", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_spls)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_reg_spls)$value[[1]],
    non_zero_load(fit_reg_spls)
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_spls)$value,
    length(non_zero_load(fit_reg_spls))
  )
  # expect_equal(
  #   .pluck_num_features_input(fit_reg_spls)$value,
  #   names(fit_reg_spls$X)
  # )
  expect_equal(
    .pluck_num_parameters(fit_reg_spls)$value,
    sum(!(unlist(fit_reg_spls$loadings) %in% c(0.0, 1.0)))
  )
})
