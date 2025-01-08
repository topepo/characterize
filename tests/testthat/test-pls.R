test_that("pls - regression", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_pls)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_reg_pls)$value[[1]],
    colnames(fit_reg_pls$X)
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_pls)$value,
    ncol(fit_reg_pls$X)
  )
  # expect_equal(
  #   .pluck_num_features_input(fit_reg_pls)$value,
  #   names(fit_reg_pls$X)
  # )
  expect_equal(
    .pluck_num_parameters(fit_reg_pls)$value,
    sum(!(unlist(fit_reg_pls$loadings) %in% c(0.0, 1.0)))
  )
})
