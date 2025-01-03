
test_that("zeroinfl - classification", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_reg_zeroinfl)
  check_characterize_object(bag_chr)

  coefs <- unlist(fit_reg_zeroinfl$coefficients)
  # expect_equal(
  #   .pluck_num_parameters(fit_reg_zeroinfl)$value,
  #   length(coefs)
  # )

  act_vars <- names(coefs)
  act_vars <- gsub("(^count\\.)|(^zero\\.)", "", act_vars)
  act_vars <- unique(act_vars)
  act_vars <- sort(act_vars[!grepl("Intercept", act_vars)])

  expect_equal(
    .pluck_features_active(fit_reg_zeroinfl)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_zeroinfl)$value,
    length(act_vars)
  )
  # expect_equal(
  #   .pluck_num_features_input(fit_reg_zeroinfl)$value,
  #   ncol(attr(fit_reg_zeroinfl$terms, "factors"))
  # )
})
