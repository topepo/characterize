test_that("glm - regression", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_glm)
  check_characterize_object(chr_res)

  coefs <- coef(fit_reg_glm)
  # expect_equal(
  #   .pluck_num_parameters(fit_reg_glm)$value,
  #   length(coefs)
  # )

  act_vars <- names(coefs)
  act_vars <- sort(act_vars[!grepl("(Intercept)|(unused)", act_vars)])

  expect_equal(
    .pluck_features_active(fit_reg_glm)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_glm)$value,
    length(act_vars)
  )
  expect_equal(
    .pluck_num_features_input(fit_reg_glm)$value,
    ncol(attr(fit_reg_glm$terms, "factors"))
  )
})

test_that("glm - classification", {
  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_glm)
  check_characterize_object(chr_res)

  coefs <- coef(fit_cls_glm)
  # expect_equal(
  #   .pluck_num_parameters(fit_cls_glm)$value,
  #   length(coefs)
  # )

  act_vars <- names(coefs)
  act_vars <- sort(act_vars[!grepl("(Intercept)|(unused)", act_vars)])

  expect_equal(
    .pluck_features_active(fit_cls_glm)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_glm)$value,
    length(act_vars)
  )
  expect_equal(
    .pluck_num_features_input(fit_cls_glm)$value,
    ncol(attr(fit_cls_glm$terms, "factors"))
  )
})
