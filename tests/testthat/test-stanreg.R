test_that("stanreg - regression", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_stan_glm)
  check_characterize_object(chr_res)

  act_vars <- terms_vars(fit_reg_stan_glm)

  # TODO this should probably be distinct variables; same for glm
  expect_equal(
    .pluck_features_active(fit_reg_stan_glm)$value[[1]],
    sort(names(fit_reg_stan_glm$coefficients)[-1])
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_stan_glm)$value,
    length(fit_reg_stan_glm$coefficients) - 1
  )
  expect_equal(
    .pluck_num_features_input(fit_reg_stan_glm)$value,
    length(act_vars)
  )
  # TODO Add parameters
})

test_that("stanreg - classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_stan_glm)
  check_characterize_object(chr_res)

  act_vars <- terms_vars(fit_cls_stan_glm)

  expect_equal(
    .pluck_features_active(fit_cls_stan_glm)$value[[1]],
    sort(names(fit_cls_stan_glm$coefficients)[-1])
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_stan_glm)$value,
    length(fit_cls_stan_glm$coefficients) - 1
  )
  expect_equal(
    .pluck_num_features_input(fit_cls_stan_glm)$value,
    length(act_vars)
  )
  # TODO Add parameters
})

