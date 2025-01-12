test_that("glmnet - via parsnip - regression", {
  skip_if_not_installed("glmnet")

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_glmnet_parsnip, penalty = 0.1)
  check_characterize_object(chr_res)

  no_int <- exp_reg_glmnet_parsnip %>% filter(!grepl("Intercept", term))
  num_terms <- no_int %>% count(lambda)
  num_param <- exp_reg_glmnet_parsnip %>% count(lambda)

  ## penalty = 0.1

  expect_equal(
    .pluck_features_active(fit_reg_glmnet_parsnip, penalty = 0.1)$value[[1]],
    exp_reg_glmnet_parsnip %>%
      filter(lambda == 0.1 & !grepl("Intercept", term)) %>%
      arrange(term) %>%
      pluck("term")
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_glmnet_parsnip, penalty = 0.1)$value,
    num_terms %>%
      filter(lambda == 0.1) %>%
      pluck("n")
  )
  # TODO
  # expect_equal(
  #   .pluck_num_features_input(fit_reg_glmnet_parsnip, penalty = 0.1)$value,
  #   length(rownames(fit_reg_glmnet_parsnip$fit$beta))
  # )

  expect_equal(
    .pluck_num_parameters(fit_reg_glmnet_parsnip, penalty = 0.1)$value,
    num_param %>%
      filter(lambda == 0.1) %>%
      pluck("n")
  )

  ## penalty = 0.01

  expect_equal(
    .pluck_features_active(fit_reg_glmnet_parsnip, penalty = 0.01)$value[[1]],
    exp_reg_glmnet_parsnip %>%
      filter(lambda == 0.01 & !grepl("Intercept", term)) %>%
      arrange(term) %>%
      pluck("term")
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_glmnet_parsnip, penalty = 0.01)$value,
    num_terms %>%
      filter(lambda == 0.01) %>%
      pluck("n")
  )
  # TODO
  # expect_equal(
  #   .pluck_num_features_input(fit_reg_glmnet_parsnip, penalty = 0.01)$value,
  #   length(rownames(fit_reg_glmnet_parsnip$beta))
  # )

  expect_equal(
    .pluck_num_parameters(fit_reg_glmnet_parsnip, penalty = 0.01)$value,
    num_param %>%
      filter(lambda == 0.01) %>%
      pluck("n")
  )

  ## penalty = 0.001

  expect_equal(
    .pluck_features_active(fit_reg_glmnet_parsnip, penalty = 0.001)$value[[1]],
    exp_reg_glmnet_parsnip %>%
      filter(lambda == 0.001 & !grepl("Intercept", term)) %>%
      arrange(term) %>%
      pluck("term")
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_glmnet_parsnip, penalty = 0.001)$value,
    num_terms %>%
      filter(lambda == 0.001) %>%
      pluck("n")
  )
  #TODO
  # expect_equal(
  #   .pluck_num_features_input(fit_reg_glmnet_parsnip, penalty = 0.001)$value,
  #   length(rownames(fit_reg_glmnet_parsnip$beta))
  # )

  expect_equal(
    .pluck_num_parameters(fit_reg_glmnet_parsnip, penalty = 0.001)$value,
    num_param %>%
      filter(lambda == 0.001) %>%
      pluck("n")
  )
})


test_that("glmnet - via parsnip - multinomial classification", {
  skip_if_not_installed("glmnet")

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_mnl_glmnet_parsnip, penalty = 0.1)
  check_characterize_object(chr_res)

  no_int <- exp_mnl_glmnet %>% filter(term != "")
  num_terms <- no_int %>% count(lambda)
  num_param <- exp_mnl_glmnet %>% count(lambda)

  ## penalty = 0.1

  expect_equal(
    .pluck_features_active(fit_mnl_glmnet_parsnip, penalty = 0.1)$value[[1]],
    exp_mnl_glmnet %>%
      filter(lambda == 0.1 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique()
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_glmnet_parsnip, penalty = 0.1)$value,
    exp_mnl_glmnet %>%
      filter(lambda == 0.1 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique() %>%
      length()
  )
  # TODO
  # expect_equal(
  #   .pluck_num_features_input(fit_mnl_glmnet_parsnip, penalty = 0.1)$value,
  #   length(rownames(fit_mnl_glmnet_parsnip$fit$beta[[1]]))
  # )

  expect_equal(
    .pluck_num_parameters(fit_mnl_glmnet_parsnip, penalty = 0.1)$value,
    num_param %>%
      filter(lambda == 0.1) %>%
      pluck("n")
  )

  ###

  ## penalty = 0.01

  expect_equal(
    .pluck_features_active(fit_mnl_glmnet_parsnip, penalty = 0.01)$value[[1]],
    exp_mnl_glmnet %>%
      filter(lambda == 0.01 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique()
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_glmnet_parsnip, penalty = 0.01)$value,
    exp_mnl_glmnet %>%
      filter(lambda == 0.01 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique() %>%
      length()
  )
  # TODO
  # expect_equal(
  #   .pluck_num_features_input(fit_mnl_glmnet_parsnip, penalty = 0.01)$value,
  #   length(rownames(fit_mnl_glmnet_parsnip$fit$beta[[1]]))
  # )

  expect_equal(
    .pluck_num_parameters(fit_mnl_glmnet_parsnip, penalty = 0.01)$value,
    num_param %>%
      filter(lambda == 0.01) %>%
      pluck("n")
  )

  ###

  ## penalty = 0.001

  expect_equal(
    .pluck_features_active(fit_mnl_glmnet_parsnip, penalty = 0.001)$value[[1]],
    exp_mnl_glmnet %>%
      filter(lambda == 0.001 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique()
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_glmnet_parsnip, penalty = 0.001)$value,
    exp_mnl_glmnet %>%
      filter(lambda == 0.001 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique() %>%
      length()
  )
  # TODO
  # expect_equal(
  #   .pluck_num_features_input(fit_mnl_glmnet_parsnip, penalty = 0.001)$value,
  #   length(rownames(fit_mnl_glmnet_parsnip$fit$beta[[1]]))
  # )

  expect_equal(
    .pluck_num_parameters(fit_mnl_glmnet_parsnip, penalty = 0.001)$value,
    num_param %>%
      filter(lambda == 0.001) %>%
      pluck("n")
  )
})

test_that("glmnet - via parsnip - multi_characterize", {
  skip_if_not_installed("bundle")
  skip_if_not_installed("glmnet")

  # tests objects in "test_cases.RData"

  vals <- c(0.001, 0.01, 0.1)

  res <- multi_characterize(fit_mnl_glmnet_parsnip, penalty = vals)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3L)
  expect_named(res, c("penalty", "results"))

  for (i in seq_along(vals)) {
    expt <- characterize(fit_mnl_glmnet_parsnip, penalty = vals[i])
    expect_equal(res$results[[i]], expt)
    expect_equal(res$penalty[i], vals[i])
  }

  # test for failed models
  bad_mod <- fit_mnl_glmnet_parsnip
  class(bad_mod) <- c("try-error", class(bad_mod))
  expect_snapshot_warning(bad_res <- multi_characterize(bad_mod))
  expect_null(bad_res)
})

