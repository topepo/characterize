test_that("glmnet - via workflows - regression", {
  skip_if_not_installed("glmnet")

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_glmnet_wflow, penalty = 0.1)
  check_characterize_object(chr_res)

  no_int <- exp_reg_glmnet_wflow %>% filter(!grepl("Intercept", term))
  num_terms <- no_int %>% count(lambda)
  num_param <- exp_reg_glmnet_wflow %>% count(lambda)
  penalties <- fit_reg_glmnet_wflow$fit$fit$fit$lambda

  ## penalty = 0.1

  expect_equal(
    .pluck_features_active(fit_reg_glmnet_wflow, penalty = penalties[1])$value[[1]],
    exp_reg_glmnet_wflow %>%
      filter(lambda == penalties[1] & !grepl("Intercept", term)) %>%
      arrange(term) %>%
      pluck("term")
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_glmnet_wflow, penalty = penalties[1])$value,
    0L
  )
  # TODO
  # expect_equal(
  #   .pluck_num_features_input(fit_reg_glmnet_wflow, penalty = 0.1)$value,
  #   length(rownames(fit_reg_glmnet_wflow$fit$fit$fit$beta))
  # )

  expect_equal(
    .pluck_num_parameters(fit_reg_glmnet_wflow, penalty = penalties[1])$value,
    num_param %>%
      filter(lambda == penalties[1]) %>%
      pluck("n")
  )

  ## penalty = 0.01

  expect_equal(
    .pluck_features_active(fit_reg_glmnet_wflow, penalty = penalties[2])$value[[1]],
    exp_reg_glmnet_wflow %>%
      filter(lambda == penalties[2] & !grepl("Intercept", term)) %>%
      arrange(term) %>%
      pluck("term")
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_glmnet_wflow, penalty = penalties[2])$value,
    num_terms %>%
      filter(lambda == penalties[2]) %>%
      pluck("n")
  )
  # TODO
  # expect_equal(
  #   .pluck_num_features_input(fit_reg_glmnet_wflow, penalty = 0.01)$value,
  #   length(rownames(fit_reg_glmnet_wflow$fit$fit$fit$beta))
  # )

  expect_equal(
    .pluck_num_parameters(fit_reg_glmnet_wflow, penalty = penalties[2])$value,
    num_param %>%
      filter(lambda == penalties[2]) %>%
      pluck("n")
  )

  ## penalty = 0.001

  expect_equal(
    .pluck_features_active(fit_reg_glmnet_wflow, penalty = penalties[3])$value[[1]],
    exp_reg_glmnet_wflow %>%
      filter(lambda == penalties[3] & !grepl("Intercept", term)) %>%
      arrange(term) %>%
      pluck("term")
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_glmnet_wflow, penalty = penalties[3])$value,
    num_terms %>%
      filter(lambda == penalties[3]) %>%
      pluck("n")
  )
  # TODO
  # expect_equal(
  #   .pluck_num_features_input(fit_reg_glmnet_wflow, penalty = 0.001)$value,
  #   length(rownames(fit_reg_glmnet_wflow$fit$fit$fit$beta))
  # )

  expect_equal(
    .pluck_num_parameters(fit_reg_glmnet_wflow, penalty = penalties[3])$value,
    num_param %>%
      filter(lambda == penalties[3]) %>%
      pluck("n")
  )

})


test_that("glmnet - via workflows - multinomial classification", {
  skip_if_not_installed("glmnet")

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_mnl_glmnet_wflow, penalty = 0.1)
  check_characterize_object(chr_res)

  no_int <- exp_mnl_glmnet_wflow %>% filter(term != "")
  num_terms <- no_int %>% count(lambda)
  num_param <- exp_mnl_glmnet_wflow %>% count(lambda)
  penalties <- fit_mnl_glmnet_wflow$fit$fit$fit$lambda

  ## penalty = 0.1

  expect_equal(
    .pluck_features_active(fit_mnl_glmnet_wflow, penalty = 0.1)$value[[1]],
    exp_mnl_glmnet_wflow %>%
      filter(lambda == 0.1 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique()
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_glmnet_wflow, penalty = 0.1)$value,
    exp_mnl_glmnet_wflow %>%
      filter(lambda == 0.1 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique() %>%
      length()
  )
  # TODO
  # expect_equal(
  #   .pluck_num_features_input(fit_mnl_glmnet_wflow, penalty = 0.1)$value,
  #   length(rownames(fit_mnl_glmnet_wflow$fit$fit$fit$beta[[1]]))
  # )

  expect_equal(
    .pluck_num_parameters(fit_mnl_glmnet_wflow, penalty = 0.1)$value,
    num_param %>%
      filter(lambda == 0.1) %>%
      pluck("n")
  )

  ###

  ## penalty = 0.01

  expect_equal(
    .pluck_features_active(fit_mnl_glmnet_wflow, penalty = 0.01)$value[[1]],
    exp_mnl_glmnet_wflow %>%
      filter(lambda == 0.01 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique()
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_glmnet_wflow, penalty = 0.01)$value,
    exp_mnl_glmnet_wflow %>%
      filter(lambda == 0.01 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique() %>%
      length()
  )
  # TODO
  # expect_equal(
  #   .pluck_num_features_input(fit_mnl_glmnet_wflow, penalty = 0.01)$value,
  #   length(rownames(fit_mnl_glmnet_wflow$fit$fit$fit$beta[[1]]))
  # )

  expect_equal(
    .pluck_num_parameters(fit_mnl_glmnet_wflow, penalty = 0.01)$value,
    num_param %>%
      filter(lambda == 0.01) %>%
      pluck("n")
  )

  ###

  ## penalty = 0.001

  expect_equal(
    .pluck_features_active(fit_mnl_glmnet_wflow, penalty = 0.001)$value[[1]],
    exp_mnl_glmnet_wflow %>%
      filter(lambda == 0.001 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique()
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_glmnet_wflow, penalty = 0.001)$value,
    exp_mnl_glmnet_wflow %>%
      filter(lambda == 0.001 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique() %>%
      length()
  )
  # TODO
  # expect_equal(
  #   .pluck_num_features_input(fit_mnl_glmnet_wflow, penalty = 0.001)$value,
  #   length(rownames(fit_mnl_glmnet_wflow$fit$fit$fit$beta[[1]]))
  # )

  expect_equal(
    .pluck_num_parameters(fit_mnl_glmnet_wflow, penalty = 0.001)$value,
    num_param %>%
      filter(lambda == 0.001) %>%
      pluck("n")
  )
})


test_that("glmnet - via workflows - multi_characterize", {
  skip_if_not_installed("bundle")
  skip_if_not_installed("glmnet")

  # tests objects in "test_cases.RData"

  vals <- c(0.001, 0.01, 0.1)

  res <- multi_characterize(fit_mnl_glmnet_wflow, penalty = vals)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3L)
  expect_named(res, c("penalty", "results"))

  for (i in seq_along(vals)) {
    expt <- characterize(fit_mnl_glmnet_wflow, penalty = vals[i])
    expect_equal(res$results[[i]], expt)
    expect_equal(res$penalty[i], vals[i])
  }
})


