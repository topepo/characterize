test_that("glmnet - regression", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_reg_glmnet, penalty = 0.1)
  check_characterize_object(chr_res)
  expect_snapshot(error = TRUE,
                  characterize(fit_reg_glmnet, penalty = 1:2)
  )


  no_int <- exp_reg_glmnet %>% filter(!grepl("Intercept", term))
  num_terms <- no_int %>% count(lambda)
  num_param <- exp_reg_glmnet %>% count(lambda)

  ## penalty = 0.1

  expect_equal(
    .pluck_features_active(fit_reg_glmnet, penalty = 0.1)$value[[1]],
    exp_reg_glmnet %>%
      filter(lambda == 0.1 & !grepl("Intercept", term)) %>%
      arrange(term) %>%
      pluck("term")
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_glmnet, penalty = 0.1)$value,
    num_terms %>%
      filter(lambda == 0.1) %>%
      pluck("n")
  )
  expect_equal(
    .pluck_num_features_input(fit_reg_glmnet, penalty = 0.1)$value,
    length(rownames(fit_reg_glmnet$beta))
  )

  expect_equal(
    .pluck_num_parameters(fit_reg_glmnet, penalty = 0.1)$value,
    num_param %>%
      filter(lambda == 0.1) %>%
      pluck("n")
  )

  ## penalty = 0.01

  expect_equal(
    .pluck_features_active(fit_reg_glmnet, penalty = 0.01)$value[[1]],
    exp_reg_glmnet %>%
      filter(lambda == 0.01 & !grepl("Intercept", term)) %>%
      arrange(term) %>%
      pluck("term")
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_glmnet, penalty = 0.01)$value,
    num_terms %>%
      filter(lambda == 0.01) %>%
      pluck("n")
  )
  expect_equal(
    .pluck_num_features_input(fit_reg_glmnet, penalty = 0.01)$value,
    length(rownames(fit_reg_glmnet$beta))
  )

  expect_equal(
    .pluck_num_parameters(fit_reg_glmnet, penalty = 0.01)$value,
    num_param %>%
      filter(lambda == 0.01) %>%
      pluck("n")
  )

  ## penalty = 0.001

  expect_equal(
    .pluck_features_active(fit_reg_glmnet, penalty = 0.001)$value[[1]],
    exp_reg_glmnet %>%
      filter(lambda == 0.001 & !grepl("Intercept", term)) %>%
      arrange(term) %>%
      pluck("term")
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_glmnet, penalty = 0.001)$value,
    num_terms %>%
      filter(lambda == 0.001) %>%
      pluck("n")
  )
  expect_equal(
    .pluck_num_features_input(fit_reg_glmnet, penalty = 0.001)$value,
    length(rownames(fit_reg_glmnet$beta))
  )

  expect_equal(
    .pluck_num_parameters(fit_reg_glmnet, penalty = 0.001)$value,
    num_param %>%
      filter(lambda == 0.001) %>%
      pluck("n")
  )
})

test_that("glmnet - binary classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_glmnet, penalty = 0.1)
  check_characterize_object(chr_res)

  no_int <- exp_cls_glmnet %>% filter(!grepl("Intercept", term))
  num_terms <- no_int %>% count(lambda)
  num_param <- exp_cls_glmnet %>% count(lambda)

  ## penalty = 0.1

  expect_equal(
    .pluck_features_active(fit_cls_glmnet, penalty = 0.1)$value[[1]],
    exp_cls_glmnet %>%
      filter(lambda == 0.1 & !grepl("Intercept", term)) %>%
      arrange(term) %>%
      pluck("term")
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_glmnet, penalty = 0.1)$value,
    num_terms %>%
      filter(lambda == 0.1) %>%
      pluck("n")
  )
  expect_equal(
    .pluck_num_features_input(fit_cls_glmnet, penalty = 0.1)$value,
    length(rownames(fit_cls_glmnet$beta))
  )

  expect_equal(
    .pluck_num_parameters(fit_cls_glmnet, penalty = 0.1)$value,
    num_param %>%
      filter(lambda == 0.1) %>%
      pluck("n")
  )

  ## penalty = 0.01

  expect_equal(
    .pluck_features_active(fit_cls_glmnet, penalty = 0.01)$value[[1]],
    exp_cls_glmnet %>%
      filter(lambda == 0.01 & !grepl("Intercept", term)) %>%
      arrange(term) %>%
      pluck("term")
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_glmnet, penalty = 0.01)$value,
    num_terms %>%
      filter(lambda == 0.01) %>%
      pluck("n")
  )
  expect_equal(
    .pluck_num_features_input(fit_cls_glmnet, penalty = 0.01)$value,
    length(rownames(fit_cls_glmnet$beta))
  )

  expect_equal(
    .pluck_num_parameters(fit_cls_glmnet, penalty = 0.01)$value,
    num_param %>%
      filter(lambda == 0.01) %>%
      pluck("n")
  )

  ## penalty = 0.001

  expect_equal(
    .pluck_features_active(fit_cls_glmnet, penalty = 0.001)$value[[1]],
    exp_cls_glmnet %>%
      filter(lambda == 0.001 & !grepl("Intercept", term)) %>%
      arrange(term) %>%
      pluck("term")
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_glmnet, penalty = 0.001)$value,
    num_terms %>%
      filter(lambda == 0.001) %>%
      pluck("n")
  )
  expect_equal(
    .pluck_num_features_input(fit_cls_glmnet, penalty = 0.001)$value,
    length(rownames(fit_cls_glmnet$beta))
  )

  expect_equal(
    .pluck_num_parameters(fit_cls_glmnet, penalty = 0.001)$value,
    num_param %>%
      filter(lambda == 0.001) %>%
      pluck("n")
  )
})

test_that("glmnet - multinomial classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_mnl_glmnet, penalty = 0.1)
  check_characterize_object(chr_res)

  no_int <- exp_mnl_glmnet %>% filter(term != "")
  num_terms <- no_int %>% count(lambda)
  num_param <- exp_mnl_glmnet %>% count(lambda)

  ## penalty = 0.1

  expect_equal(
    .pluck_features_active(fit_mnl_glmnet, penalty = 0.1)$value[[1]],
    exp_mnl_glmnet %>%
      filter(lambda == 0.1 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique()
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_glmnet, penalty = 0.1)$value,
    exp_mnl_glmnet %>%
      filter(lambda == 0.1 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique() %>%
      length()
  )
  expect_equal(
    .pluck_num_features_input(fit_mnl_glmnet, penalty = 0.1)$value,
    length(rownames(fit_mnl_glmnet$beta[[1]]))
  )

  expect_equal(
    .pluck_num_parameters(fit_mnl_glmnet, penalty = 0.1)$value,
    num_param %>%
      filter(lambda == 0.1) %>%
      pluck("n")
  )

  ###

  ## penalty = 0.01

  expect_equal(
    .pluck_features_active(fit_mnl_glmnet, penalty = 0.01)$value[[1]],
    exp_mnl_glmnet %>%
      filter(lambda == 0.01 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique()
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_glmnet, penalty = 0.01)$value,
    exp_mnl_glmnet %>%
      filter(lambda == 0.01 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique() %>%
      length()
  )
  expect_equal(
    .pluck_num_features_input(fit_mnl_glmnet, penalty = 0.01)$value,
    length(rownames(fit_mnl_glmnet$beta[[1]]))
  )

  expect_equal(
    .pluck_num_parameters(fit_mnl_glmnet, penalty = 0.01)$value,
    num_param %>%
      filter(lambda == 0.01) %>%
      pluck("n")
  )

  ###

  ## penalty = 0.001

  expect_equal(
    .pluck_features_active(fit_mnl_glmnet, penalty = 0.001)$value[[1]],
    exp_mnl_glmnet %>%
      filter(lambda == 0.001 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique()
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_glmnet, penalty = 0.001)$value,
    exp_mnl_glmnet %>%
      filter(lambda == 0.001 & term != "") %>%
      arrange(term) %>%
      pluck("term") %>%
      unique() %>%
      length()
  )
  expect_equal(
    .pluck_num_features_input(fit_mnl_glmnet, penalty = 0.001)$value,
    length(rownames(fit_mnl_glmnet$beta[[1]]))
  )

  expect_equal(
    .pluck_num_parameters(fit_mnl_glmnet, penalty = 0.001)$value,
    num_param %>%
      filter(lambda == 0.001) %>%
      pluck("n")
  )
})


