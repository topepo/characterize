test_that("bagging - C5.0", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_cls_bag_c5)
  check_characterize_object(bag_chr)

  # Computed in "inst/test_objects.R"
  expect_equal(
    .pluck_num_features_active(fit_cls_bag_c5)$value,
    exp_cls_bag_c5$num_features_active
  )
  expect_equal(
    .pluck_features_active(fit_cls_bag_c5)$value[[1]],
    exp_cls_bag_c5$features_active
  )
  expect_equal(
    .pluck_num_term_nodes(fit_cls_bag_c5)$value[[1]],
    exp_cls_bag_c5$num_term_nodes
  )
})


test_that("bagging - rpart - regression", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_reg_bag_rpart)
  check_characterize_object(bag_chr)

  num_nodes <- map_int(fit_reg_bag_rpart$model_df$model, rpart_term_nodes)
  expect_equal(
    .pluck_num_term_nodes(fit_reg_bag_rpart)$value,
    sum(num_nodes)
  )

  expect_equal(
    .pluck_num_features_input(fit_reg_bag_rpart)$value,
    3L
  )

  act_vars <- purrr::map(fit_reg_bag_rpart$model_df$model, rpart_active_vars)
  act_vars <- sort(unique(unlist(act_vars)))

  expect_equal(
    .pluck_features_active(fit_reg_bag_rpart)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_bag_rpart)$value,
    length(act_vars)
  )
})


test_that("bagging - rpart - regression", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_reg_bag_rpart)
  check_characterize_object(bag_chr)

  num_nodes <- map_int(fit_reg_bag_rpart$model_df$model, rpart_term_nodes)
  expect_equal(
    .pluck_num_term_nodes(fit_reg_bag_rpart)$value,
    sum(num_nodes)
  )

  expect_equal(
    .pluck_num_features_input(fit_reg_bag_rpart)$value,
    3L
  )

  act_vars <- purrr::map(fit_reg_bag_rpart$model_df$model, rpart_active_vars)
  act_vars <- sort(unique(unlist(act_vars)))

  expect_equal(
    .pluck_features_active(fit_reg_bag_rpart)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_bag_rpart)$value,
    length(act_vars)
  )
})


test_that("bagging - rpart - classification", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_cls_bag_rpart)
  check_characterize_object(bag_chr)

  num_nodes <- map_int(fit_cls_bag_rpart$model_df$model, rpart_term_nodes)
  expect_equal(
    .pluck_num_term_nodes(fit_cls_bag_rpart)$value,
    sum(num_nodes)
  )

  expect_equal(
    .pluck_num_features_input(fit_cls_bag_rpart)$value,
    15L
  )

  act_vars <- purrr::map(fit_cls_bag_rpart$model_df$model, rpart_active_vars)
  act_vars <- sort(unique(unlist(act_vars)))

  expect_equal(
    .pluck_features_active(fit_cls_bag_rpart)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_bag_rpart)$value,
    length(act_vars)
  )
})


test_that("bagging - earth - regression", {
  skip_if_not_installed("earth")
  library(earth)

  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_reg_bag_mars)
  check_characterize_object(bag_chr)

  coefs <- purrr::map(fit_reg_bag_mars$model_df$model, ~ .x$fit$coefficients)
  expect_equal(
    .pluck_num_parameters(fit_reg_bag_mars)$value,
    sum(map_int(coefs, nrow))
  )

  expect_equal(
    .pluck_num_features_input(fit_reg_bag_mars)$value,
    3L
  )

  act_vars <-
    purrr::map(coefs, rownames) %>%
    unlist() %>%
    purrr::map(expr_vars) %>%
    unlist() %>%
    unique()

  act_vars <- sort(act_vars[!grepl("Intercept", act_vars)])

  expect_equal(
    .pluck_features_active(fit_reg_bag_mars)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_bag_mars)$value,
    length(act_vars)
  )
})



test_that("bagging - earth - classification", {
  skip_if_not_installed("earth")
  library(earth)

  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_cls_bag_mars)
  check_characterize_object(bag_chr)

  coefs <- purrr::map(fit_cls_bag_mars$model_df$model, ~ .x$fit$glm.coefficients)
  expect_equal(
    .pluck_num_parameters(fit_cls_bag_mars)$value,
    sum(map_int(coefs, nrow))
  )

  expect_equal(
    .pluck_num_features_input(fit_cls_bag_mars)$value,
    15L
  )

  act_vars <-
    purrr::map(coefs, rownames) %>%
    unlist() %>%
    purrr::map(expr_vars) %>%
    unlist() %>%
    unique()

  act_vars <- sort(act_vars[!grepl("Intercept", act_vars)])

  expect_equal(
    .pluck_features_active(fit_cls_bag_mars)$value[[1]],
    act_vars
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_bag_mars)$value,
    length(act_vars)
  )
})

test_that("bagging - nnet - regression", {
  # tests objects in "test_cases.RData"
  skip("not implemented in characterize yet")

  bag_chr <- characterize(fit_reg_bag_nnet)
  check_characterize_object(bag_chr)

  coefs <- purrr::map(fit_reg_bag_nnet$model_df$model, ~ .x$fit$wts)
  expect_equal(
    .pluck_num_parameters(fit_reg_bag_nnet)$value,
    sum(map_int(coefs, nrow))
  )

  expect_equal(
    .pluck_num_features_input(fit_reg_bag_nnet)$value,
    20L
  )

  expect_equal(
    .pluck_num_features_active(fit_reg_bag_nnet)$value,
    20L
  )
})



test_that("bagging - nnet - classification", {
  # tests objects in "test_cases.RData"
  skip("not implemented in characterize yet")

  bag_chr <- characterize(fit_cls_bag_nnet)
  check_characterize_object(bag_chr)

  coefs <- purrr::map(fit_cls_bag_nnet$model_df$model, ~ .x$fit$wts)
  expect_equal(
    .pluck_num_parameters(fit_cls_bag_nnet)$value,
    sum(map_int(coefs, nrow))
  )

  expect_equal(
    .pluck_num_features_input(fit_cls_bag_nnet)$value,
    20L
  )

  expect_equal(
    .pluck_num_features_active(fit_cls_bag_nnet)$value,
    20L
  )
})

