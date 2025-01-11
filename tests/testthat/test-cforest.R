test_that("cforest - regression", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(partykit))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_reg_cforest <-
    cforest(Sale_Price ~ ., data = ames, ntree = 5) %>%
    butcher()

  chr_res <- characterize(fit_reg_cforest)
  check_characterize_object(chr_res)

  input_vars <- terms_vars(fit_reg_cforest)

  expect_equal(
    .pluck_num_features_input(fit_reg_cforest)$value,
    length(input_vars)
  )

  used <-
    purrr::map(fit_reg_cforest$nodes, characterize:::get_party_var_index) %>%
    unlist() %>%
    unique()
  used <- names(fit_reg_cforest$data)[used]

  expect_equal(
    .pluck_features_active(fit_reg_cforest)$value[[1]],
    sort(unique(used))
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_cforest)$value,
    length(unique(used))
  )

  terminal <-
    purrr::map(fit_reg_cforest$nodes, ~ nodeids(.x, terminal = TRUE)) %>%
    map_int(length) %>%
    sum()

  expect_equal(
    .pluck_num_term_nodes(fit_reg_cforest)$value,
    terminal
  )
})

test_that("cforest - binary classification", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(partykit))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_cls_cforest <-
    cforest(class ~ ., data = cls_dat, ntree = 5) %>%
    butcher()

  chr_res <- characterize(fit_cls_cforest)
  check_characterize_object(chr_res)


  input_vars <- terms_vars(fit_cls_cforest)

  expect_equal(
    .pluck_num_features_input(fit_cls_cforest)$value,
    length(input_vars)
  )

  used <-
    purrr::map(fit_cls_cforest$nodes, characterize:::get_party_var_index) %>%
    unlist() %>%
    unique()
  used <- names(fit_cls_cforest$data)[used]

  expect_equal(
    .pluck_features_active(fit_cls_cforest)$value[[1]],
    sort(unique(used))
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_cforest)$value,
    length(unique(used))
  )

  terminal <-
    purrr::map(fit_cls_cforest$nodes, ~ nodeids(.x, terminal = TRUE)) %>%
    map_int(length) %>%
    sum()

  expect_equal(
    .pluck_num_term_nodes(fit_cls_cforest)$value,
    terminal
  )
})

test_that("cforest - multinomial classification", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(partykit))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_mnl_cforest <-
    cforest(class ~ ., data = mnl_dat, ntree = 5)

  chr_res <- characterize(fit_mnl_cforest)
  check_characterize_object(chr_res)


  input_vars <- terms_vars(fit_mnl_cforest)

  expect_equal(
    .pluck_num_features_input(fit_mnl_cforest)$value,
    length(input_vars)
  )

  used <-
    purrr::map(fit_mnl_cforest$nodes, characterize:::get_party_var_index) %>%
    unlist() %>%
    unique()
  used <- names(fit_mnl_cforest$data)[used]

  expect_equal(
    .pluck_features_active(fit_mnl_cforest)$value[[1]],
    sort(unique(used))
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_cforest)$value,
    length(unique(used))
  )

  terminal <-
    purrr::map(fit_mnl_cforest$nodes, ~ nodeids(.x, terminal = TRUE)) %>%
    map_int(length) %>%
    sum()

  expect_equal(
    .pluck_num_term_nodes(fit_mnl_cforest)$value,
    terminal
  )
})

