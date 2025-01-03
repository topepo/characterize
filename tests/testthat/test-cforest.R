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

  bag_chr <- characterize(fit_reg_cforest)
  check_characterize_object(bag_chr)

  # TODO more

  exp_reg_cforest <- list()

  terminal <-
    purrr::map(fit_reg_cforest$nodes, ~ nodeids(.x, terminal = TRUE)) %>%
    map_int(length) %>%
    sum()
  exp_reg_cforest$num_term_nodes <- terminal

  used <-
    purrr::map(fit_reg_cforest$nodes, characterize:::get_party_var_index) %>%
    unlist() %>%
    unique()
  used <- names(fit_reg_cforest$data)[used]

  exp_reg_cforest$features_active <- used
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

  bag_chr <- characterize(fit_cls_cforest)
  check_characterize_object(bag_chr)

  exp_cls_cforest <- list()

  terminal <-
    purrr::map(fit_cls_cforest$nodes, ~ nodeids(.x, terminal = TRUE)) %>%
    map_int(length) %>%
    sum()
  exp_cls_cforest$num_term_nodes <- terminal

  used <-
    purrr::map(fit_cls_cforest$nodes, characterize:::get_party_var_index) %>%
    unlist() %>%
    unique()
  used <- names(fit_cls_cforest$data)[used]

  exp_cls_cforest$features_active <- used

  # TODO more
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

  bag_chr <- characterize(fit_mnl_cforest)
  check_characterize_object(bag_chr)

  # TODO more
})

