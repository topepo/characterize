test_that("ctree - regression", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(partykit))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_reg_ctree <-
    ctree(Sale_Price ~ ., data = ames) %>%
    butcher()

  chr_res <- characterize(fit_reg_ctree)
  check_characterize_object(chr_res)

  used <- unique(characterize:::get_party_var_index(fit_reg_ctree))
  used <- names(fit_reg_ctree$data)[used]

  expect_equal(
    .pluck_features_active(fit_reg_ctree)$value[[1]],
    sort(unique(used))
  )
  expect_equal(
    .pluck_num_features_active(fit_reg_ctree)$value,
    length(unique(used))
  )

  terminal <- length(nodeids(fit_reg_ctree, terminal = TRUE))

  expect_equal(
    .pluck_num_term_nodes(fit_reg_ctree)$value,
    terminal
  )
})

test_that("ctree - binary classification", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(partykit))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_cls_ctree <-
    ctree(class ~ ., data = cls_dat) %>%
    butcher()

  chr_res <- characterize(fit_cls_ctree)
  check_characterize_object(chr_res)



  used <- unique(characterize:::get_party_var_index(fit_cls_ctree))
  used <- names(fit_cls_ctree$data)[used]

  expect_equal(
    .pluck_features_active(fit_cls_ctree)$value[[1]],
    sort(unique(used))
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_ctree)$value,
    length(unique(used))
  )

  terminal <- length(nodeids(fit_cls_ctree, terminal = TRUE))

  expect_equal(
    .pluck_num_term_nodes(fit_cls_ctree)$value,
    terminal
  )

})

test_that("ctree - multinomial classification", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(partykit))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_mnl_ctree <-
    ctree(class ~ ., data = mnl_dat)

  chr_res <- characterize(fit_mnl_ctree)
  check_characterize_object(chr_res)



  used <- unique(characterize:::get_party_var_index(fit_mnl_ctree))
  used <- names(fit_mnl_ctree$data)[used]

  expect_equal(
    .pluck_features_active(fit_mnl_ctree)$value[[1]],
    sort(unique(used))
  )
  expect_equal(
    .pluck_num_features_active(fit_mnl_ctree)$value,
    length(unique(used))
  )

  terminal <- length(nodeids(fit_mnl_ctree, terminal = TRUE))

  expect_equal(
    .pluck_num_term_nodes(fit_mnl_ctree)$value,
    terminal
  )

})

