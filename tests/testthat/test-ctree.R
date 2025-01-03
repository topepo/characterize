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

  bag_chr <- characterize(fit_reg_ctree)
  check_characterize_object(bag_chr)

  # TODO more
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

  bag_chr <- characterize(fit_cls_ctree)
  check_characterize_object(bag_chr)

  # TODO more
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

  bag_chr <- characterize(fit_mnl_ctree)
  check_characterize_object(bag_chr)

  # TODO more
})

