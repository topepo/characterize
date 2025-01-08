test_that("rpart - regression", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(rpart))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_reg_rpart <-
    rpart(Sale_Price ~ ., data = ames) %>%
    butcher()

  chr_res <- characterize(fit_reg_rpart)
  check_characterize_object(chr_res)

  # TODO more
})

test_that("rpart - binary classification", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(rpart))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_cls_rpart <-
    rpart(class ~ ., data = cls_dat) %>%
    butcher()

  chr_res <- characterize(fit_cls_rpart)
  check_characterize_object(chr_res)

  # TODO more
})

test_that("rpart - multinomial classification", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("butcher")
  suppressPackageStartupMessages(library(rpart))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(butcher))

  fit_mnl_rpart <-
    rpart(class ~ ., data = mnl_dat) %>%
    butcher()

  chr_res <- characterize(fit_mnl_rpart)
  check_characterize_object(chr_res)

  # TODO more
})

