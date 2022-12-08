library(testthat)

test_that("method descriptions", {
  expect_snapshot(as.data.frame(list_characteristics()))
  expect_snapshot(object_list())
})
