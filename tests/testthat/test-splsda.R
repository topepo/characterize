test_that("splsda - classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_splsda)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_cls_splsda)$value[[1]],
    non_zero_load(fit_cls_splsda)
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_splsda)$value,
    length(non_zero_load(fit_cls_splsda))
  )
  # expect_equal(
  #   .pluck_num_features_input(fit_cls_splsda)$value,
  #   names(fit_cls_splsda$X)
  # )
  expect_equal(
    .pluck_num_parameters(fit_cls_splsda)$value,
    sum(!(unlist(fit_cls_splsda$loadings) %in% c(0.0, 1.0)))
  )
})
