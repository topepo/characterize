test_that("plsda - classification", {

  # tests objects in "test_cases.RData"

  chr_res <- characterize(fit_cls_plsda)
  check_characterize_object(chr_res)

  expect_equal(
    .pluck_features_active(fit_cls_plsda)$value[[1]],
    non_zero_load(fit_cls_plsda)
  )
  expect_equal(
    .pluck_num_features_active(fit_cls_plsda)$value,
    length(non_zero_load(fit_cls_plsda))
  )
  # expect_equal(
  #   .pluck_num_features_input(fit_cls_plsda)$value,
  #   names(fit_cls_plsda$X)
  # )
  expect_equal(
    .pluck_num_parameters(fit_cls_plsda)$value,
    sum(!(unlist(fit_cls_plsda$loadings) %in% c(0.0, 1.0)))
  )
})
