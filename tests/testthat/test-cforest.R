test_that("cforest - classification", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_cls_cforest)
  check_characterize_object(bag_chr)

  expect_equal(
    .pluck_num_features_active(fit_cls_cforest)$value,
    length(exp_cls_cforest$features_active)
  )
  expect_equal(
    .pluck_features_active(fit_cls_cforest)$value[[1]],
    sort(exp_cls_cforest$features_active)
  )
  expect_equal(
    .pluck_num_term_nodes(fit_cls_cforest)$value[[1]],
    exp_cls_cforest$num_term_nodes
  )
})

test_that("cforest - regression", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_reg_cforest)
  check_characterize_object(bag_chr)

  expect_equal(
    .pluck_num_features_active(fit_reg_cforest)$value,
    length(exp_reg_cforest$features_active)
  )
  expect_equal(
    .pluck_features_active(fit_reg_cforest)$value[[1]],
    sort(exp_reg_cforest$features_active)
  )
  expect_equal(
    .pluck_num_term_nodes(fit_reg_cforest)$value[[1]],
    exp_reg_cforest$num_term_nodes
  )
})
