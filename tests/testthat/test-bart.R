test_that("BART - classification", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_cls_dbart)
  check_characterize_object(bag_chr)

  used <- apply(fit_cls_dbart$varcount, 2, function(x) any(x > 0))
  expect_equal(
    .pluck_num_features_active(fit_cls_dbart)$value,
    sum(used)
  )
  expect_equal(
    .pluck_features_active(fit_cls_dbart)$value[[1]],
    sort(names(used)[used])
  )
  expect_equal(
    .pluck_num_term_nodes(fit_cls_dbart)$value[[1]],
    sum(sum(apply(fit_cls_dbart$varcount, 1, sum) - 1))
  )
})

test_that("BART - regression", {
  # tests objects in "test_cases.RData"

  bag_chr <- characterize(fit_reg_dbart)
  check_characterize_object(bag_chr)

  used <- apply(fit_reg_dbart$varcount, 2, function(x) any(x > 0))
  expect_equal(
    .pluck_num_features_active(fit_reg_dbart)$value,
    sum(used)
  )
  expect_equal(
    .pluck_features_active(fit_reg_dbart)$value[[1]],
    sort(names(used)[used])
  )
  expect_equal(
    .pluck_num_term_nodes(fit_reg_dbart)$value[[1]],
    sum(sum(apply(fit_reg_dbart$varcount, 1, sum) - 1))
  )
})
