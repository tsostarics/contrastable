test_that("two level functional coding work", {
  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = factor(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))

  expect_equal(use_contrast_function(tst_data$two, contr.sum, reference_level = "b"),
               use_contrast_matrix(tst_data$two, contr.sum(2)))
  expect_equal(use_contrast_function(tst_data$two, contr.sum, reference_level = "a"),
               use_contrast_matrix(tst_data$two, -contr.sum(2)))
})

test_that("four level functional coding work", {
  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = factor(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))

  expect_equal(use_contrast_function(tst_data$four, contr.poly),
               use_contrast_matrix(tst_data$four, contr.poly(4)))
  expect_equal(use_contrast_function(tst_data$four, reverse_helmert_code),
               use_contrast_matrix(tst_data$four, reverse_helmert_code(4)))
})
