test_that("Intercept interpretations work", {
  expect_equal(interpret_intercept(contr.treatment(2)), "mean(1)")
  expect_equal(interpret_intercept(contr.SAS(2)), "mean(2)")
  expect_equal(interpret_intercept(contr.sum(2)), "grand mean")

  unweighted_intercept <- solve(t(matrix(c(.5, .5, 0, -1, 1, 0, -1, 0, 1), nrow = 3)))[, 2:3]
  weighted_intercept <- solve(t(matrix(c(.8, .2, 0, -1, 1, 0, -1, 0, 1), nrow = 3)))[, 2:3]

  expect_equal(interpret_intercept(unweighted_intercept), "mean(1,2)")
  expect_equal(interpret_intercept(weighted_intercept), "custom weights")
})
