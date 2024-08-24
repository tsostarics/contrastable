test_that("cumulative split contrasts work", {
  hyp_matrix <- .convert_matrix(cumulative_split_code(4))

  expect_equal(round(colSums(hyp_matrix)), c(1,0,0,0))

  set.seed(111)
  testdf <- data.frame(x = gl(3,10),
                       y = c(rnorm(10, 4, .0001),
                             rnorm(10, 6, .0001),
                             rnorm(10, 10, .0001))) |>
    set_contrasts(x ~ cumulative_split_code, verbose = FALSE)

  expect_equal(round(coef(lm(y ~ x, data = testdf))),
               c("(Intercept)" = 7,
                 x1 = -4,
                 x2 = -5))
})
