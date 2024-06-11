test_that("Different combinations of formula operations work", {
  # Grouped by which should be equivalent
  formulae <-
    list(
      f1 = 1 ~ 2,
      f2 = 1 ~ 2 + 3,
      f3 = 1 ~ 2 * 4,
      f4 = 1 ~ 2 - 5:6,
      f5 = 1 ~ 2 + 3 * 4,
      f6 = 1 ~ 2 * 4 + 3,
      f7 = 1 ~ 2 + 3 - 5:6,
      f8 = 1 ~ 2 - 5:6 + 3,
      f9 = 1 ~ 2 * 4 - 5:6,
      f10 = 1 ~ 2 - 5:6 * 4,
      f11 = 1 ~ 2 + 3 * 4 - 5:6,
      f12 = 1 ~ 2 + 3 - 5:6 * 4,
      f12 = 1 ~ 2 * 4 + 3 - 5:6,
      f13 = 1 ~ 2 * 4 - 5:6 + 3,
      f14 = 1 ~ 2 - 5:6 + 3 * 4,
      f15 = 1 ~ 2 - 5:6 * 4 + 3
    )

  params <- lapply(formulae, .make_parameters)

  expect_true(params$f1$factor_col == 1 & params$f1$code_by == 2)
  expect_true(params$f2$factor_col == 1 &
                params$f2$code_by == 2 &
                params$f2$reference_level == 3)
  expect_true(params$f3$factor_col == 1 &
                params$f3$code_by == 2 &
                params$f3$intercept_level == 4)
  expect_true(params$f4$factor_col == 1 &
                params$f4$code_by == 2 &
                identical(params$f4$drop_trends, expr(5:6)))
  expect_true(params$f5$factor_col == 1 &
                params$f5$code_by == 2 &
                params$f5$reference_level == 3 &
                params$f5$intercept_level == 4)
  expect_equal(params$f5, params$f6)
  expect_true(params$f7$factor_col == 1 &
                params$f7$code_by == 2 &
                params$f7$reference_level == 3 &
                identical(params$f7$drop_trends, expr(5:6)))
  expect_equal(params$f7, params$f8)
  expect_true(params$f9$factor_col == 1 &
                params$f9$code_by == 2 &
                params$f9$intercept_level == 4 &
                identical(params$f9$drop_trends, expr(5:6)))
  expect_equal(params$f9, params$f10)
  expect_true(params$f11$factor_col == 1 &
                params$f11$code_by == 2 &
                params$f11$reference_level == 3 &
                params$f11$intercept_level == 4 &
                identical(params$f11$drop_trends, expr(5:6)))

  purrr::walk(
    params[12:16],
    function(ps) {
      expect_equal(ps, params$f11)
    }
  )
})


test_that("Operator checking works", {
  expect_true(.is_reserved_operator(sym("+")))
  expect_false(.is_reserved_operator(sym("a")))
})
