test_that("Different combinations of formula operations work", {
  # Grouped by which should be equivalent
  formulae <-
    list(
      # Tests for individual parameters
      f1  = 1 ~ sum_code,
      f2  = 1 ~ sum_code + 3,
      f3  = 1 ~ sum_code * 4,
      f4  = 1 ~ sum_code - 5:6,
      # Tests for combinations of parameters
      f5  = 1 ~ sum_code + 3 * 4,
      f6  = 1 ~ sum_code * 4 + 3,
      f7  = 1 ~ sum_code + 3 - 5:6,
      f8  = 1 ~ sum_code - 5:6 + 3,
      f9  = 1 ~ contr.poly * 4 - 5:6,
      f10 = 1 ~ contr.poly - 5:6 * 4,
      # Tests for combinations of all parameters
      f11 = 1 ~ contr.poly + 3 * 4 - 5:6,
      f12 = 1 ~ contr.poly + 3 - 5:6 * 4,
      f12 = 1 ~ contr.poly * 4 + 3 - 5:6,
      f13 = 1 ~ contr.poly * 4 - 5:6 + 3,
      f14 = 1 ~ contr.poly - 5:6 + 3 * 4,
      f15 = 1 ~ contr.poly - 5:6 * 4 + 3,
      # Ensure as_is_works correctly
      f16 = 1 ~ I(sum_code) - 5:6 * 4 + 3,
      f17 = 1 ~ I(sum_code)
    )

  params <- suppressWarnings(lapply(formulae, .make_parameters))
  expect_true(params$f1$factor_col == 1 &
                identical(params$f2$code_by, sym("sum_code")))
  expect_true(params$f2$factor_col == 1 &
                identical(params$f2$code_by, sym("sum_code")) &
                params$f2$reference_level == 3)
  expect_true(params$f3$factor_col == 1 &
                identical(params$f3$code_by, sym("sum_code")) &
                params$f3$intercept_level == 4)
  expect_true(params$f4$factor_col == 1 &
                identical(params$f4$code_by, sym("sum_code")) &
                identical(params$f4$drop_trends, NA))
  expect_true(params$f5$factor_col == 1 &
                identical(params$f5$code_by, sym("sum_code")) &
                params$f5$reference_level == 3 &
                params$f5$intercept_level == 4)
  expect_equal(params$f5, params$f6)
  expect_true(params$f7$factor_col == 1 &
                identical(params$f7$code_by, sym("sum_code")) &
                params$f7$reference_level == 3 &
                identical(params$f7$drop_trends, NA))
  expect_equal(params$f7, params$f8)
  expect_true(params$f9$factor_col == 1 &
                identical(params$f9$code_by, sym("contr.poly")) &
                params$f9$intercept_level == 4 &
                identical(params$f9$drop_trends, expr(5:6)))
  expect_equal(params$f9, params$f10)
  expect_true(params$f11$factor_col == 1 &
                identical(params$f11$code_by, sym("contr.poly")) &
                params$f11$reference_level == 3 &
                params$f11$intercept_level == 4 &
                identical(params$f11$drop_trends, expr(5:6)))

  purrr::walk(
    params[12:16],
    function(ps) {
      expect_equal(ps, params$f11)
    }
  )

  expect_true(params$f16$as_is)
  expect_true(params$f17$as_is)
})


test_that("Operator checking works", {
  expect_true(.is_reserved_operator(sym("+")))
  expect_false(.is_reserved_operator(sym("a")))
})

test_that("Multiple operator usage disallowed", {
  expect_error(
    enlist_contrasts(mtcars, cyl ~ sum_code + 4 + 4, verbose = FALSE),
    regex = "You may only use"
  )
})

test_that("Error on illformed expression", {
  expect_error(.make_parameters(cyl ~ +1 + stats::contr.sum),
               regexp = "\\+1 \\+ sum_code")
})

test_that("Backtransforming contrasts works", {
  expect_equal(contr.sum(2), .convert_matrix(.convert_matrix(contr.sum(2))),
               ignore_attr = TRUE)
})
