test_that("tidyselect patterns work", {
  reference_value <- enlist_contrasts(mtcars,
                                      cyl ~ sum_code,
                                      gear ~ sum_code,
                                      verbose = FALSE)
  expect_equal(reference_value,
               enlist_contrasts(mtcars, cyl + gear ~ sum_code,
                                verbose = FALSE))
  expect_equal(reference_value,
               enlist_contrasts(mtcars, c(cyl, gear) ~ sum_code,
                                verbose = FALSE))
  expect_equal(reference_value,
               enlist_contrasts(mtcars, all_of(c('cyl', 'gear')) ~ sum_code,
                                verbose = FALSE))
})

test_that("environment handling with passed symbol works", {
  reference_value <- enlist_contrasts(mtcars,
                                      cyl ~ sum_code,
                                      gear ~ sum_code,
                                      verbose=FALSE)
  these_vars <- c("cyl", "gear")
  expect_equal(reference_value,
               enlist_contrasts(mtcars,
                                all_of(these_vars) ~ sum_code,
                                verbose=FALSE))
})

test_that("tidyselect helper works", {
  expect_equal(names(mtcars),
               names(enlist_contrasts(mtcars,
                                      where(is.numeric) ~ sum_code,
                                      verbose = FALSE)))
})

test_that("tidyselect errors work", {
  expect_error(enlist_contrasts(mtcars, cyl ~ sum_code, cyl ~ scaled_sum_code),
               '"cyl" at locations 1 and 2')

  expect_error(enlist_contrasts(mtcars,
                                cyl ~ sum_code,
                                where(is.numeric) ~ sum_code),
               'Left hand side of multiple formulas')
})

