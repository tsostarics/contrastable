test_that("Character coercion error message works", {
  expect_error(set_contrasts(mtcars, carb = contr.poly),
               regexp = "Did you use = instead of ~")
})

test_that("Other error messages still display correctly", {
  expect_error(set_contrasts(mtcars, carb = contr.poly()),
               regexp = "is missing, with no default")
})
