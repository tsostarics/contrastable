test_that("Character coercion error message works", {
  expect_error(set_contrasts(mtcars, carb = contr.poly), regexp = "Did you use = instead of ~")
})