test_that("Factor conversion message works", {
  expect_message(.convert_to_factors(mtcars, "cyl"),
                 regexp = "Converting to factors")
})
