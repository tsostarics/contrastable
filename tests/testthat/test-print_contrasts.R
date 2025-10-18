test_that("printing contrasts works", {
  tstdata <- data.frame(x = gl(3, 1), y = gl(3, 1, ordered = TRUE))
  expect_silent(print_contrasts(mtcars))
  expect_output(print_contrasts(tstdata), "2378")
  expect_output(print_contrasts(tstdata, "y"), "2378")
})
