test_that("Parsing drop sequences works", {
  a <- 1
  b <- 3
  test_env <- new.env()
  test_env$a <- 3
  test_env$b <- 5
  expect_equal(.parse_drop_sequence("3:5", test_env), 3:5)
  expect_equal(.parse_drop_sequence("3L:5L", test_env), 3:5)
  expect_equal(.parse_drop_sequence("a:5", test_env), 3:5)
  expect_equal(.parse_drop_sequence("3:b", test_env), 3:5)
  expect_equal(.parse_drop_sequence("a:b", test_env), 3:5)
  expect_error(.parse_drop_sequence("cc:5", test_env), regexp = r"(object 'cc' not found)")
})
