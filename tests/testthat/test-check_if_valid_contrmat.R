test_that("Error messages for valid contrast matrix work", {
  expect_error(.is_valid_contrmat(matrix(c(1, 1, 1, 1, 1, 1), nrow = 3)),
    regexp = "is invalid for contrasts"
  )
  expect_error(.is_valid_contrmat(orth_polynomial_code(5)[, -1]),
               regexp = "invalid size")
})
