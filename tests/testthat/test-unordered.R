test_that("unordered checks work", {
  expect_true(is.unordered(gl(5,1)))
  expect_false(is.unordered(gl(5,1,ordered = TRUE)))
})

test_that("unordered coercions work", {

  # Convert an ordered factor to unordered
  expect_true(is.unordered(as.unordered(gl(5,1,ordered = TRUE))))

  # If level order is pre-specified differently from default alphabetical order
  # then the ordering will be retained
  expect_equal(c("c", "a", "b"),
               levels(as.unordered(ordered(c("a", "b", "c"),
                              levels = c("c", "a", "b")))))

  # Otherwise the vector will be converted to an unordered factor with levels
  # in the default alphabetical order
  expect_equal(c("a", "b", "c"), levels(as.unordered(c("c", "a", "b"))))
  expect_equal(as.character(1:4), levels(as.unordered(4:1)))
})
