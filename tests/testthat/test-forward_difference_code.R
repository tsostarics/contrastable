test_that("backward difference labels work", {
  labels <- colnames(enlist_contrasts(data.frame(x=gl(4,1)),
                            x ~ backward_difference_code)[[1]])

  expect_equal(labels, c("2-1", "3-2", "4-3"))
})

test_that("forward difference labels work", {
  labels <- colnames(enlist_contrasts(data.frame(x=gl(4,1)),
                                      x ~ forward_difference_code)[[1]])

  expect_equal(labels, c("1-2", "2-3", "3-4"))
})

test_that("backward difference values correct", {
  expect_equal(
    backward_difference_code(4),
    matrix(c(
      -.75, .25, .25, .25,
      -.5, -.5, .5, .5,
      -.25, -.25, -.25, .75
    ), nrow = 4)
  )
})

test_that("forward difference values correct", {
  expect_equal(
    forward_difference_code(4),
    matrix(rev(c(
      -.75, .25, .25, .25,
      -.5, -.5, .5, .5,
      -.25, -.25, -.25, .75
    )), nrow = 4)
  )
})
