test_that("backward difference labels work", {
  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = factor(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))

  expect_equal(dimnames(use_contrasts(tst_data$four, backward_difference_code(4))),
               list(c('a', 'b', 'c', 'd'),
                    c('b-a', 'c-b', 'd-c')))
})

test_that("forward difference labels work", {
  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = factor(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))

  expect_equal(dimnames(use_contrasts(tst_data$four, forward_difference_code(4))),
               list(c('a', 'b', 'c', 'd'),
                    c('a-b', 'b-c', 'c-d')))
})

test_that("backward difference values correct", {
  expect_equal(backward_difference_code(4),
               matrix(c(-.75, .25, .25, .25,
                        -.5, -.5, .5, .5,
                        -.25, -.25, -.25, .75), nrow=4)
  )
})

test_that("forward difference values correct", {
  expect_equal(forward_difference_code(4),
               matrix(rev(c(-.75, .25, .25, .25,
                        -.5, -.5, .5, .5,
                        -.25, -.25, -.25, .75)), nrow=4)
  )
})
