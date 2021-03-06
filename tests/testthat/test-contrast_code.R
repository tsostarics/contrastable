test_that("two level factor coding works", {
  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = factor(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))

  expect_equal(contrasts(tst_data$two), contrast_code(tst_data$two, matrix(c(0,1))))
  expect_equal(colnames(contrast_code(tst_data$two, matrix(c(.5, -.5)))), "a")
  expect_equal(colnames(contrast_code(tst_data$two, matrix(c(1,0)))),"a")
  expect_equal(colnames(contrast_code(tst_data$two, matrix(c(0,1)))),"b")
})

test_that("three level factor coding works", {
  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = factor(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))

  expect_equal(
    colnames(
      contrast_code(
        tst_data$three,
        matrix(c(-1/3, -1/3, 2/3, 2/3, -1/3, -1/3),
               nrow=3)
      )
    ),
    c("c","a")
  )
  expect_equal(matrix(c(-1/3, -1/3, 2/3, 2/3, -1/3, -1/3),nrow=3),
               contrast_code(
                 tst_data$three,
                 matrix(c(-1/3, -1/3, 2/3, 2/3, -1/3, -1/3),
                        nrow=3)
               ),
               ignore_attr = TRUE
  )
})

test_that("four level helmert coding works", {
  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = factor(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))

  expect_equal(
    colnames(contrast_code(tst_data$four, matrix(c(-1/4, -1/4, -1/4, 3/4,
                                                   -1/3, -1/3, 2/3, 0,
                                                   -1/2, 1/2, 0, 0),
                                                 nrow = 4))),
    c("d","c","b")
  )
  expect_equal(contrast_code(tst_data$four, matrix(c(-1/4, -1/4, -1/4, 3/4,
                                                     -1/3, -1/3, 2/3, 0,
                                                     -1/2, 1/2, 0, 0),
                                                   nrow = 4)),
               matrix(c(-1/4, -1/4, -1/4, 3/4,
                        -1/3, -1/3, 2/3, 0,
                        -1/2, 1/2, 0, 0),
                      nrow = 4),
               ignore_attr = TRUE)
})

test_that("polynomial coding works", {
  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = factor(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))

  expect_equal(colnames(contrast_code(tst_data$four, contr.poly(4))),
               c(".L",".Q",".C")
  )
})

test_that("four level functional coding work", {
  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = factor(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))

  expect_equal(functional_code(tst_data$four, contr.poly),
               contrast_code(tst_data$four, contr.poly))
  expect_equal(contrast_code(tst_data$four, contr.poly(4)),
               manual_code(tst_data$four, contr.poly(4)))
  expect_equal(functional_code(tst_data$four, scaled_sum_code),
               contrast_code(tst_data$four, scaled_sum_code))
})

test_that("default 2 level factor works", {
  test_matrix <- matrix(c(-.5,.5), nrow = 2)
  colnames(test_matrix) <- 2
  rownames(test_matrix) <- c(1,2)

  expect_equal(test_matrix, contrast_code(factor(1:2)))
})

test_that("Non matrix or function error works", {
  expect_error(contrast_code(factor(c(1,2,3)), c(0,1,0,0,0,1)),
               regexp = "Invalid value for code_by")
})


test_that("Labelling parsing works", {
  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = factor(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))

  my_labels = c('test1','test2')

  test_contrasts <-
    enlist_contrasts(tst_data,
                     two ~ treatment_code | "test",
                     three ~ treatment_code | my_labels,
                     four ~ treatment_code | c('t1','t2','t3'))

  expect_equal(colnames(test_contrasts[['two']]), "test")
  expect_equal(colnames(test_contrasts[['three']]), c('test1', 'test2'))
  expect_equal(colnames(test_contrasts[['four']]), c('t1', 't2', 't3'))

  expect_error(
    suppressMessages(
      set_contrasts(tst_data, three ~ treatment_code | my_labels + "a" )
    ),
    regexp = "must be the last operator"
  )
})

test_that("Argument handling in parentheses & empty parentheses work", {

  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = factor(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))


  test_contrasts <-
    enlist_contrasts(tst_data,
                     two ~ contr.poly,
                     three ~ contr.poly(),
                     four ~ contr.poly(scores = c(.1, .2, .5, .9)))

  expect_equal(test_contrasts[['two']],
               contr.poly(2),
               ignore_attr = TRUE)
  expect_equal(test_contrasts[['three']],
               contr.poly(3),
               ignore_attr = TRUE)
  expect_equal(test_contrasts[['four']],
               contr.poly(4, scores = c(.1, .2, .5, .9)),
               ignore_attr = TRUE)

  expect_error(
    suppressMessages(
      set_contrasts(tst_data, three ~ treatment_code(bogus = 1))
    ),
    regexp = "unused argument"
  )
})
