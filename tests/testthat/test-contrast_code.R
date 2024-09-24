
test_that("default 2 level factor works", {
  unordered_result <- suppressWarnings(use_contrasts(factor(1:2)))
  ordered_result <- suppressWarnings(use_contrasts(ordered(factor(1:2))))

  expect_equal(unordered_result,
               structure(c(0, 1),
                         dim = 2:1,
                         dimnames = list(c("1", "2"), "2")))

  expect_equal(round(ordered_result, 6),
               structure(c(-0.707107, 0.707107),
                         dim = 2:1,
                         dimnames = list(NULL, ".L")))
})

test_that("Atomic type object throws error", {
  expect_error(use_contrasts(factor(c(1, 2, 3)), c(0, 1, 0, 0, 0, 1)),
               regexp = "atomic")

  expect_error(enlist_contrasts(mtcars, gear ~ 1 + sum_code, verbose = FALSE),
               regexp = "atomic")
})

test_that("Unknown nonatomic object throws warning", {
  foo <- list(sum_code(3))
  class(foo) <- "foo"

  expect_warning(use_contrasts(factor(c(1, 2, 3)), foo),
               regexp = "Can't set contrasts with object of class foo")

})


test_that("Labelling parsing works", {
  tst_data <-
    data.frame(
      two = factor(c("a", "b", "a", "b")),
      three = factor(c("a", "b", "c", "a")),
      four = factor(c("a", "b", "c", "d"))
    )

  my_labels <- c("test1", "test2")

  test_contrasts <-
    enlist_contrasts(
      tst_data,
      two ~ treatment_code | "test",
      three ~ treatment_code | my_labels,
      four ~ treatment_code | c("t1", "t2", "t3")
    )

  expect_equal(colnames(test_contrasts[["two"]]), "test")
  expect_equal(colnames(test_contrasts[["three"]]), c("test1", "test2"))
  expect_equal(colnames(test_contrasts[["four"]]), c("t1", "t2", "t3"))

})

test_that("Argument handling in parentheses & empty parentheses work", {
  tst_data <-
    data.frame(
      two = factor(c("a", "b", "a", "b")),
      three = factor(c("a", "b", "c", "a")),
      four = factor(c("a", "b", "c", "d"))
    )


  test_contrasts <-
    enlist_contrasts(
      tst_data,
      two ~ contr.poly,
      three ~ contr.poly(),
      four ~ contr.poly(scores = c(.1, .2, .5, .9))
    )

  expect_equal(test_contrasts[["two"]],
               contr.poly(2),
               ignore_attr = TRUE
  )
  expect_equal(test_contrasts[["three"]],
               contr.poly(3),
               ignore_attr = TRUE
  )
  expect_equal(test_contrasts[["four"]],
               contr.poly(4, scores = c(.1, .2, .5, .9)),
               ignore_attr = TRUE
  )

  expect_error(
    suppressMessages(
      set_contrasts(tst_data, three ~ treatment_code(bogus = 1))
    ),
    regexp = "unused argument"
  )
})

test_that("Setting contrast with hypr object works", {
  testthat::skip_if_not_installed("hypr")
  tst_data <-
    data.frame(three = factor(c("a", "b", "c", "a")))

  hypr_object <- hypr::hypr(b ~ a, c ~ a)

  test_contrasts <-
    enlist_contrasts(tst_data, three ~ hypr_object)

  expect_equal(test_contrasts,
               enlist_contrasts(tst_data, three ~ scaled_sum_code),
               ignore_attr = TRUE
  )
})


test_that("Warning with missing level hypr object works", {
  testthat::skip_if_not_installed("hypr")

  tst_data <-
    data.frame(three = factor(c("a", "b", "c", "a")))

  hypr_object <- hypr::hypr(b ~ a, d ~ a)
  hypr_object2 <- hypr::hypr(threeb ~ threea, threed ~ threea)

  expect_warning(enlist_contrasts(tst_data, three ~ hypr_object),
                 regexp = "not found in factor column `three`: d"
  )

  expect_warning(enlist_contrasts(tst_data, three ~ hypr_object2),
                 regexp = "not found in factor column `three`: d"
  )
})

test_that("No warning when factor passed to use_contrasts directly", {
  testthat::skip_if_not_installed("hypr")

  hypr_object <- hypr::hypr(b ~ a, d ~ a)
  expect_equal(use_contrasts(factor(c("a", "b", "c", "a")), hypr_object),
               scaled_sum_code(3),
               ignore_attr = TRUE
  )
})

test_that("Warnings when trying to set values with hypr object", {
  testthat::skip_if_not_installed("hypr")

  hypr_object <- hypr::hypr(b ~ a, d ~ a)
  expect_warning(use_contrasts(factor(c("a", "b", "c", "a")),
                               hypr_object,
                               reference_level = "b"),
                 regexp = "reference_level ignored"
  )
  expect_warning(use_contrasts(factor(c("a", "b", "c", "a")),
                               hypr_object,
                               set_intercept = "b"),
                 regexp = "set_intercept ignored"
  )
  expect_warning(use_contrasts(factor(c("a", "b", "c", "a")),
                               hypr_object,
                               drop_trends = "b"),
                 regexp = "drop_trends ignored"
  )
})

