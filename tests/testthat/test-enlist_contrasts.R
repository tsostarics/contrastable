test_that("Names of resulting list of contrasts correct", {
  tst_data <-
    data.frame(
      two = factor(c("a", "b", "a", "b")),
      three = factor(c("a", "b", "c", "a")),
      four = factor(c("a", "b", "c", "d"))
    )

  expect_equal(
    names(enlist_contrasts(tst_data,
                           two ~ contr.sum,
                           three ~ scaled_sum_code,
                           four ~ contr.poly,
                           verbose = FALSE)),
    c("two", "three", "four")
  )
})

test_that("Throw error when factor column not found in model data frame", {
  tst_data <-
    data.frame(
      x = gl(2, 1)
    )

  # Default error message works fine, no need to specify it to something else
  expect_error(enlist_contrasts(tst_data, foo ~ contr.sum),
               regexp = "Can't select columns that")
})

test_that("Setting both reference and intercept with + and * works", {
  my_df <- mtcars
  my_df$gear <- factor(my_df$gear)
  my_df$carb <- factor(my_df$carb)
  expect_equal(
    enlist_contrasts(
      my_df,
      gear ~ contr.treatment,
      carb ~ contr.treatment
    )[[2L]],
    enlist_contrasts(
      my_df,
      gear ~ contr.treatment,
      carb ~ scaled_sum_code + 1 * 1
    )[[2L]]
  )
})

test_that("Raw matrix call with automatic reference level switching", {
  my_df <- mtcars
  my_df$gear <- factor(my_df$gear)
  my_df$carb <- factor(my_df$carb)

  reference <- enlist_contrasts(
    my_df,
    gear ~ contr.sum + 3,
    carb ~ forward_difference_code
  )[[1L]]
  # Raw matrix
  expect_equal(
    enlist_contrasts(
      my_df,
      gear ~ matrix(c(1, -1, 0, 0, -1, 1), nrow = 3),
      carb ~ forward_difference_code
    )[[1L]],
    reference
  )
})

test_that("Matrix doesn't trigger atomic error", {
  cmat <- sum_code(3)
  expect_equal(sum_code(3),
               enlist_contrasts(mtcars, cyl ~ cmat, verbose = FALSE)[[1]],
               ignore_attr = TRUE)

  class(cmat) <- "foo"
  expect_equal(treatment_code(3),
               suppressWarnings(
                 enlist_contrasts(mtcars, cyl ~ cmat, verbose = FALSE)[[1]]
                 ),
               ignore_attr = TRUE)
})

test_that("Programmatic environment handling with set reference levels works", {
  my_df <- mtcars
  my_df$gear <- factor(my_df$gear)
  gear_levels <- levels(my_df$gear)

  # Apply scheme multiple times w/ different reference levels,
  # then check contrasts
  output <- lapply(
    gear_levels,
    function(ref_level) {
      set_contrasts(
        my_df,
        gear ~ scaled_sum_code + ref_level * ref_level
      )
    }
  ) |>
    lapply(function(set_df) contrasts(set_df$gear))

  # Row names are all the same, col names vary depending on reference level
  reference <-
    list(
      matrix(c(0, 1, 0, 0, 0, 1), nrow = 3, dimnames = list(3:5, c(4, 5))),
      matrix(c(1, 0, 0, 0, 0, 1), nrow = 3, dimnames = list(3:5, c(3, 5))),
      matrix(c(1, 0, 0, 0, 1, 0), nrow = 3, dimnames = list(3:5, c(3, 4)))
    )

  expect_equal(output, reference)
})

test_that("Error handling when an invalid matrix is passed", {
  expect_error(enlist_contrasts(mtcars,
                                gear ~ matrix(c(1, 1, 1, 2, 2, 2),
                                              nrow = 3),
                                verbose = FALSE),
               regexp = ("Lapack")
  )
})

test_that("Passing matrix in variable name works", {
  tstvar <- contr.poly(6)
  reference <- enlist_contrasts(mtcars, carb ~ tstvar, verbose = FALSE)
  rownames(tstvar) <- c(1, 2, 3, 4, 6, 8)

  expect_equal(list("carb" = tstvar), reference)
})


test_that("Providing namespace with contrast function works", {
  newdata <- mtcars
  newdata$carb <- factor(newdata$carb)
  df1 <- enlist_contrasts(newdata, carb ~ contrastable::sum_code)
  df2 <- enlist_contrasts(newdata, carb ~ sum_code)

  expect_equal(df1, df2)
})

test_that("Warning is thrown when colnames are not set and as_is is TRUE", {
  newdata <- mtcars
  newdata$carb <- factor(newdata$carb)
  expect_warning(enlist_contrasts(newdata, carb ~ I(contr.sum)),
                 regexp = "and as_is=TRUE"
  )
})

test_that("As is with I works as expected", {
  newdata <- mtcars
  newdata$carb <- factor(newdata$carb)
  df1 <- suppressWarnings(enlist_contrasts(newdata, carb ~ I(contr.sum)))
  df2 <- enlist_contrasts(newdata, carb ~ sum_code + 8)
  expect_equal(df1, df2, ignore_attr = TRUE)
})

test_that("nested I is handled appropriately", {
  newdata <- mtcars
  newdata$carb <- factor(newdata$carb)
  df1 <- suppressWarnings(
    enlist_contrasts(newdata,
                     carb ~ I(I(I((contr.sum))))
    )
  )

  expect_equal(df1[[1]], contr.sum(6), ignore_attr = TRUE)
})

test_that("moving reference level to earlier level works", {
  newdata <- mtcars
  newdata$carb <- factor(newdata$carb)
  df1 <- enlist_contrasts(newdata, carb ~ contr.sum + 4)[[1]]
  df2 <- structure(c(
    0, 0, 0, -1, 0, 1, 1, 0, 0, -1, 0, 0, 0, 1, 0, -1,
    0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, -1, 1, 0
  ), dim = 6:5, dimnames = list(
    c("1", "2", "3", "4", "6", "8"), c("8", "1", "2", "3", "6")
  ))
  expect_equal(df1, df2)
})

test_that("warn when n does not match", {
  expect_warning(
    enlist_contrasts(mtcars, gear ~ contr.poly(n = 4), verbose = FALSE),
    regex = "using nlevels instead")
})

test_that("setting contrasts with external list works", {
  my_contrasts <- list(carb ~ treatment_code)
  expect_equal(
    enlist_contrasts(mtcars, my_contrasts, verbose = FALSE)[[1]],
    treatment_code(6),
    ignore_attr = TRUE
  )

  # Test for list length >1
  my_contrasts <- list(carb ~ treatment_code, gear ~ helmert_code)
  expect_equal(
    enlist_contrasts(mtcars, my_contrasts, verbose = FALSE)[[1]],
    treatment_code(6),
    ignore_attr = TRUE
  )
})

test_that("error when no formula provided", {
  expect_error(enlist_contrasts(mtcars),
               regexp = "No contrast formulas provided")
})

test_that("Warnings when reference level is attempted to be changed work", {
  expect_error(
    enlist_contrasts(mtcars, cyl ~ helmert_code + "a", verbose = FALSE),
    regexp = "Reference level not found"
  )
  expect_warning(
    enlist_contrasts(mtcars, cyl ~ helmert_code + 6, verbose = FALSE),
    regexp = "Ignoring reference level"
  )
})

test_that("Error message when forgetting model_data works", {
  expect_error(enlist_contrasts(cyl ~ 2),
               regexp = "forget to pass a data frame"
  )
  expect_error(enlist_contrasts(matrix(c(1, 1, 1, 1), nrow = 2)),
               regexp = "Instead found"
  )
})


test_that("Error on illformed formula", {
  expect_error(enlist_contrasts(mtcars, cyl ~ 4 + contr.sum, verbose = FALSE),
               regexp = "with atomic type")
})


test_that("Error on illformed formula", {
  expect_error(
    enlist_contrasts(mtcars, cyl ~ c("3", "4") + contr.sum, verbose = FALSE),
    regexp = "Reference level is a function"
  )
})


test_that("Error on mismatching dimensions", {
  cmat <- contr.sum(6)

  expect_error(enlist_contrasts(mtcars, cyl ~ cmat, verbose = FALSE),
               regexp = "is size 6x5 but .+? is size 3x2")
})

test_that("Error on missing intercept level", {
  expect_error(enlist_contrasts(mtcars, cyl ~ contr.sum * 7, verbose = FALSE),
               regexp = "to use as intercept not found")
})

test_that("Check that implementation of contrast functions remains the same", {
  # Occasionally, modifications to the codebase can break the handling of
  # matrix manipulations, so this serves as a canary if the 2-level edge case
  # or the more general 5-level case changes at all
  schemes <-
    c("sum_code",
      "treatment_code",
      "scaled_sum_code",
      "cumulative_split_code",
      "helmert_code",
      "reverse_helmert_code",
      "backward_difference_code",
      "forward_difference_code")

  names(schemes) <- schemes

  tstdata_2 <- data.frame(x = gl(2, 1))
  tstdata_5 <- data.frame(x = gl(5, 1))

  expect_snapshot(
    lapply(
      schemes,
      \(s) {
        fx <- sym(s)
        cmat <-
          rlang::inject(enlist_contrasts(tstdata_2, x ~ !!fx))

        as.character(MASS::fractions(cmat[[1L]]))
      }
    ))

  expect_snapshot(
    lapply(
      schemes,
      \(s) {
        fx <- sym(s)
        cmat <-
          suppressWarnings(
            rlang::inject(enlist_contrasts(tstdata_2, x ~ !!fx + 2))
          )

        as.character(MASS::fractions(cmat[[1L]]))
      }
    ))

  expect_snapshot(
    lapply(
      schemes,
      \(s) {
        fx <- sym(s)
        cmat <-
          rlang::inject(enlist_contrasts(tstdata_2, x ~ !!fx * 2))

        as.character(MASS::fractions(cmat[[1L]]))
      }
    ))

  # 5-level cases
  expect_snapshot(
    lapply(
      schemes,
      \(s) {
        fx <- sym(s)
        cmat <-
          rlang::inject(enlist_contrasts(tstdata_5, x ~ !!fx))

        as.character(MASS::fractions(cmat[[1L]]))
      }
    )
  )

  expect_snapshot(
    lapply(
      schemes,
      \(s) {
        fx <- sym(s)
        cmat <-
          rlang::inject(enlist_contrasts(tstdata_5, x ~ !!fx * 3))

        as.character(MASS::fractions(cmat[[1L]]))
      }
    )
  )

  expect_snapshot(
    lapply(
      schemes,
      \(s) {
        fx <- sym(s)
        cmat <-
          suppressWarnings(
            rlang::inject(enlist_contrasts(tstdata_5, x ~ !!fx + 3))
          )

        as.character(MASS::fractions(cmat[[1L]]))
      }
    )
  )
}
)
