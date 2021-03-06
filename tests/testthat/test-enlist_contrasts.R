test_that("Names of resulting list of contrasts correct", {
  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = factor(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))

  expect_equal(names(enlist_contrasts(tst_data, two~contr.sum, three~scaled_sum_code, four~contr.poly)),
               c("two", "three", "four"))
})

test_that("Throw error when factor column not found in model data frame", {
  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = factor(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))

  # Default error message works fine, no need to specify it to something else
  expect_error(enlist_contrasts(tst_data, foo~contr.sum),regexp = "undefined columns selected")
})

test_that("Setting both reference and intercept simultaneously with + and * works", {

  my_df <- mtcars
  my_df$gear = factor(my_df$gear)
  my_df$carb = factor(my_df$carb)
  expect_equal(
    enlist_contrasts(my_df,
                     gear ~ contr.treatment,
                     carb ~ contr.treatment)[[2L]],
    enlist_contrasts(my_df,
                     gear ~ contr.treatment,
                     carb ~ scaled_sum_code + 1 * 1)[[2L]]
  )

}
)

test_that("Passing a raw matrix call to set contrasts works", {
  my_df <- mtcars
  my_df$gear <-  factor(my_df$gear)
  my_df$carb <-  factor(my_df$carb)

  reference <- enlist_contrasts(my_df,
                                gear ~ contr.sum + 4,
                                carb ~ forward_difference_code)[[1L]]
  # Raw matrix
  expect_equal(
    enlist_contrasts(my_df,
                     gear ~ matrix(c(1,-1,0,0,-1,1), nrow = 3),
                     carb ~ forward_difference_code)[[1L]],
    reference
  )

})

test_that("Environment handling with programmatically set reference levels works", {
  my_df <- mtcars
  my_df$gear <-  factor(my_df$gear)
  gear_levels <- levels(my_df$gear)

  # Apply scheme multiple times w/ different reference levels, then check contrasts
  output <- lapply(gear_levels,
                   function(ref_level)
                     set_contrasts(my_df,
                                   gear ~ scaled_sum_code + ref_level * ref_level)) %>%
    lapply(function(set_df) contrasts(set_df$gear))

  # Row names are all the same, col names vary depending on reference level
  reference <-
    list(matrix(c(0, 1, 0, 0, 0, 1), nrow = 3,dimnames = list(3:5, c(4,5))),
         matrix(c(1, 0, 0, 0, 0, 1), nrow = 3,dimnames = list(3:5, c(3,5))),
         matrix(c(1, 0, 0, 0, 1, 0), nrow = 3,dimnames = list(3:5, c(3,4))))

  expect_equal(output, reference)

})

test_that("Environment handling when piping with magrittr works", {
  my_df <- mtcars
  my_df$gear <-  factor(my_df$gear)
  magrittr_df <- my_df %>% set_contrasts(gear ~ helmert_code)
  magrittr_contrasts <- my_df %>% enlist_contrasts(gear ~ helmert_code)
  native_df <- set_contrasts(my_df,gear ~ helmert_code)

  expect_equal(contrasts(native_df$gear), contrasts(magrittr_df$gear))
  expect_equal(contrasts(native_df$gear), magrittr_contrasts[[1L]])
})

test_that("Error handling when an invalid matrix is passed", {
  expect_error(enlist_contrasts(mtcars, gear ~ matrix(c(1, 1, 1, 2, 2, 2), nrow = 3), verbose = FALSE),
               regexp = ("Lapack"))
})

test_that("Passing matrix in variable name works", {
  tstvar <- contr.poly(6)
  reference <- enlist_contrasts(mtcars, carb ~ tstvar, verbose = FALSE)
  rownames(tstvar) <- c(1,2,3,4,6,8)

  expect_equal(list("carb" = tstvar), reference)

})

