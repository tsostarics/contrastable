test_that("Messaging when there are remaining factors works", {
  tst_data <-
    data.frame(two = factor(c('a','b','a','b')),
               three = ordered(c('a','b','c','a')),
               four = factor(c('a','b','c','d')))

  re <- paste("unset factors:", crayon::blue("two"), crayon::red("three"), crayon::blue("four"))
  expect_message(.msg_if_remaining_factors(tst_data, "a"),
                 regexp = gsub("\\[", "\\\\[", re))

})

test_that("Messaging if user tries to reset ordered factors works", {
  my_df <- mtcars
  my_df$gear <-  ordered(my_df$gear)

  expect_message(enlist_contrasts(my_df, gear ~ scaled_sum_code),
                 regexp = "These factors are ordered")
})

test_that("Messaging if factor coercion occurs works",{
  expect_message(.msg_if_coerced_to_factors(c('a','b')),
                 regexp = "Converting to factors")
})

test_that("Warning if one level factor works", {
  tstdf <- data.frame(a = 1:5,
             b = factor("a"))

  expect_error(suppressWarnings(enlist_contrasts(tstdf, b ~ sum_code)),
               regexp = "No factors with more")
  expect_warning(enlist_contrasts(tstdf, b ~ treatment_code, a ~ treatment_code, verbose=FALSE),
                 regexp = "only one level")
})

test_that("Warning if one level works with different parameters", {
  tstdf <- data.frame(one = factor("a"),
                      two = factor("b"),
                      three = factor(c('a','b','c')))
  expect_warning(.warn_if_onelevel(c("one","two")), regexp = "only one level")
  expect_warning(.warn_if_onelevel(NULL, tstdf, c('one','two','three')), regexp = "only one level")
  expect_error(.warn_if_onelevel(NULL), regexp = "model data and factors")
})

