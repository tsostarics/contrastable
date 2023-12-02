# test_that("Intuit reference level works", {
#   expect_equal(.intuit_reference_level(contr.poly(6), 1:6), NA)
#   expect_equal(.intuit_reference_level(contr.treatment(2), 1:2), 1)
#   expect_equal(.intuit_reference_level(contr.SAS(6), 1:6), 6)
#   expect_equal(.intuit_reference_level(scaled_sum_code(6), 1:6), 6)
#   expect_equal(.intuit_reference_level(helmert_code(6), 1:6), 6)
#   expect_equal(.intuit_reference_level(reverse_helmert_code(6), 1:6), 1)
#   expect_equal(.intuit_reference_level(contr.sum(6), 1:6), 6)
# })

test_that("Intercept interpretation works", {
  expect_equal(interpret_intercept(contr.treatment(5)), "mean(1)", ignore_attr = TRUE)
  expect_equal(interpret_intercept(scaled_sum_code(5)), "grand mean", ignore_attr = TRUE)
  expect_equal(interpret_intercept(helmert_code(5)), "grand mean", ignore_attr = TRUE)
  expect_equal(interpret_intercept(contr.poly(5)[,1:3]), "grand mean", ignore_attr = TRUE)
})

test_that("Glimpse works", {
  tstdf <- mtcars
  tstdf$cyl <- factor(tstdf$cyl)
  tst <- glimpse_contrasts(tstdf,
                           carb ~ contr.poly - 3:5,
                           gear ~ scaled_sum_code + 5,
                           verbose = FALSE)
  expect_equal(tst$factor, c('carb','gear','cyl'))
  expect_equal(tst$n, c(6, 3, 3), ignore_attr = TRUE) # need unname
  expect_equal(tst$scheme, c('contr.poly','scaled_sum_code','contr.treatment'), ignore_attr = TRUE) # need unname
  expect_equal(tst$reference, c(NA, '5', '4'), ignore_attr = TRUE) # need unname
  expect_equal(tst$intercept, c('grand mean', 'grand mean', 'mean(4)'), ignore_attr = TRUE)
  expect_equal(tst$orthogonal, c(TRUE, FALSE, FALSE))
  expect_equal(tst$centered, c(TRUE, TRUE, FALSE))
  expect_equal(tst$dropped_trends, c("3,4,5",NA,NA))
  expect_equal(tst$explicitly_set, c(TRUE,TRUE,FALSE))
})

test_that("Glimpse with variables works", {
  tstdf <- mtcars
  a <- 3
  b <- 4
  c <- 5
  mat <- scaled_sum_code(3)
  tst <- glimpse_contrasts(tstdf,
                           carb ~ contr.poly - a:c,
                           gear ~ mat + c * b,
                           cyl ~ scaled_sum_code + b * b,
                           verbose = FALSE)
  expect_equal(tst$scheme, c('contr.poly','custom','scaled_sum_code'))
  expect_equal(tst$intercept, c('grand mean', 'grand mean', 'mean(4)'), ignore_attr = TRUE)
  expect_equal(tst$dropped_trends, c('3,4,5',NA,NA))
})

test_that("Append namespace to scheme names", {
  expect_equal(.add_namespace(c("contr.helmert", "contr.sum", "scaled_sum_code")),
               c("stats::contr.helmert", "stats::contr.sum", "contrastable::scaled_sum_code"))
})

test_that("Warning with non default contrasts works", {
  tstdf <- dplyr::mutate(mtcars, gear = factor(gear), cyl = factor(cyl), carb = ordered(carb))
  tstdf <- set_contrasts(tstdf, cyl ~ contr.sum, carb ~ raw_polynomial_code, verbose = FALSE)

  expect_warning(.glimpse_default_factors(tstdf), regexp="Glimpse table may be unreliable")
})

test_that("Grouping columns aren't detected as ordered", {
  tst <- mtcars %>%
    dplyr::mutate(cyl = factor(cyl), carb = ordered(carb), gear = factor(gear)) %>%
    dplyr::group_by(cyl)

  # Avoid message from .warn_if_nondefault
  expect_warning(glimpse_contrasts(tst, verbose = FALSE), NA)
})


test_that("List output works", {
  schemes <- list(cyl ~ helmert_code,
                  gear ~ orth_polynomial_code)
  glimpse_list <- glimpse_contrasts(mtcars, schemes, return.list = TRUE, verbose = FALSE)

  expect_equal(length(glimpse_list), 2L)
  expect_equal(glimpse_list$contrasts, enlist_contrasts(mtcars, schemes, verbose = FALSE))
})

test_that("One level factor glimpse works", {
  tst <-  data.frame(onelevel = factor('a'),
                     twolevel = factor(c('a','b')))

  glimpse <- glimpse_contrasts(tst, incl.one.levels = TRUE, verbose = FALSE)

  expect_equal(glimpse$explicitly_set, c(FALSE, NA))
  expect_equal(glimpse$factor, c("twolevel","onelevel"))

})

