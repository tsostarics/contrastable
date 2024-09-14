test_that("Intercept interpretation works", {
  expect_equal(interpret_intercept(contr.treatment(5)),
               "mean(1)",
               ignore_attr = TRUE)

  expect_equal(interpret_intercept(scaled_sum_code(5)),
               "grand mean",
               ignore_attr = TRUE)

  expect_equal(interpret_intercept(helmert_code(5)),
               "grand mean",
               ignore_attr = TRUE)

  expect_warning(interpret_intercept(contr.poly(5)[, 1:3]),
                 "4 columns but found 3")
  expect_equal(suppressWarnings(interpret_intercept(contr.poly(5)[, 1:3])),
               "grand mean",
               ignore_attr = TRUE)
})

test_that("Glimpse works", {
  tstdf <- mtcars
  tstdf$cyl <- factor(tstdf$cyl)
  tst <- suppressWarnings(glimpse_contrasts(tstdf,
                                            carb ~ contr.poly - 3:5,
                                            gear ~ scaled_sum_code + 5,
                                            minimal = FALSE,
                                            verbose = FALSE))

  expect_equal(tst$factor, c("carb", "gear", "cyl"))
  expect_equal(tst$n, c(6, 3, 3),
               ignore_attr = TRUE) # need unname
  expect_equal(tst$scheme,
               c("contr.poly", "scaled_sum_code", "contr.treatment"),
               ignore_attr = TRUE) # need unname
  expect_equal(tst$reference, c(NA, "5", "4"),
               ignore_attr = TRUE) # need unname
  expect_equal(tst$intercept, c("grand mean", "grand mean", "mean(4)"),
               ignore_attr = TRUE)
  expect_equal(tst$orthogonal, c(TRUE, FALSE, FALSE))
  expect_equal(tst$centered, c(TRUE, TRUE, FALSE))
  expect_equal(tst$dropped_trends, c("3,4,5", NA, NA))
  expect_equal(tst$explicitly_set, c(TRUE, TRUE, FALSE))
})

test_that("Glimpse with variables works", {
  tstdf <- mtcars
  a <- 3
  b <- 4
  c <- 5
  tstdf$vs <- factor(tstdf$vs)
  mat <- scaled_sum_code(3)
  tst <- suppressWarnings(glimpse_contrasts(tstdf,
                                            carb ~ contr.poly - a:c,
                                            gear ~ mat + c * b,
                                            cyl ~ scaled_sum_code + b * b,
                                            minimal = FALSE,
                                            show_all_factors = TRUE,
                                            verbose = FALSE))

  expect_equal(tst$scheme, c("contr.poly", "custom", "scaled_sum_code", "contr.treatment"))
  expect_equal(tst$intercept, c("grand mean", "mean(4)", "mean(4)", "mean(0)"),
               ignore_attr = TRUE)
  expect_equal(tst$dropped_trends, c("3,4,5", NA, NA, NA))
  expect_equal(tst$explicitly_set, c(TRUE, TRUE, TRUE, FALSE))
})

test_that("Append namespace to scheme names", {
  expect_equal(
    .add_namespace(
      c("contr.helmert",
        "contr.sum",
        "scaled_sum_code")
    ),
    c("stats::contr.helmert",
      "stats::contr.sum",
      "contrastable::scaled_sum_code")
  )

  expect_equal(
    suppressWarnings(glimpse_contrasts(mtcars,
                                       cyl ~ contrastable:::sum_code,
                                       verbose = FALSE)[['scheme']]),
    "contrastable:::sum_code"
  )

  expect_equal(
    suppressWarnings(glimpse_contrasts(mtcars,
                                       cyl ~ sum_code,
                                       verbose = FALSE,
                                       add_namespace = TRUE)[['scheme']]),
    "contrastable::sum_code"
  )
})

test_that("Nonexistant namespace returns original name", {
  foo <- helmert_code

  expect_equal("foo", .add_namespace("foo"))
})


test_that("Glimpse shows namespace when requested", {
  gtbl <- suppressWarnings(glimpse_contrasts(mtcars, cyl ~ sum_code, verbose = FALSE,
                                             add_namespace = TRUE))

  expect_equal("contrastable::sum_code", gtbl$scheme)
})

test_that("Returning list works fine", {
  gtbl <- suppressWarnings(glimpse_contrasts(mtcars, cyl ~ sum_code, verbose = FALSE,
                                             return_list = TRUE))

  expect_equal(c("glimpse", "contrasts"), names(gtbl))
})


test_that("Warning with non default contrasts works", {
  tstdf <- dplyr::mutate(mtcars,
                         gear = factor(gear),
                         cyl = factor(cyl),
                         carb = ordered(carb))
  tstdf <- set_contrasts(tstdf,
                         cyl ~ contr.sum,
                         carb ~ raw_polynomial_code,
                         verbose = FALSE)

  expect_warning(.glimpse_default_factors(tstdf),
                 regexp = "Glimpse table may be unreliable")
})

test_that("List output works", {
  schemes <- list(
    cyl ~ helmert_code,
    gear ~ orth_polynomial_code
  )
  my_data <- set_contrasts(mtcars, schemes, verbose = FALSE)
  glimpse_list <- glimpse_contrasts(my_data,
                                    schemes,
                                    return_list = TRUE,
                                    verbose = FALSE)

  expect_equal(length(glimpse_list), 2L)
  expect_equal(glimpse_list$contrasts,
               enlist_contrasts(mtcars, schemes, verbose = FALSE))
})

test_that("One level factor glimpse works", {
  tst <- data.frame(
    onelevel = factor("a"),
    twolevel = factor(c("a", "b"))
  )

  glimpse <-
    glimpse_contrasts(tst, show_one_level_factors = TRUE, verbose = FALSE)

  expect_equal(glimpse$explicitly_set, c(FALSE, NA))
  expect_equal(glimpse$factor, c("twolevel", "onelevel"))
})

test_that(".warn_if_mismatched_contrasts throws correct warnings", {

  my_data <- mtcars
  my_data$cyl <- factor(my_data$cyl)
  clist <- list(cyl ~ helmert_code)

  # (1) no warning, default
  expect_no_warning(glimpse_contrasts(my_data))

  # (2) warning that contrast matrices dont match
  expect_warning(glimpse_contrasts(my_data, clist),
                 "Contrasts for factors in `my_data` don't match matrices in formulas:[ \n]+- cyl") # nolint


  # (3) warning that labels dont match (but matrices are fine)
  contrasts(my_data$cyl) <- helmert_code(3)
  expect_warning(glimpse_contrasts(my_data, clist),
                 "Comparison labels for contrasts in `my_data` don't match:[ \n]+- cyl	\\(expected `<6, <8` but found ``\\)") # nolint

  # (4) no warnings so long as the contrasts in clist ARE set to my_data
  my_data <- set_contrasts(my_data, clist)
  expect_no_warning(glimpse_contrasts(my_data, clist))

  # (5) warning that carb isn't a factor
  clist <- list(cyl ~ helmert_code, carb ~ helmert_code)
  expect_warning(glimpse_contrasts(my_data, clist, verbose = FALSE),
                 "These vars in `my_data` are not factors:[ \n]+- carb")

  my_data <- mtcars
  my_data$am <- factor(my_data$am)
  # (6) check that the manually set labels show up correctly
  expect_warning(glimpse_contrasts(my_data,
                                   am ~ treatment_code + 0 | c("diffA")),
                 c("\\(expected `diffA` but found `1`\\)"))

  clist <- list(cyl ~ helmert_code,
                am ~ treatment_code + 0 | c("diffA"))
  my_data$cyl <- factor(my_data$cyl)
  my_data$am <- factor(my_data$am)

  # (7) check that symbol handling works correctly
  expect_warning(glimpse_contrasts(my_data, clist),
                 "my_data <- set_contrasts\\(my_data, clist\\)")

  # (8) check that formula formatting works correctly
  expect_warning(glimpse_contrasts(my_data,
                                   cyl ~ helmert_code,
                                   am ~ treatment_code + 0 | c("diffA")),
                 "my_data <- set_contrasts\\(my_data,[ \n]+cyl ~ helmert_code,am ~ treatment_code + 0 | c\\(\"diffA\"\\)") # nolint

})


test_that("Reset label to ??? if nondefault", {
  tstdf <- mtcars
  tstdf$cyl <- factor(tstdf$cyl)
  contrasts(tstdf$cyl) <- sum_code(3)

  tst <- suppressWarnings(glimpse_contrasts(tstdf,
                                            verbose = FALSE))

  expect_equal(tst$scheme[[1]], "???")
})

test_that("Reference levels reported correctly", {
  tstdf <-
    data.frame(a = factor(c("a", "b", "c")), # contr.treatment default ref = a
               b = factor(c("a", "b", "c")), # will make ref = b
               c = factor(c("a", "b", "c"))) # will be NA via helmert coding

  tst <- suppressWarnings(glimpse_contrasts(tstdf,
                                            b ~ sum_code + "b",
                                            c ~ helmert_code,
                                            verbose = FALSE))

  expect_equal(tst$reference, c("b", NA, "a"))
})


test_that("Readme example unchanged", {
  contrast_schemes <- list(
    cyl ~ scaled_sum_code + 6,
    carb ~ helmert_code,
    vs ~ treatment_code + 1
  )

  my_data <- mtcars
  my_data$gear <- ordered(my_data$gear) # Set as ordered factor in dataframe

  # Get information about our contrasts, even those we didn't explicitly set
  # (gear is ordered, and so uses contr.poly by default)
  expect_warning(glimpse_contrasts(my_data,
                                   contrast_schemes,
                                   add_namespace = TRUE,
                                   show_all_factors = TRUE,
                                   verbose = FALSE))

  expect_snapshot(
    glimpse_contrasts(
      set_contrasts(my_data,
                    contrast_schemes,
                    verbose = FALSE),
      contrast_schemes,
      add_namespace = TRUE,
      show_all_factors = TRUE,
      verbose = FALSE))
})
