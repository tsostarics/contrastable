test_that("set contrast equivalent to manual contrast codes", {
  tst_df <-
    set_contrasts(mtcars,
                  gear ~ helmert_code,
                  cyl ~ contr.sum,
                  verbose = FALSE)

  comparison_df <- mtcars
  comparison_df$gear <- factor(comparison_df$gear)
  comparison_df$cyl <- factor(comparison_df$cyl)

  contrasts(comparison_df$cyl) <-
    use_contrasts(comparison_df$cyl, contr.sum)
  contrasts(comparison_df$gear) <-
    use_contrasts(comparison_df$gear, helmert_code)

  columns_equivalent <-
    vapply(
      names(tst_df),
      function(x) {
        all(tst_df[[x]] == comparison_df[[x]]) &
          class(tst_df[[x]]) == class(comparison_df[[x]])
      },
      TRUE
    )

  expect_true(all(columns_equivalent))
})

test_that("Ignoring dropped levels in orthogonal polynomial contrasts", {
  expect_warning(set_contrasts(mtcars,
                               carb ~ contr.poly - 4:6,
                               verbose = FALSE),
                 regexp = r"(Cannot use ... with set_contrasts.)"
  )
  testdf <- suppressWarnings(set_contrasts(mtcars,
                                           carb ~ contr.poly - 4:6,
                                           verbose = FALSE))

  expect_warning(set_contrasts(mtcars,
                               gear ~ contr.sum - 2:3,
                               verbose = FALSE),
                 regexp = "only be used with polynomial contrasts"
  )


  expect_equal(contrasts(testdf$carb), contr.poly(6), ignore_attr = TRUE)
})

test_that("Contrasts print when asked for", {
  expect_snapshot_output(
    invisible(
      set_contrasts(mtcars,
                    gear ~ helmert_code,
                    print_contrasts = TRUE,
                    verbose = FALSE)
    )
  )
})


test_that("Namespaced call works", {
  result <- contrastable::set_contrasts(mtcars,
                                        carb ~ contr.sum,
                                        verbose = FALSE)
  expect_true(!is.null(result))
})
