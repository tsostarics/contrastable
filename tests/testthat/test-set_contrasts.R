test_that("set contrast equivalent to manual contrast codes", {
  tst_df <- set_contrasts(dplyr::mutate(mtcars, gear = factor(gear)),
                          gear ~ helmert_code,
                          cyl ~ contr.sum,
                          verbose = FALSE)

  comparison_df <- dplyr::mutate(mtcars, gear = factor(gear), cyl = factor(cyl))
  contrasts(comparison_df$cyl) <- use_contrasts(comparison_df$cyl, contr.sum)
  contrasts(comparison_df$gear) <- use_contrasts(comparison_df$gear, helmert_code)

  columns_equivalent <-
    vapply(names(tst_df),
         function(x)
           all(tst_df[[x]] == comparison_df[[x]]) & class(tst_df[[x]]) == class(comparison_df[[x]]),
         TRUE)

  expect_true(all(columns_equivalent))

  })

test_that("Ignoring dropped levels in orthogonal polynomial contrasts", {
  expect_warning(set_contrasts(mtcars, carb ~ contr.poly - 4:6, verbose = FALSE),
                 regexp = r"(Cannot drop trends .+ formula\.)")
  expect_warning(aaa <- set_contrasts(mtcars, carb ~ contr.poly - 4:6, gear ~ contr.poly - 2:3, verbose = FALSE),
                 regexp = r"(Cannot drop trends .+ formulas\.)")
  expect_warning(set_contrasts(mtcars, gear ~ contr.sum - 2:3, verbose = FALSE),
                 regexp = "used in invalid context")


  expect_equal(contrasts(aaa$carb), contr.poly(6), ignore_attr = TRUE)
  expect_equal(contrasts(aaa$gear), contr.poly(3), ignore_attr = TRUE)

})
# mutate(aaa, across(.cols = carb,
#                    .fns = \(x) {
#                      newcols <- model.matrix(~carb)[,2:3]
#                      colnames(newcols) <- gsub("^carb\\.","",colnames(newcols))
#                      return(newcols)
#                      },
#                    .names = list("linear","quadratic")
#                    )
#        )
