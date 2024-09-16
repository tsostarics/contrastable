test_that("Polynomial verification works", {
  cmat <- contr.poly(5)
  cmat2 <- unname(contr.poly(5))
  tstdf <- data.frame(x = gl(5,1),
                      y = gl(100,1))

  poly_results1 <- enlist_contrasts(tstdf, x ~ cmat | c('a', 'b', 'c', 'd'),
                                    verbose = FALSE)
  poly_results2 <- enlist_contrasts(tstdf, x ~ cmat, verbose = FALSE)
  poly_results3 <- enlist_contrasts(tstdf, x ~ cmat2, verbose = FALSE)

  expect_equal(c('a', 'b', 'c', 'd'), colnames(poly_results1[['x']]))
  expect_equal(poly_results2, poly_results3)


})

test_that("Error with too many levels", {
  tstdf <- data.frame(a = gl(100,1),
                      b = gl(100,1,ordered = TRUE))

  expect_error(enlist_contrasts(tstdf, a ~ contr.poly, verbose = FALSE),
               "cannot be represented accurately enough")
  expect_error(set_contrasts(tstdf, a ~ contr.poly, verbose = FALSE),
               "cannot be represented accurately enough")
  expect_error(glimpse_contrasts(tstdf,show_all_factors = TRUE, verbose = FALSE),
               "Convert `b` to unordered")
})
