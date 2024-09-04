test_that("AsIs method dispatch works", {
  expect_warning(use_contrasts(gl(5,1), I(contr.sum)))

  expect_equal(suppressWarnings(use_contrasts(gl(5,1), I(contr.sum))), contr.sum(5))

})

test_that("name method dispatch works", {
  aliased_scheme <- sum_code
  contrast_scheme <- rlang::sym("aliased_scheme")

  expect_equal(use_contrasts(gl(5,1), contrast_scheme),
               use_contrasts.name(gl(5,1), contrast_scheme))
})
