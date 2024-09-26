test_that("AsIs method dispatch works", {
  expect_warning(use_contrasts(gl(5,1), I(contr.sum)))

  expect_equal(suppressWarnings(use_contrasts(gl(5,1), I(contr.sum))), contr.sum(5),
               ignore_attr = TRUE)

})

test_that("name method dispatch works with aliasing", {
  aliased_scheme <- sum_code
  contrast_scheme <- rlang::sym("aliased_scheme")

  expect_equal(use_contrasts(gl(5,1), contrast_scheme),
               use_contrasts.name(gl(5,1), contrast_scheme))
})

test_that("name method dispatch works with symbol manipulation", {
  fx_sym <- rlang::sym("sum_code")
  reference_value <- sum_code(3)
  expect_equal(use_contrasts(gl(3,1), fx_sym),
               reference_value,
               ignore_attr = TRUE)

  expect_equal(enlist_contrasts(data.frame(x = gl(3,1)),
                                x ~ fx_sym,
                                verbose = FALSE)[[1]],
               reference_value,
               ignore_attr = TRUE)
})

test_that("Matrix handling works", {
  tstdf <- data.frame(x = gl(3,5))
  cmat <- scaled_sum_code(3)

  expect_equal(enlist_contrasts(tstdf, x ~ scaled_sum_code),
               enlist_contrasts(tstdf, x ~ cmat))


  expect_equal(enlist_contrasts(tstdf, x ~ scaled_sum_code + 2),
               enlist_contrasts(tstdf, x ~ cmat + 2))


  expect_equal(enlist_contrasts(tstdf, x ~ scaled_sum_code + 2 * 1),
               enlist_contrasts(tstdf, x ~ cmat + 2 * 1))

  expect_equal(enlist_contrasts(tstdf, x ~ scaled_sum_code + 2 * 1 | c("a", "b")),
               enlist_contrasts(tstdf, x ~ cmat + 2 * 1 | c("a", "b")))


  expect_equal(c("2", "3"),
               colnames(enlist_contrasts(tstdf, x ~ I(cmat))[[1]]))

})
