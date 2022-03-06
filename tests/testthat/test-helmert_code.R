test_that("reverse helmert coding values correct", {
  expect_equal(reverse_helmert_code(4),
               matrix(c(-1/2, 1/2, 0, 0, -1/3, -1/3, 2/3, 0, -1/4, -1/4, -1/4, 3/4),
                      nrow=4))
})

test_that("helmert coding values correct", {
  expect_equal(helmert_code(4),
               matrix(rev(c(-1/2, 1/2, 0, 0, -1/3, -1/3, 2/3, 0, -1/4, -1/4, -1/4, 3/4)),
                      nrow=4))
})

test_that("helmert coding labels correct", {
  expect_equal(2 * 2, 4)
})

test_that("reverse helmert coding labels correct", {
  expect_equal(2 * 2, 4)
})

test_that("contr.helmert works", {
  tstdata <- mtcars
  tstdata$carb <- factor(tstdata$carb)
  tstdata2 <- tstdata

  contrasts(tstdata$carb) <- contr.helmert(6)

  carb_contrasts <- enlist_contrasts(tstdata2, carb ~ contr.helmert)[[1]]

  expect_equal(contrasts(tstdata$carb), carb_contrasts, ignore_attr = TRUE)
})
