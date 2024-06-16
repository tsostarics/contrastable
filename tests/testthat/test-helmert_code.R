test_that("helmert coding values correct", {
  expect_equal(
    helmert_code(4),
    matrix(c(-1 / 2, 1 / 2, 0, 0, -1 / 3, -1 / 3, 2 / 3, 0, -1 / 4, -1 / 4, -1 / 4, 3 / 4),
      nrow = 4
    )
  )
})

test_that("reverse helmert coding values correct", {
  expect_equal(
    reverse_helmert_code(4),
    matrix(rev(c(-1 / 2, 1 / 2, 0, 0, -1 / 3, -1 / 3, 2 / 3, 0, -1 / 4, -1 / 4, -1 / 4, 3 / 4)),
      nrow = 4
    )
  )
})


test_that("contr.helmert works", {
  tstdata <- mtcars
  tstdata$carb <- factor(tstdata$carb)
  tstdata2 <- tstdata

  contrasts(tstdata$carb) <- contr.helmert(6)

  carb_contrasts <- enlist_contrasts(tstdata2, carb ~ contr.helmert)[[1]]

  expect_equal(contrasts(tstdata$carb), carb_contrasts, ignore_attr = TRUE)
})

test_that("helmert_code auto scales contrasts", {
  set.seed(111)
  tstdata <-
    data.frame(
      grp = factor(rep(c("a", "b", "c", "d"), each = 50)),
      val = c(
        rnorm(50, 1, .001),
        rnorm(50, 5, .001),
        rnorm(50, 11, .001),
        rnorm(50, 17, .001)
      )
    )

  grp_means <- vapply(split(tstdata, ~grp), \(d) mean(d$val), 1.0, USE.NAMES = TRUE)

  set.seed(111)
  unscaled_coefs <- coef(lm(val ~ grp, data = tstdata, contrasts = enlist_contrasts(tstdata, grp ~ contr.helmert)))

  set.seed(111)
  scaled_coefs <- coef(lm(val ~ grp, data = tstdata, contrasts = enlist_contrasts(tstdata, grp ~ helmert_code)))

  expect_true(((grp_means["b"] - grp_means["a"]) - scaled_coefs[2]) < 1e-10)
  expect_true(((grp_means["c"] - mean(c(grp_means["b"], grp_means["a"])) - scaled_coefs[3]) < 1e-10))
  expect_true(((grp_means["d"] - mean(c(grp_means["c"], grp_means["b"], grp_means["a"])) - scaled_coefs[4]) < 1e-10))

  expect_true(all(unscaled_coefs * (1:4) - scaled_coefs < 1e-12))
})
