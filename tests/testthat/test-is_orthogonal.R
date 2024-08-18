test_that("Orthogonality checks are correct", {
  tst_df <- data.frame(
    a = gl(5, 1),
    b = gl(5, 1),
    c = gl(5, 1),
    d = gl(5, 1),
    e = gl(5, 1),
    f = gl(5, 1),
    g = gl(5, 1),
    h = gl(5, 1)
  )
  orth_vector <-
    is_orthogonal(
      enlist_contrasts(
        tst_df,
        a ~ contr.treatment,
        b ~ scaled_sum_code,
        c ~ contr.poly,
        d ~ helmert_code,
        e ~ backward_difference_code,
        f ~ backward_difference_code * 3,
        g ~ reverse_helmert_code,
        h ~ reverse_helmert_code * 3
      ),
      USE.NAMES = TRUE
    )

  references <-
    c(
      "a" = FALSE,
      "b" = FALSE,
      "c" = TRUE,
      "d" = TRUE,
      "e" = FALSE,
      "f" = FALSE,
      "g" = TRUE,
      "h" = TRUE
    )

  expect_equal(orth_vector, references)
})

test_that("Two level factors return NA", {
  tst_df <- data.frame(
    a = gl(2, 1),
    b = gl(2, 1),
    c = gl(2, 1)
  )
  orth_vector <-
    is_orthogonal(
      enlist_contrasts(
        tst_df,
        a ~ contr.treatment,
        b ~ contr.sum,
        c ~ scaled_sum_code,
      )
    )

  expect_equal(orth_vector, c(NA, NA, NA), ignore_attr = TRUE)
})
