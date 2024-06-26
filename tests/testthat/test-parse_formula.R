test_that("Formula validation works", {
  f_onesided <- ~gear
  f_biglhs1 <- gear + 2 ~ contr.poly
  f_biglhs2 <- 2 + gear ~ contr.poly
  f_toomanyops <- gear ~ contr.poly + "a" + 2

  f_firstterm1 <- gear ~ -1 + contr.poly
  f_firstterm2 <- gear ~ 1 + contr.poly
  f_labelorder <- gear ~ contr.poly | c("t1", "t2") - 1

  expect_error(.check_if_valid_formula(f_onesided),
    regexp = "two-sided"
  )
  expect_error(.check_if_valid_formula(f_biglhs1),
    regexp = "1 variable name on left"
  )
  expect_error(.check_if_valid_formula(f_biglhs2),
    regexp = "1 variable name on left"
  )
  expect_error(.check_if_valid_formula(f_toomanyops),
    regexp = "may only use .+ once"
  )

  expect_error(.check_if_valid_formula(f_firstterm1),
    regexp = "in right hand side must be a symbol"
  )
  expect_error(.check_if_valid_formula(f_firstterm2),
    regexp = "in right hand side must be a symbol"
  )
  expect_error(.check_if_valid_formula(f_labelorder),
    regexp = "last operator"
  )
})

test_that("Formula validation with matrix calls works", {
  f_mat1 <- gear ~ matrix(c(
    0.75, -0.25, -0.25,
    -0.25 / 4, -0.25 * 2, 0.75^2.3,
    -0.25, -0.25, -0.25 %in% c(1, 2, 3),
    -0.25, 0.75, -0.25
  ) |> abs(), nrow = 4) + 4
  char_formula <- deparse1(f_mat1)
  expect_true(.check_if_valid_formula(f_mat1))
})

test_that("Formula parsing with matrix call works", {
  f_mat1 <- gear ~ matrix(c(
    0.75, -0.25, -0.25,
    -0.25 / 4, -0.25 * 2, 0.75^2.3,
    -0.25, -0.25, -0.25 %in% c(1, 2, 3),
    -0.25, 0.75, -0.25
  ) |> abs(), nrow = 4) + 4 * 2 - 3:5
  params <- .parse_formula(f_mat1)
  params[["code_by"]] <- round(eval(params[["code_by"]]), 4)
  ref_mat <-
    matrix(c(
      0.75, 0.5, 0, 0.25,
      0.516, 0.25, 0.25, 0.25,
      0.75, 0.0625, 0.25, 0.25
    ), nrow = 4, byrow = TRUE)

  reference <-
    list(
      "factor_col" = sym("gear"),
      "code_by" = ref_mat,
      "reference_level" = 4,
      "intercept_level" = 2,
      "drop_trends" = str2lang("3:5"),
      "labels" = NULL,
      "as_is" = FALSE
    )

  expect_equal(params, reference, ignore_attr = TRUE)
})

test_that("Formula parsing with functions works", {
  f_allops <- gear ~ contr.poly + "a" * "b" - a:b
  params_allops <- .parse_formula(f_allops)
  reference_allops <-
    list(
      "factor_col" = sym("gear"),
      "code_by" = sym("contr.poly"),
      "reference_level" = "a",
      "intercept_level" = "b",
      "drop_trends" = str2lang("a:b"),
      "labels" = NULL,
      "as_is" = FALSE
    )

  f_noops <- gear ~ contr.poly
  params_noops <- .parse_formula(f_noops)
  reference_noops <-
    list(
      "factor_col" = sym("gear"),
      "code_by" = sym("contr.poly"),
      "reference_level" = NA,
      "intercept_level" = NA,
      "drop_trends" = NA,
      "labels" = NULL,
      "as_is" = FALSE
    )

  expect_equal(params_allops, reference_allops)
  expect_equal(params_noops, reference_noops)
})
