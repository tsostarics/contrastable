test_that("Decomposing interactions works", {
  tst <- set_contrasts(mtcars[1:5, ],
                       cyl ~ scaled_sum_code,
                       carb ~ scaled_sum_code,
                       verbose = FALSE)

  output <- decompose_contrasts(tst, ~ cyl * carb)

  expect_equal(as.character(MASS::fractions(output$`cyl6:carb2`)),
               c("-2/9", "-2/9", "1/9", "-2/9", "-2/9"))

})

test_that("Decomposing contrasts into new columns works", {
  tst <- mtcars
  tst$carb <- factor(mtcars$carb)
  tst$cyl <- factor(mtcars$cyl)
  contrasts(tst$cyl) <- scaled_sum_code(3)
  colnames(contrasts(tst$cyl)) <- c("6-4", "8-4")

  test_extract_rename <- decompose_contrasts(tst, ~ cyl)
  test_extract_multifactors <- decompose_contrasts(tst, ~ cyl + carb)
  test_extract_remove_original <-
    decompose_contrasts(tst, ~ cyl + carb, remove_original = TRUE)

  test_extract_intercept <-
    decompose_contrasts(tst, ~ cyl + carb, remove_intercept = FALSE)

  expect_true(all(c("cyl6-4", "cyl8-4") %in% colnames(test_extract_rename)))
  expect_true(all(c("cyl6-4", "cyl8-4", paste0("carb", c(2, 3, 4, 6, 8))) %in%
                    colnames(test_extract_multifactors)))
  expect_true(all(!c("cyl", "carb") %in% colnames(test_extract_remove_original)))
  expect_true("(Intercept)" %in% colnames(test_extract_intercept))

})
