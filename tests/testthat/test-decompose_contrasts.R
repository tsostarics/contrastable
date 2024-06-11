test_that("Decomposing contrasts into new columns works", {
  tst <- mtcars
  tst$carb <- factor(mtcars$carb)
  tst$cyl  <- factor(mtcars$cyl)

  dec_test1 <- decompose_contrasts(tst, "cyl")
  dec_test2 <- decompose_contrasts(tst, "cyl", list(cyl = c("6-4", "8-4")))
  dec_test3 <- decompose_contrasts(tst,
                                   c('cyl', 'carb'),
                                   list(cyl = c("6-4", "8-4"),
                                        carb = c("a", "b", "c", "d", "e"))
                                   )
  expect_true(all(c("cyl6", "cyl8") %in% colnames(dec_test1)))
  expect_true(all(c("cyl6-4", "cyl8-4") %in% colnames(dec_test2)))
  expect_true(all(c("cyl6-4", "cyl8-4", paste0("carb", c("a", "b", "c","d","e")))
                  %in% colnames(dec_test3)))
})
