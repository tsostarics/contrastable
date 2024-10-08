test_that("Treatment_code and contr.treatment are equivalent", {
  tst <- data.frame(fac = factor(c("a", "b", "c")))
  ref_mat <- matrix(c(1, 0, 0, 0, 0, 1), nrow = 3)
  colnames(ref_mat) <- c("a", "c")
  rownames(ref_mat) <- c("a", "b", "c")

  ref_cont <-
    enlist_contrasts(tst, fac ~ contr.treatment + "b", verbose = FALSE)
  test_cont <-
    enlist_contrasts(tst, fac ~ treatment_code + "b", verbose = FALSE)

  expect_equal(ref_cont, test_cont)
  expect_equal(test_cont[[1L]], ref_mat)
})
