test_that("droplevels warning works", {
  tstdata <- head(data.frame(x = gl(5, 1), y = 1:5), 3)

  expect_warning(.droplevels_as_needed(tstdata, "x", verbose = TRUE), "x \\(2\\)")
})


test_that("unneeded dropping handles properly", {
  tstdata <- data.frame(x = gl(5, 1), y = 1:5)

  expect_equal(tstdata, .droplevels_as_needed(tstdata, "x", verbose = TRUE))
})


test_that("ordered handling works", {
  tstdata <- head(data.frame(x = gl(5, 1), y = gl(5, 1, ordered = TRUE)), 3)

  expect_warning(.droplevels_as_needed(tstdata, c("x", "y"), verbose = TRUE),
                 "x \\(2\\).+?y \\(2\\)")
})

test_that("multiple messages with one level factors", {
  tstdata <- head(data.frame(x = gl(6, 1), y = gl(3, 2)), 2)

  tst <- tryCatch(,
                  error = \(e) e,
                  warning = \(w) w,
                  message = \(m) m)
  expect_warning(
    expect_warning(enlist_contrasts(tstdata, x + y ~ sum_code),
                   "undefined for factors with only one"),
  "Dropping missing levels"
  )
})
