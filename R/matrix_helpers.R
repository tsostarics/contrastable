.contrasts_to_hypotheses <- function(contrast_matrix, n) {
  n <- nrow(contrast_matrix)
  intercept_matrix <- matrix(c(rep(1,n), contrast_matrix), n)
  solve(t(intercept_matrix))
}

.hypotheses_to_contrasts <- function(hypothesis_matrix) {
  solve(t(hypothesis_matrix))[,-1]
}

# nocov start
# only really used in debugging sessions
.convert_matrix <- function(m) {
  if (nrow(m) == ncol(m))
    return(.hypotheses_to_contrasts(m))

  .contrasts_to_hypotheses(m)
}
# nocov end
