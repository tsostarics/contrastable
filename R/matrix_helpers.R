#' Convert contrast matrix to hypothesis matrix
#'
#' @param contrast_matrix Contrast matrix
#'
#' @return Hypothesis matrix
#' @keywords internal
#' @noRd
.contrasts_to_hypotheses <- function(contrast_matrix) {
  n <- nrow(contrast_matrix)
  intercept_matrix <- matrix(c(rep(1, n), contrast_matrix), n)
  solve(t(intercept_matrix))
}

#' Convert hypothesis matrix to contrast matrix
#'
#' @param hypothesis_matrix Hypothesis matrix
#'
#' @return Hypothesis matrix
#' @keywords internal
#' @noRd
.hypotheses_to_contrasts <- function(hypothesis_matrix) {
  result <- solve(t(hypothesis_matrix))[, -1]

  # Should only happen for 2 level factors
  if (!is.matrix(result))
    result <- matrix(result)

  result
}

# nocov start
#' Convert between hypothesis and contrast matrices
#'
#' Quick debugging helper that runs the correct matrix conversion based on
#' the dimensions of the matrix
#'
#' @param m matrix
#'
#' @return Hypothesis matrix if passed contrasts, contrast matrix if passed
#' hypotheses
#' @keywords internal
#' @noRd
.convert_matrix <- function(m) {
  if (nrow(m) == ncol(m)) {
    return(.hypotheses_to_contrasts(m))
  }

  .contrasts_to_hypotheses(m)
}
# nocov end
