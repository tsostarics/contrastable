#' Orthogonal Polynomial code
#'
#' Wrapper around R's built in function
#'
#' For n levels of factors where k in 1:n, generate a matrix with n-1 comparisons where each
#' comparison looks for a polynomial trend of degree k where each polynomial
#' is independent of the others.
#'
#' @param n_levels number of levels for this factor
#'
#' @return A matrix of orthogonal polynomial contrasts
#' @export
orth_polynomial_code <- function(n_levels) {
  stats::contr.poly(n_levels)
}

#' Raw Polynomial code
#'
#' Make raw polynomial contrast, rather than orthogonal ones. Normally you
#' would use orthogonal polynomials, so make sure this is what you want. Using
#' raw polynomials may increase the collinearity in your model, especially with
#' higher levels.
#'
#' For n levels of factors where k in 1:n, generate a matrix with n-1 comparisons where each
#' comparison looks for a polynomial trend of degree k where each polynomial
#' may be correlated with the others. Normally you would use orthogonal polynomials.
#'
#' @param n_levels number of levels for this factor
#'
#' @return A matrix of raw polynomial contrasts
#' @export
#'
#' @importFrom stats poly
raw_polynomial_code <- function(n_levels) {
  contrmat <- stats::poly(seq_len(n_levels), n_levels - 1, raw = TRUE)
  attr(contrmat, "degree") <- NULL
  contrmat
}


.is_polynomial_scheme <- function(scheme) {
  # maybe include a check on the numbers to handle raw passed matrices
  checks <- list(stats::contr.poly, orth_polynomial_code, raw_polynomial_code,
                 'contr.poly', 'orth_polynomial_code', 'raw_polynomial_code')

  any(vapply(checks, function(x) identical(x, scheme), FUN.VALUE = TRUE))
}
