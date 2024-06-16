#' Sum code
#'
#' Wrapper around R's built in function
#'
#' For n levels of factors, generate a matrix with n-1 comparisons where:
#' Reference level = -1
#' Comparison level = 1
#' All others = 0
#'
#' @param n number of levels for this factor
#'
#' @return A matrix of sum coded contrasts, unlabeled
#' @export
sum_code <- function(n) {
  if (n == 2) {
    return(-stats::contr.sum(n))
  }

  stats::contr.sum(n)[c(n, seq_len(n - 1)), ]
}
