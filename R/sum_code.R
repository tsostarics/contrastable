#' Sum code
#'
#' Wrapper around R's built in function
#'
#' For n levels of factors, generate a matrix with n-1 comparisons where:
#' Reference level = -1
#' Comparison level = 1
#' All others = 0
#'
#' @param n_levels number of levels for this factor
#'
#' @return A matrix of sum coded contrasts, unlabeled
#' @export
sum_code <- function(n_levels) {
  contr.sum(n_levels)
}
