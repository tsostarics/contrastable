#' Treatment code
#'
#' Wrapper around R's built in function
#'
#' For n levels of factors, generate a matrix with n-1 comparisons where:
#' Reference level = 0
#' Comparison level = 1
#'
#' @param n number of levels for this factor
#'
#' @return A matrix of treatment coded contrasts, unlabeled
#' @export
treatment_code <- function(n) {
  stats::contr.treatment(n)
}
