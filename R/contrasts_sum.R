#' Sum code
#'
#' @description
#' Wrapper around \link[stats]{contr.sum}, but ensures that the reference level
#' is the first level alphabetically, not the last. Returns a contrast matrix
#' where comparisons give differences between comparison levels and the grand
#' mean.
#'
#' @details
#' For n levels of factors, generate a matrix with n-1 comparisons where:
#'
#'  - Reference level = -1
#'  - Comparison level = 1
#'  - All others = 0
#'
#'
#' Example interpretation for a 4 level factor:
#'
#'  - Intercept = Grand mean (mean of the means of each level)
#'  - grp2 = grp2 - mean(grp4, grp3, grp2, grp1)
#'  - grp3 = grp3 - mean(grp4, grp3, grp2, grp1)
#'  - grp4 =  grp4 - mean(grp4, grp3, grp2, grp1)
#'
#' Note that when n = 2, the coefficient estimate is half of the difference
#' between the two levels. But, this coincidence does not hold when the number
#' of levels is greater than 2.
#'
#' @inherit scaled_sum_code params return
#' @export
sum_code <- function(n) {
  if (n == 2) {
    return(-stats::contr.sum(n))
  }

  stats::contr.sum(n)[c(n, seq_len(n - 1)), ]
}
