#' Treatment code
#'
#' @description Wrapper around \link[stats]{contr.treatment}. Returns a contrast
#' matrix where comparisons give differences between each comparison level and a
#' baseline reference level, while the intercept equals the first level of the
#' factor. See \link[contrastable]{scaled_sum_code} for a function that centers
#' the intercept on the grand mean while retaining pairwise comparisons from a
#' reference level.
#'
#' @details For n levels of factors, generate a matrix with n-1 comparisons
#' where:
#'
#'  - Reference level = 0
#'  - Comparison level = 1
#'  - All others = 0
#'
#' Note that this function sets the first level (alphabetically) as the
#' reference level while \link[stats]{contr.SAS} sets the LAST level as the
#' reference level. However, in functions like
#' \link[contrastable]{set_contrasts}, and
#' \link[contrastable]{enlist_contrasts}, the reference level is automatically
#' set to be the first level alphabetically.
#'
#' @inherit scaled_sum_code params return
#' @export
#'
#' @examples
#' mydf <- data.frame(
#'   grp = gl(4,5),
#'   resp = c(seq(1, 5), seq(5, 9), seq(10, 14), seq(15, 19))
#' )
#'
#' mydf <- set_contrasts(mydf, treatment_code)
#'
#' lm(resp ~ grp, data = mydf)
#'
treatment_code <- function(n) {
  stats::contr.treatment(n)
}
