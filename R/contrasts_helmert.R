#' Use helmert coding
#'
#' @description R's [stats::contr.helmert()] function is unscaled, meaning
#' that you need to scale the coefficients of a model fit to get the actual
#' comparisons of interest. This version will automatically scale the contrast
#' matrix such that the coefficients are the expected scaled values.
#'
#' @details Helmert coding compares each level to the total mean of all levels
#' that have come before it. Differs from backward difference coding, which
#' compares only pairs of levels (not a level to a cumulative mean of levels)
#'
#' Example interpretation for a 4 level factor:
#'
#'  - Intercept = Grand mean (mean of the means of each level)
#'  - grp2 = mean(grp2) - mean(grp1)
#'  - grp3 = mean(grp3) - mean(grp1, grp2)
#'  - grp4 = mean(grp4) - mean(grp1, grp2, grp3)
#'
#' @inherit scaled_sum_code params return
#' @export
#'
#' @examples
#'
#' mydf <- data.frame(
#'   grp = gl(4,5),
#'   resp = c(seq(1, 5), seq(5, 9), seq(10, 14), seq(15, 19))
#' )
#'
#' mydf <- set_contrasts(mydf, grp ~ helmert_code)
#' lm(resp ~ grp, data = mydf)
helmert_code <- function(n) {
  apply(unname(stats::contr.helmert(n)), 2L, function(x) x / sum(x != 0))
}

#' Use reverse helmert coding
#'
#' @description
#' Reverse helmert coding is the same concept as helmert coding, but the order
#' of the groupings is reversed. See also \link[contrastable]{helmert_code}.
#'
#' @details
#' Reverse helmert coding compares each level to the total mean of all levels
#' that come after it. Differs from forward difference coding, which only
#' compares pairs of levels (not a level to a cumulative mean of levels).
#'
#' Example interpretation for a 4 level factor:
#'
#'  - Intercept = Grand mean (mean of the means of each level)
#'  - grp1 = mean(grp4, grp3, grp2) - grp(1)
#'  - grp2 = mean(grp4, grp3) - mean(grp2)
#'  - grp3 = mean(grp3) - mean(grp4)
#'
#' @inherit scaled_sum_code params return
#'
#' @export
#' @examples
#'
#' mydf <- data.frame(
#'   grp = gl(4,5),
#'   resp = c(seq(1, 5), seq(5, 9), seq(10, 14), seq(15, 19))
#' )
#'
#' mydf <- set_contrasts(mydf, grp ~ reverse_helmert_code)
#' lm(resp ~ grp, data = mydf)
reverse_helmert_code <- function(n) {
  matrix(rev(helmert_code(n)), nrow = n)
}
