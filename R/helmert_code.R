#' Reverse helmert code
#'
#' R's contr.helmert function actually implements reverse helmert coding, this
#' extension scales the matrix such that a 1 unit increase corresponds to a
#' change in factor level.
#'
#' Reverse helmert coding compares each level to the total mean of all levels
#' that have come before it. Differs from backward difference coding, which
#' compares only pairs of levels (not a level to a cumulative mean of levels)
#'
#' Example interpretation for a 4 level factor:
#' \itemize{
#' \item Intercept = Grand mean (mean of the means of each level)
#' \item grp2 = mean(grp2) - mean(grp1)
#' \item grp3 = mean(grp3) - mean(grp1, grp2)
#' \item grp4 = mean(grp4) - mean(grp1, grp2, grp3)
#'}
#' @param n Number of levels in the factor
#'
#' @return Contrast matrix
#' @export
#'
#' @examples
#'
#' mydf <- data.frame(
#'    grp = factor(c(rep("F1",5),rep("F2",5),rep("F3",5),rep("F4",5))),
#'    resp = c(seq(1,5), seq(5,9), seq(10,14), seq(15,19))
#' )
#'
#' mydf %>%
#'   dplyr::group_by(grp) %>%
#'   dplyr::summarize(mu = mean(resp)) %>%
#'   dplyr::ungroup() %>%
#'   dplyr::mutate(grand_mean = mean(mu))
#'
#' summary(lm(resp ~ grp,
#'            data = mydf,
#'            contrasts = enlist_contrasts(mydf, grp ~ reverse_helmert_code)))
reverse_helmert_code <- function(n){
  apply(unname(stats::contr.helmert(n)), 2L, function(x) x / sum(x != 0))
}

#' Helmert code
#'
#' Since R's contr.helmert implements reverse helmert coding, we can get
#' regular helmert coding with reversing the reverse helmert matrix.
#'
#' Helmert coding compares each level to the total mean of all levels that come
#' after it. Differs from forward difference coding, which only compares pairs
#' of levels (not a level to a cumulative mean of levels).
#'
#' Example interpretation for a 4 level factor:
#' \itemize{
#' \item Intercept = Grand mean (mean of the means of each level)
#' \item grp1 = mean(grp4, grp3, grp2) - grp(1)
#' \item grp2 = mean(grp4, grp3) - mean(grp2)
#' \item grp3 = mean(grp3) - mean(grp4)
#'}
#'
#' @param n Number of levels in the factor
#'
#' @return Contrast matrix
#' @export
#' @examples
#'
#' mydf <- data.frame(
#'    grp = factor(c(rep("F1",5),rep("F2",5),rep("F3",5),rep("F4",5))),
#'    resp = c(seq(1,5), seq(5,9), seq(10,14), seq(15,19))
#' )
#'
#' mydf %>%
#'   dplyr::group_by(grp) %>%
#'   dplyr::summarize(mu = mean(resp)) %>%
#'   dplyr::ungroup() %>%
#'   dplyr::mutate(grand_mean = mean(mu))
#'
#' summary(lm(resp ~ grp,
#'            data = mydf,
#'            contrasts = enlist_contrasts(mydf, grp ~ helmert_code)))
helmert_code <- function(n){
  matrix(rev(reverse_helmert_code(n)), nrow = n)
}
