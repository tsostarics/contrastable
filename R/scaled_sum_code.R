#' Scaled sum coding
#'
#' This is also sometimes called simple, sum, or deviation coding depending on
#' the field and source. Use this to compare each level to a reference level.
#'
#' For n levels of factors, generate a matrix with n-1 comparisons where:
#' \itemize{
#'  \item Reference level = -1/n
#'  \item Comparison level = (n-1)/n
#'  \item All others = -1/n
#'}
#'
#' Example interpretation for a 4 level factor:
#' \itemize{
#'  \item Intercept = Grand mean (mean of the means of each level)
#'  \item grp2 = mean(grp2) - mean(grp1)
#'  \item grp3 = mean(grp3) - mean(grp1)
#'  \item grp4 = mean(grp4) - mean(grp1)
#'}
#' Note: grp coefficient estimates are the same as with contr.treatment, but
#' the intercept is changed to the grand mean instead of the mean of grp1.
#'
#' IMPORTANT: This coding scheme is NOT the same as contr.sum/2 when the number
#' of levels is GREATER than 2. When k=2, estimates with contr.sum can be interpreted
#' as "half the distance between levels" but when k>2, contr.sum is to be interpreted
#' as "the distance between this level and the GRAND MEAN". You may be tempted
#' to use contr.sum(k)/2, but this tests the hypothesis that 3/2 times the mean
#' of a level is equal to half the sum of the means of the other levels, i.e.,
#' 1.5mu1 - .5mu2 - .5mu3 - .5mu4 = 0. I'm not sure when this would be useful.
#'
#' @param n_levels number of levels for this factor
#'
#' @return A matrix of scaled sum coded contrasts, unlabeled
#' @export
#' @examples
#' # Compare these two, note that contr.sum(4)/2 is not the same
#' scaled_sum_code(4)
#' contr.sum(4)
#'
#' # Here they happen to be equivalent (well, aside from the reference level contr.sum uses)
#' scaled_sum_code(2)
#' contr.sum(2)/2
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
#'            contrasts = enlist_contrasts(mydf, grp ~ scaled_sum_code)))
scaled_sum_code <- function(n_levels) {
  contrast_matrix <- stats::contr.sum(n_levels)
  contrast_matrix[contrast_matrix == 1] <- (n_levels - 1) / n_levels
  contrast_matrix[contrast_matrix == 0] <- -1/n_levels
  contrast_matrix[contrast_matrix == -1] <- -1/n_levels

  contrast_matrix
}

my_formula <-
  varname ~
  matrix(c(1,2,3),
         nrow = 3,
         ncol = 1) + 1 * get_whatever(asd = 2) |
  c('one', 'two', 'three')
