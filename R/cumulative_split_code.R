#' Cumulative split contrasts
#'
#' This scheme is similar to Helmert contrasts, but instead of comparing one
#' level to the accumulated mean of all previous levels, each comparison with
#' this scheme splits the levels into two groups: those below and including
#' the current level, and those above the current level. Conceptually this is
#' similar to continuation ratio logits used in ordinal models. For example,
#' with a four level factor with levels A, B, C, and D, the comparisons would
#' be:
#'
#'  - A vs. BCD
#'  - AB vs. CD
#'  - ABC vs. D
#'
#'  In other words, each comparison splits the levels into two groups.
#'  Each of these comparisons uses the cumulative mean of all the levels in
#'  each group. The intercept is the grand mean.
#'
#' @param n_levels Number of levels in the factor
#'
#' @return Contrast matrix
#' @export
#'
#' @examples
#'
#'
#' set.seed(111)
#' df <- data.frame(grp = rep(c('a', 'b', 'c', 'd'), each = 2000),
#'                  val = c(rnorm(2000, 2, 1),
#'                          rnorm(2000, 5, 1),
#'                          rnorm(2000, 7.5, 1),
#'                          rnorm(2000, 15, 1))) |>
#'   set_contrasts(grp ~ cumulative_split_code | c("a-rest", "ab-rest", "abc-rest"))
#'
#' lm(val ~ grp, data = df)
#'
#'
cumulative_split_code <- function(n_levels) {
  contrast_matrix <- matrix(0, nrow = n_levels, ncol = n_levels - 1)

  for (i in seq_len(n_levels-1)) {
    n_up <- seq(1, i)
    n_down <- seq(i+1, n_levels)
    contrast_matrix[n_up, i] <- 1 / length(n_up)
    contrast_matrix[n_down, i] <- -1 / length(n_down)
  }

  .hypotheses_to_contrasts(cbind(rep(1/n_levels, n_levels), contrast_matrix))
}
