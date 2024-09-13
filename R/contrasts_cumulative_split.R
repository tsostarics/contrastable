#' Cumulative split contrasts
#'
#' @description
#' Contrast coding scheme that repeatedly dichotomizes the factor levels.
#'
#'
#' @details
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
#' @inherit scaled_sum_code params return
#' @export
#'
#' @examples
#'
#'
#' set.seed(111)
#' mydf <- data.frame(
#'   grp = rep(c("a", "b", "c", "d"), each = 2000),
#'   val = c(
#'     rnorm(200, 2, 1),
#'     rnorm(200, 5, 1),
#'     rnorm(200, 7.5, 1),
#'     rnorm(200, 15, 1)
#'   )
#' ) |>
#'   set_contrasts(grp ~ cumulative_split_code |
#'     c("a-rest", "ab-rest", "abc-rest"))
#'
#' lm(val ~ grp, data = mydf)
#'
cumulative_split_code <- function(n) {
  hypothesis_matrix <- matrix(0, nrow = n, ncol = n - 1)

  for (i in seq_len(n - 1)) {
    n_up <- seq(1, i)
    n_down <- seq(i + 1, n)
    hypothesis_matrix[n_up, i] <- 1 / length(n_up)
    hypothesis_matrix[n_down, i] <- -1 / length(n_down)
  }

  # Add in centered intercept and convert to contrast matrix
  contrast_matrix <-
    .hypotheses_to_contrasts(cbind(rep(1 / n, n), hypothesis_matrix))

  contrast_matrix
}
