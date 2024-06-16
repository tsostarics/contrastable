#' Forward difference code
#'
#' Compares the mean of level k to level k+1. Differs from helmert coding
#' because it doesn't compare k to mean(k_i, ..., k_n)
#'
#' Example interpretation for a 4 level factor:
#' \itemize{
#' \item Intercept = Grand mean (mean of the means of each level)
#' \item grp1 = mean(grp1) - mean(grp2)
#' \item grp2 = mean(grp2) - mean(grp3)
#' \item grp3 = mean(grp3) - mean(grp4)
#' }
#'
#' @param n Number of factor levels
#'
#' @return Forward difference contrast matrix
#' @export
#'
#'
#' @examples
#' mydf <- data.frame(
#'   grp = factor(c(rep("F1", 5), rep("F2", 5), rep("F3", 5), rep("F4", 5))),
#'   resp = c(seq(1, 5), seq(5, 9), seq(10, 14), seq(15, 19))
#' )
#'
#' mydf |>
#'   dplyr::group_by(grp) |>
#'   dplyr::summarize(mu = mean(resp)) |>
#'   dplyr::ungroup() |>
#'   dplyr::mutate(grand_mean = mean(mu))
#'
#' summary(lm(resp ~ grp,
#'   data = mydf,
#'   contrasts = enlist_contrasts(mydf, grp ~ scaled_sum_code)
#' ))
forward_difference_code <- function(n) {
  contrasts <-
    lapply(
      seq_len(n),
      function(i) {
        c(rep(n - i, i), rep(-i, n - i)) / rep(n, n)
      }
    )
  matrix(unlist(contrasts[seq_len(n - 1L)]),
    nrow = n
  )
}

#' Backward difference code
#'
#' Compares the mean of level k to level k-1. Differs from reverse helmert
#' coding because it doesn't compare k to mean(k_0, ..., k_i-1). Mathematically
#' just flipping the signs of the matrix from forward difference coding.
#'
#' Example interpretation for a 4 level factor:
#' \itemize{
#' \item Intercept = Grand mean (mean of the means of each level)
#' \item grp1 = mean(grp2) - mean(grp1)
#' \item grp2 = mean(grp3) - mean(grp2)
#' \item grp3 = mean(grp4) - mean(grp3)
#' }
#' @param n Number of factor levels
#'
#' @return Backward difference contrast matrix
#' @export
#'
#' @examples
#' mydf <- data.frame(
#'   grp = factor(c(rep("F1", 5), rep("F2", 5), rep("F3", 5), rep("F4", 5))),
#'   resp = c(seq(1, 5), seq(5, 9), seq(10, 14), seq(15, 19))
#' )
#'
#' mydf |>
#'   dplyr::group_by(grp) |>
#'   dplyr::summarize(mu = mean(resp)) |>
#'   dplyr::ungroup() |>
#'   dplyr::mutate(grand_mean = mean(mu))
#'
#' summary(lm(resp ~ grp,
#'   data = mydf,
#'   contrasts = enlist_contrasts(
#'     mydf,
#'     grp ~ backward_difference_code
#'   )
#' ))
backward_difference_code <- function(n) {
  -forward_difference_code(n)
}
