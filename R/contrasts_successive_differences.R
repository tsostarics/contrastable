#' Forward difference code
#'
#' @description
#' Compares the mean of level k to level k+1. Differs in direction from
#' \link[contrastable]{backward_difference_code}, so be careful to pick the
#' right function. See also \link[stats]{contr.sdif}.
#'
#' @details
#' Example interpretation for a 4 level factor:
#'
#'  - Intercept = Grand mean (mean of the means of each level)
#'  - grp1 = mean(grp1) - mean(grp2)
#'  - grp2 = mean(grp2) - mean(grp3)
#'  - grp3 = mean(grp3) - mean(grp4)
#'
#' @inherit scaled_sum_code params return
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
#' @description
#' Compares the mean of level k to level k-1. Differs in direction from
#' \link[contrastable]{forward_difference_code}, so be careful to pick the
#' right function.  See also \link[stats]{contr.sdif}.
#'
#' @details
#' Example interpretation for a 4 level factor:
#'
#'  - Intercept = Grand mean (mean of the means of each level)
#'  - grp1 = mean(grp2) - mean(grp1)
#'  - grp2 = mean(grp3) - mean(grp2)
#'  - grp3 = mean(grp4) - mean(grp3)
#'
#' @inherit scaled_sum_code params return
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
