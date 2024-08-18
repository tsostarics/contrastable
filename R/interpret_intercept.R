#' Interpret intercept from contrasts
#'
#' Given a contrast matrix, try and interpret the intercept. Will usually be
#' either the grand mean, the mean of a reference level (e.g. contr.treatment),
#' the unweighted mean of multiple levels. Anything else would indicate custom
#' weights that the user provided, hence they should know how to interpret it.
#'
#' @param contr_mat Contrast matrix
#'
#' @return A string describing how to interpret the effect on the intercept
#' this coding scheme has
#' @export
#'
#' @examples
#' interpret_intercept(contr.treatment(2)) # mean(1)
#' interpret_intercept(contr.SAS(2)) # mean(2)
#' interpret_intercept(contr.sum(2)) # grand mean
#'
#' # Here there are 3 levels but the intercept is either an unweighted
#' # mean of 2 levels or a weighted mean of 2 levels
#' unweighted_intercept <- solve(t(matrix(c(.5, .5, 0, -1, 1, 0, -1, 0, 1), nrow = 3)))[, 2:3]
#' weighted_intercept <- solve(t(matrix(c(.8, .2, 0, -1, 1, 0, -1, 0, 1), nrow = 3)))[, 2:3]
#'
#' interpret_intercept(unweighted_intercept) # mean(1,2)
#' interpret_intercept(weighted_intercept) # custom weights
interpret_intercept <- function(contr_mat) {
  .nlevels <- nrow(contr_mat)

  # Account for polynomial contrasts with dropped trends, resulting in
  # non-square matrices
  if (ncol(contr_mat) < (.nlevels - 1)) {
    warning(paste0("Expected contrast matrix to have ",
                   .nlevels-1,
                   " columns but found ",
                   ncol(contr_mat)))
    return("grand mean")
  }

  intercept_column <- .contrasts_to_hypotheses(contr_mat, nrow(contr_mat))[, 1]

  # Check if grand mean (most common), round to avoid floating point errors
  is_grandmean <- all(round(intercept_column - (1 / .nlevels), 10) == 0)
  if (is_grandmean) {
    return("grand mean")
  }


  level_names <- rownames(contr_mat)
  if (is.null(level_names)) {
    level_names <- seq_len(.nlevels)
  }

  # Levels contribute to the intercept if they're not 0 in the intercept column
  contributing_levels <- intercept_column != 0
  same_weights <- all(round(intercept_column[contributing_levels], 3) ==
                        round(1 / sum(contributing_levels), 3))

  if (!same_weights) {
    return("custom weights")
  }

  mean <- paste(level_names[contributing_levels], collapse = ",")
  paste0("mean(", mean, ")")
}
