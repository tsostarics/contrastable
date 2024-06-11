#'  Find column with same non-positive values
#'
#' The inverse matrix of a contrast matrix has a useful property
#' where, after removing the row corresponding to the intercept,
#' the reference level is the column index where each value in
#' that column is (1) the same and (2) less than or equal to 0.
#'
#' For a contrast matrix generated from an expression `x`, with
#' number of levels n, consider the inverse matrices from
#' `solve(.contrasts_to_hypotheses(x))` below for different `x`:
#'
#'  - contr.treatment: The first column contains all 0s, hence
#'                     the reference level is the first level
#'  - contr.SAS: The last column contains all 0s, hence the
#'               reference level is the last level
#'  - scaled_sum_code: The first column contains all -1/n, hence
#'                     the reference level is the first level
#'  - helmert_code: While the first column contains all negative
#'                  numbers, they are not the same EXCEPT if n=2
#'
#' Note that "reference level" here is defined as a level from
#' which every other level is compared to via pairwise comparisons.
#'
#' @param invmatrix Inverse of the hypothesis matrix
#'
#' @return Integer index of the reference level, if there is
#' no reference level, NA is returned
find_same_col <- function(invmatrix) {
  ncols <- ncol(invmatrix)
  indices <- seq.int(2, nrow(invmatrix))

  for (i in seq_len(ncols)) {
    col_i <- invmatrix[indices, i]
    if ((abs(min(col_i) - max(col_i)) < 1e-15) && all(col_i <= 0)) {
      return(i)
    }
  }

  NA_integer_
}
