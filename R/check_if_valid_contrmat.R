#' Check if valid contrast
#'
#' Sometimes a user might pass a matrix that can't actually be used, in which
#' case we can avoid some calculations by stopping earlier.
#'
#' @param contrast_matrix Contrast matrix
#' @return invisibly returns TRUE
#' @keywords internal
.is_valid_contrmat <- function(contrast_matrix) {
  if (diff(dim(contrast_matrix)) != -1L) {
    stop(paste0("Contrast matrix has invalid size: ",
                paste0(dim(contrast_matrix), collapse = ", ")))
  }

  tryMatch(.contrasts_to_hypotheses(contrast_matrix),
           "Lapack" = "This usually means your matrix is invalid for contrasts, try a different matrix.") # nolint

  return(invisible(TRUE))
}
