#' Check if valid contrast
#'
#' Sometimes a user might pass a matrix that can't actually be used, in which
#' case we can avoid some calculations by stopping earlier.
#'
#' @param contr_mat Contrast matrix
#' @return invisibly returns TRUE
.is_valid_contrmat <- function(contr_mat) {
  if (diff(dim(contr_mat)) != -1L) {
    stop(paste0("Contrast matrix has invalid size: ",
                paste0(dim(contr_mat), collapse = ", ")))
  }

  tryMatch(.contrasts_to_hypotheses(contr_mat, nrow(contr_mat)),
           "Lapack" = "This usually means your matrix is invalid for contrasts, try a different matrix.") # nolint

  return(invisible(TRUE))
}
