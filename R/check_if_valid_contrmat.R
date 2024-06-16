#' Check if valid contrast
#'
#' Sometimes a user might pass a matrix that can't actually be used, in which
#' case we can avoid some calculations by stopping earlier.
#'
#' @param contr_mat Contrast matrix
.check_if_valid_contrmat <- function(contr_mat) {
  tryCatch(.contrasts_to_hypotheses(contr_mat, nrow(contr_mat)),
    error = function(c) {
      err <- conditionMessage(c)
      if (!grepl("Lapack", err)) {
        stop(c)
      }
      msg <- crayon::red("This usually means your matrix is invalid for contrasts, try a different matrix.")
      stop(paste(err, msg, sep = "\n"))
    }
  )
  return(invisible(1))
}
