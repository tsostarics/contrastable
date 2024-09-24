#' Check for orthogonality
#'
#' Given a contrast matrix or list of contrast matrices (eg from
#' `enlist_contrasts`), return a logical vector of whether each contrast is
#' orthogonal or not.
#'
#' @param contrast_matrices Contrast matrix or list of contrast matrices
#' @param USE.NAMES  Logical, whether vector should be named
#'
#' @return Logical vector, will retain names of a passed list
#' @export
#'
#' @examples
#'
#' is_orthogonal(treatment_code(5)) # FALSE
#' is_orthogonal(helmert_code(5)) # TRUE
is_orthogonal <- function(contrast_matrices, USE.NAMES = FALSE) {
  if (is.matrix(contrast_matrices)) {
    contrast_matrices <- list(contrast_matrices)
  }

  vapply(contrast_matrices,
         function(m) {
           # Orthogonal polynomial contrasts have floating point issues
           cor_mat <- stats::cor(m)
           cor_upper <- abs(cor_mat[upper.tri(cor_mat)]) < 1e-15
           cor_lower <- abs(cor_mat[lower.tri(cor_mat)]) < 1e-15

           # 2 level factor contrasts return logical(0)s
           if (identical(cor_upper, logical(0)) ||
               identical(cor_lower, logical(0))) {
             return(NA)
           }

           all(c(cor_upper, cor_lower))
         },
         TRUE,
         USE.NAMES = USE.NAMES
  )
}
