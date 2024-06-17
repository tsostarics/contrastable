#' Check for orthogonality
#'
#' Given a contrast matrix or list of contrast matrices (eg from
#' `enlist_contrasts`), return a logical vector of whether each contrast is
#' orthogonal or not.
#'
#' @param .contrasts Contrast matrix or list of contrast matrices
#' @param USE.NAMES  Logical, whether vector should be named
#'
#' @return Logical vector, will retain names of a passed list
#' @export
is_orthogonal <- function(.contrasts, USE.NAMES = FALSE) {
  if (is.matrix(.contrasts)) {
    .contrasts <- list(.contrasts)
  }

  vapply(.contrasts,
         function(m) {
           # Orthogonal polynomial contrasts have floating point issues
           cor_mat <- stats::cor(m)
           cor_upper <- cor_mat[upper.tri(cor_mat)] < 1e-15
           cor_lower <- cor_mat[lower.tri(cor_mat)] < 1e-15

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
