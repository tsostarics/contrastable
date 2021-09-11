#' Check for orthogonality
#'
#' Given a contrast matrix or list of contrast matrices (eg from `enlist_contrasts`),
#' return a logical vector of whether each contrast is orthogonal or not.
#'
#' @param .contrasts Contrast matrix or list of contrast matrices
#'
#' @return Logical vector, will retain names of a passed list
#' @export
is_orthogonal <- function(.contrasts) {
  if (is.matrix(.contrasts))
    .contrasts <- list(.contrasts)

  vapply(.contrasts,
         function(m) {
           # Orthogonal polynomial contrasts have floating point issues
           cor_mat <- round(stats::cor(m), digits = 10L)
           cor_upper <- cor_mat[upper.tri(cor_mat)] == 0
           cor_lower <- cor_mat[lower.tri(cor_mat)] == 0

           # 2 level factor contrasts return logical(0)s
           if (identical(cor_upper, logical(0)) || identical(cor_lower, logical(0)))
             return(NA)

           all(c(cor_upper, cor_lower))
         },
         TRUE)

}
