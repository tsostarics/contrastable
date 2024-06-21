#' Check for orthogonality
#'
#' Given a contrast matrix or list of contrast matrices (eg from
#' `enlist_contrasts`), return a logical vector of whether each contrast is
#' centered or not.
#'
#' @inherit is_orthogonal params
#' @return Logical vector, will retain names of a passed list
#' @export
is_centered <- function(contrast_matrices, USE.NAMES = FALSE) {
  if (is.matrix(contrast_matrices)) {
    contrast_matrices <- list(contrast_matrices)
  }

  # Contrasts centered if column sums are all 0
  vapply(contrast_matrices,
    function(m) all(colSums(m) < 1e-15),
    logical(1),
    USE.NAMES = USE.NAMES
  )
}
