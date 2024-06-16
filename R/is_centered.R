#' Check for orthogonality
#'
#' Given a contrast matrix or list of contrast matrices (eg from
#' `enlist_contrasts`), return a logical vector of whether each contrast is
#' centered or not.
#'
#' @param .contrasts Contrast matrix or list of contrast matrices
#' @param USE.NAMES Logical, whether vector should be named
#'
#' @return Logical vector, will retain names of a passed list
#' @export
is_centered <- function(.contrasts, USE.NAMES = FALSE) {
  if (is.matrix(.contrasts)) {
    .contrasts <- list(.contrasts)
  }

  # Contrasts centered if column sums are all 0
  vapply(.contrasts,
    function(m) all(round(colSums(m), 10) == 0),
    logical(1),
    USE.NAMES = USE.NAMES
  )
}
