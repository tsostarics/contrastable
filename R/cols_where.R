#' Get columns where
#'
#' Helper to avoid the use of tidyselect and dplyr::select, returns either a
#' logical vector (optionally named) or a character vector of which columns
#' satisfy the given function
#'
#' @param model_data Model data
#' @param fx Function to apply, must be something that returns a logical value.
#' Usually either `is.factor` or `is.ordered`
#' @param use.names Whether the resulting vector should be named
#' @param return.names Whether names (where the fx returns TRUE) should be
#' returned instead of a logical vector. Overwrites use.names.
#'
#' @return optionally named logical vector or character vector
.cols_where <- function(model_data,
                        fx,
                        use.names = FALSE,
                        return.names = FALSE) {
  cnames <- colnames(model_data)
  if (return.names)
    use.names <- TRUE
  cols <- vapply(cnames,
                 function(x) fx(model_data[[x]]),
                 FUN.VALUE = logical(1),
                 USE.NAMES = use.names)
  if (return.names)
    return(cnames[cols])
  cols
}

.is.onelevel <- function(x) {
  nlevels(x) == 1L
}
