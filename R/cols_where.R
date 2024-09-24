#' Get columns where
#'
#' Helper to avoid the use of tidyselect and dplyr::select, returns either a
#' logical vector (optionally named) or a character vector of which columns
#' satisfy the given function
#'
#' @param model_data Model data
#' @param fx Function to apply, must be something that returns a logical value.
#' Usually either `is.factor` or `is.ordered`
#' @param use_names Whether the resulting vector should be named
#' @param return_names Whether names (where the fx returns TRUE) should be
#' returned instead of a logical vector. Overwrites use.names.
#'
#' @return Optionally named logical vector or character vector depending on
#' values of `use_names` and `return_names`
.cols_where <- function(model_data,
                        fx,
                        use_names = FALSE,
                        return_names = FALSE) {
  cnames <- colnames(model_data)
  if (return_names) {
    use_names <- TRUE
  }
  cols <- vapply(cnames,
    function(x) fx(model_data[[x]]),
    FUN.VALUE = logical(1),
    USE.NAMES = use_names
  )
  if (return_names) {
    return(cnames[cols])
  }
  cols
}
