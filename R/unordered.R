#' Check for unordered factor
#'
#' Helper to check if a factor is exclusively unordered. is.factor(x) is TRUE
#' when x is unordered OR ordered.
#'
#' @param x a vector of data
#'
#' @return TRUE if x is an unordered factor, FALSE if x is not a factor or is an
#'   ordered factor
#' @export
#'
#' @examples
#' is.unordered(gl(5,1)) # True
#' is.unordered(gl(5,1,ordered = TRUE)) # False
is.unordered <- function(x) {
  is.factor(x) & !is.ordered(x)
}


#' Convert to unordered factor
#'
#' Unordered analogue of base R's `as.ordered`. Will convert `x` to an unordered
#' factor; unlike `as.factor()`, this will convert ordered factors to unordered
#' factors.
#'
#' @param x Object to convert to unordered factor
#'
#' @return `x` as an unordered factor
#' @export
#' @seealso [as.factor()]
#' @examples
#'
#' # Convert an ordered factor to unordered
#' as.unordered(gl(5,1,ordered = TRUE))
#'
#' # If level order is pre-specified differently from default alphabetical order
#' # then the ordering will be retained
#' as.unordered(ordered(c("a", "b", "c"), levels = c("c", "a", "b")))
#'
#' # Otherwise the vector will be converted to an unordered factor with levels
#' # in the default alphabetical order
#' as.unordered(c("c", "a", "b"))
#'
#' # Note that coercing integer values will sort the values to use as the levels
#' as.unordered(4:1)
as.unordered <- function(x) {

  if (is.unordered(x)) {
    x
  } else if (is.ordered(x)) {
    class(x) <- "factor"
    x
    # nocov start
  } else if (!is.object(x) && is.integer(x)) {
    levels <- sort.int(unique.default(x))
    f <- match(x, levels)
    levels(f) <- as.character(levels)
    if (!is.null(nx <- names(x)))
      names(f) <- nx
    class(f) <- "factor"
    f
  } else {
    factor(x)
  }
  # nocov end
}
