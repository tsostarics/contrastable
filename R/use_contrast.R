#' Contrast code factors
#'
#' Helper to do contrast coding. There are two options:
#'  - Manually specify a matrix for code_by (implements use_contrast_matrix). Reference level
#'  is automatically set to the row that's always negative.
#'  - Specify a style of contrast coding as a function (implements use_contrast_function).
#'  Label of the reference level should be specified in ...
#'
#' @param factor_col The factor column to use, eg data$gender
#' @param code_by Either a matrix or a function
#' @param labels Labels to use in the contrast matrix, must equal number of contrasts
#' @param ... Additional arguments to be passed to use_contrast_function, specifically,
#' which level you want the reference level to be
#' @param as_is Logical, default FALSE, whether to suppress auto switching of
#' the reference level to the first level if not specified
#'
#' @return A contrast coding matrix with labels and proper reference level
#' @export
use_contrasts <- function(factor_col, code_by=NA, labels = NULL, as_is = FALSE, ...) {
  # If code_by is a matrix, use use_contrast_matrix
  if (is.symbol(code_by))
    code_by <- get(code_by)

  if (is.function(code_by)) {
    specify_with <- use_contrast_function
  } else {
    specify_with <- use_contrast_matrix
  }

  contrast_matrix <- specify_with(factor_col=factor_col, code_by=code_by, as_is=as_is, ...)


  if (!is.null(labels)) {
    stopifnot("Provided labels must be same length as number of columns in contrast matrix." = ncol(contrast_matrix) == length(labels))
    colnames(contrast_matrix) <- labels
  }

  if (as_is && is.null(colnames(contrast_matrix))) {
    warning("No comparison labels set and as_is=TRUE, contrast labels will be column indices.")
  }

  contrast_matrix
}
