#' Contrast code factors
#'
#' Helper to do contrast coding. There are two options:
#'  - Manually specify a matrix for code_by (implements manual_code). Reference level
#'  is automatically set to the row that's always negative.
#'  - Specify a style of contrast coding as a function (implements functional_code).
#'  Label of the reference level should be specified in ...
#'
#' @param factor_col The factor column to use, eg data$gender
#' @param code_by Either a matrix or a function
#' @param use_labels Labels to use in the contrast matrix, must equal number of contrasts
#' @param ... Additional arguments to be passed to functional_code, specifically,
#' which level you want the reference level to be
#' @param as_is Logical, default FALSE, whether to suppress auto switching of
#' the reference level to the first level if not specified
#'
#' @return A contrast coding matrix with labels and proper reference level
#' @export
contrast_code <- function(factor_col, code_by=NA, use_labels = NULL, as_is = FALSE, ...) {
  # If code_by is a matrix, use manual_code
  if (is.symbol(code_by))
    code_by <- get(code_by)

  if (is.function(code_by)) {
    specify_with <- functional_code
  } else {
    specify_with <- manual_code
  }
  # specify_with <- ifelse(is.function(code_by), functional_code, manual_code)
  contrast_matrix <- specify_with(factor_col=factor_col, code_by=code_by, as_is=as_is, ...)


  if (!is.null(use_labels)) {
    stopifnot("Provided labels must be same length as number of columns in contrast matrix." = ncol(contrast_matrix) == length(use_labels))
    colnames(contrast_matrix) <- use_labels
  }

  contrast_matrix
}
