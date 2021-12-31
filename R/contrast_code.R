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
#'
#' @return A contrast coding matrix with labels and proper reference level
#' @export
contrast_code <- function(factor_col, code_by=NA, use_labels = NULL, ...) {
  if (is.matrix(code_by)) {
    contrast_matrix <- manual_code(factor_col, code_by)
  } else if (is.function(code_by)) {
    contrast_matrix <- functional_code(factor_col, code_by, ...)
  } else if (length(contrasts(factor_col)) == 2) {
    contrast_matrix <- manual_code(factor_col)
  } else {
  stop("Invalid value for code_by, must be a matrix or coding function")
  }

  if (!is.null(use_labels)) {
    stopifnot("Provided labels must be same length as number of columns in contrast matrix." = ncol(contrast_matrix) == length(use_labels))
    colnames(contrast_matrix) <- use_labels
  }

  contrast_matrix
}
