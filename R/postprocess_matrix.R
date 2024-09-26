#' Postprocess contrast matrices
#'
#' When using [use_contrasts()] with a function or matrix, there are shared
#' postprocessing steps for manipulating the contrasts to set the reference
#' level, intercept, and labels. If as_is is TRUE, then these post processing
#' steps aren't included.
#'
#' @param new_contrasts Contrast matrix passed from [use_contrasts()]
#' @param reference_level Reference level specification
#' @param matrix_labels Labels from the matrix dimension names
#' @param labels User-specified labels
#' @param as_is Whether to use matrix as-is
#' @param dots Any additional arguments passed with code_by
#' @param code_by What to code with, passed from [use_contrasts()]
#' @param set_intercept Intercept specification
#' @param drop_trends Any trends to drop
#'
#' @return Contrast matrix
#' @seealso [use_contrasts()]
#' @keywords internal
.postprocess_matrix <- function(new_contrasts,
                                code_by,
                                reference_level,
                                set_intercept,
                                drop_trends,
                                matrix_labels,
                                labels,
                                as_is,
                                dots) {
  if (as_is) {

    if (is.null(colnames(new_contrasts))) {
      warning("No comparison labels set and as_is=TRUE, contrast labels will be column indices.") # nolint

      colnames(new_contrasts) <- seq_len(ncol(new_contrasts))
    }
    return(new_contrasts)
  }



  # Get indices for the default reference level and user-specified level
  if (is.function(reference_level))
    stop(
      cli::format_error(
        c(
          "Reference level is a function instead of an atomic type object.",
          " " = "Is the contrast object/function in the wrong place? See example:", # nolint
          "x" = "var ~ 1 + sum_code",
          "v" = "var ~ sum_code + 1"
        )
      )
    )

  # Switch reference level if needed, along with various error handling
  new_reference_index <- which(matrix_labels[[1]] == reference_level)
  new_contrasts <-
    .switch_reference_if_needed(new_contrasts,
                                reference_level,
                                new_reference_index)


  # Set the contrast labels, reorder as needed
  dimnames(new_contrasts) <- matrix_labels

  new_contrasts <- .reset_comparison_labels(new_contrasts, dots[["symchar"]])


  # If an intercept was specified, set it here
  if (!is.na(set_intercept)) {
    new_contrasts <- .set_intercept(new_contrasts, set_intercept)
  }

  # If one of the polynomial coding functions was used & the user wants to
  # drop one or more of the trends, do so here
  if (.is_polynomial_scheme(code_by) && !any(is.na(drop_trends))) {
    new_contrasts <- new_contrasts[, -drop_trends]
  }

  if (!is.null(labels)) {
    if (ncol(new_contrasts) != length(labels))
      stop("Provided labels must be same length as number of columns in contrast matrix.") # nolint
    colnames(new_contrasts) <- labels
  }

  new_contrasts
}
