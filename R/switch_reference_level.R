#' Switch reference level in contrast matrix
#'
#' Reorders the rows of a contrast matrix to properly set the reference level
#'
#' @param contrast_matrix An unlabeled contrast matrix
#' @param coding_fx The function used to generate the contrasts, used to check for treatment coding
#' @param old_reference The previously specified reference level, usually the default
#' for the scheme found by `.get_reference_level`
#' @param new_reference The index of the row to use as the new reference level
#'
#' @return A matrix with the reordered rows and correct values for the reference level
#' @export
.switch_reference_level <- function(contrast_matrix, coding_fx, old_reference, new_reference){

  # If the new reference is invalid or equal to the old reference, return the original matrix
  if (is.na(new_reference) | new_reference == old_reference | identical(coding_fx, stats::contr.helmert))
    return(contrast_matrix)

  # Swap the old reference row with the new reference row
  reference_row <- as.matrix(contrast_matrix[old_reference,])
  if (length(reference_row) == 2) # Ensure that the row is in the correct format
    reference_row <- t(reference_row)
  contrast_matrix[old_reference,] <- contrast_matrix[new_reference,]
  contrast_matrix[new_reference,] <- reference_row

  # Determine the order of the comparison rows
  comparison_order <- seq_len(nrow(contrast_matrix))
  if (old_reference != 2 && new_reference != 2) {
  comparison_order[old_reference] <- new_reference
  comparison_order[new_reference] <- old_reference
  }
  comparison_order <- comparison_order[(comparison_order - nrow(contrast_matrix)) < 0]
  # comparison_order <- seq_len(ncol(contrast_matrix))
    # seq_len(ifelse(old_reference == 1,
    #                                  ncol(contrast_matrix),
    #                                  old_reference - 1))

  # if (old_reference != 2) # Avoid dimension length issues with n=2
  #   comparison_order <- rev(seq_len(ncol(contrast_matrix)))# c(comparison_order[comparison_order != new_reference], new_reference)
  #
  # if (length(comparison_order) > 2)
  #   comparison_order <- c(comparison_order[-old_reference], new_reference)

  # Return the modified matrix with the rows reordered
  as.matrix(contrast_matrix[,comparison_order])
}

.get_reference_level <- function(cmat) {
  # Inverse matrix
  inv_matrix <- solve(.contrasts_to_hypotheses(cmat))

  if (all((cmat - stats::contr.helmert(nrow(cmat)) < 1e-10)))
      return(NA_integer_)

  inverse_matrix_result <- find_same_col(inv_matrix)
  inverse_matrix_result
}

.is_unscaled <- function(cmat) {
  determinant_value <-
    cmat |>
    .contrasts_to_hypotheses() |>
    base::solve() |>
    base::determinant(logarithm = FALSE)

  (abs(determinant_value[['modulus']]) - factorial(nrow(cmat))) < 1e-10
}
