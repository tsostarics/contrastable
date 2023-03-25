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
  if (is.na(new_reference) | new_reference == old_reference | identical(coding_fx, contr.helmert))
    return(contrast_matrix)

  # Swap the old reference row with the new reference row
  reference_row <- as.matrix(contrast_matrix[old_reference,])
  if (length(reference_row) == 2) # Ensure that the row is in the correct format
    reference_row <- t(reference_row)
  contrast_matrix[old_reference,] <- contrast_matrix[new_reference,]
  contrast_matrix[new_reference,] <- reference_row

  # Determine the order of the comparison rows
  comparison_order <- seq_len(ifelse(old_reference == 1,
                                     ncol(contrast_matrix),
                                     old_reference - 1))

  if (old_reference != 2) # Avoid dimension length issues with n=2
    comparison_order <- c(comparison_order[comparison_order != new_reference], new_reference)

  # Return the modified matrix with the rows reordered
  as.matrix(contrast_matrix[,comparison_order])
}




.get_reference_level <- function(cmat) {
  # Inverse matrix
  inv_matrix <- solve(.contrasts_to_hypotheses(cmat))

  .find_same_col(inv_matrix)
}

.find_same_col <- function(A) {
  A <- A[-1,]

  if (!is.matrix(A))
    A <- t(as.matrix(A))

  for (j in 1:ncol(A)) {
    if ((abs(min(A[, j]) - max(A[, j])) < 1e-15) && all(A[,j] <= 0)) {
      return(j)
    }
  }
  return(NA)
}
