#' Switch reference level in contrast matrix
#'
#' Switch rows to set the reference level properly
#'
#' @param contrast_matrix Contrast matrix, unlabeled
#' @param coding_fx function used to generate contrasts, used to check for treatment coding
#' @param old_reference Usually number of levels in the factor
#' @param new_reference Which row to use as the reference level
#'
#' @return A matrix with the correct values for the reference level
#' @export
.switch_reference_level <- function(contrast_matrix, coding_fx, old_reference, new_reference){
  if (identical(coding_fx, contr.treatment) | identical(coding_fx, treatment_code))
    old_reference <- 1L
  if (is.na(new_reference) | new_reference == old_reference)
    return(contrast_matrix)
  reference_row <- as.matrix(contrast_matrix[old_reference,]) # as.matrix needed for n_levels = 2
  contrast_matrix[old_reference,] <- as.matrix(contrast_matrix[new_reference,])
  contrast_matrix[new_reference,] <- reference_row
  comparison_order <- seq_len(old_reference - 1)
  if (identical(coding_fx, contr.treatment) | identical(coding_fx, treatment_code))
    return(contrast_matrix)
  if (old_reference != 2) # Avoid dimension length issues with n=2
    comparison_order <- c(comparison_order[comparison_order != new_reference], new_reference)

  as.matrix(contrast_matrix[,comparison_order])
}
