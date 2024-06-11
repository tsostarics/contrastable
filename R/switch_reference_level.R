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
  comparison_order <- seq_len(ncol(contrast_matrix))
  if (abs(old_reference - new_reference) != 1) {
    if (new_reference == 1) {
      comparison_order <- c(seq(2, old_reference - 1L),
                            1L,
                            comparison_order[comparison_order >= old_reference])
    } else if (new_reference > old_reference) {
      comparison_order <- c(comparison_order[comparison_order < old_reference],
                            new_reference - 1,
                            comparison_order[comparison_order >= old_reference & comparison_order != (new_reference-1)])
    } else if (new_reference < old_reference) {
      comparison_order <- c(new_reference,
                            comparison_order[comparison_order != new_reference])
    }
  }


  # Return the modified matrix with the rows reordered
  as.matrix(contrast_matrix[,comparison_order])
}

.switch_reference_if_needed <- function(cmat,
                                        coding_fx,
                                        reference_level = NA,
                                        default_reference,
                                        new_reference) {

  if (!is.na(reference_level) && identical(new_reference, integer(0)))
    stop("Reference level not found in factor levels")

  if (is.na(default_reference)) {
    if (!is.na(reference_level))
      warning("Ignoring reference level for scheme lacking a singular reference")
  } else {

    if (identical(new_reference, integer(0)))
      new_reference <- 1L
    cmat <- .switch_reference_level(cmat,
                                    coding_fx,
                                    default_reference,
                                    new_reference)
  }

  cmat
}

.is_unscaled <- function(cmat) {
  determinant_value <-
    base::determinant(
      base::solve(
        .contrasts_to_hypotheses(cmat)
        ),
      logarithm = FALSE
      )

  (abs(determinant_value[['modulus']]) - factorial(nrow(cmat))) < 1e-10
}
