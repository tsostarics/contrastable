#' Switch reference level in contrast matrix
#'
#' Reorders the rows of a contrast matrix to properly set the reference level
#'
#' @param contrast_matrix An unlabeled contrast matrix
#' @param old_reference The previously specified reference level, usually the default
#' for the scheme found by `.get_reference_level`
#' @param new_reference The index of the row to use as the new reference level
#'
#' @return A matrix with the reordered rows and correct values for the reference level
#' @export
.switch_reference_level <- function(contrast_matrix, old_reference, new_reference){

  # If the new reference is invalid or equal to the old reference, return the original matrix
  if (is.na(new_reference) | new_reference == old_reference)
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

.switch_reference_if_needed <- function(cmat, new_reference_label = NA, new_reference_index) {
  if (!is.na(new_reference_label)  && identical(new_reference_index, integer(0)))
    stop("Reference level not found in factor levels")

  default_reference_index <- .get_reference_level(cmat)

  if (is.na(default_reference_index)) {
    if (!is.na(new_reference_label))
      warning("Ignoring reference level for scheme lacking a singular reference")
  } else {

    # If a new reference level isn't specified, then make the first level the
    # reference by default
    if (identical(new_reference_index, integer(0)))
      new_reference_index <- 1L
    cmat <- .switch_reference_level(cmat,
                                    default_reference_index,
                                    new_reference_index)
  }

  cmat
}

# Not using this for anything for the time being
# .is_unscaled <- function(cmat) {
#   determinant_value <-
#     base::determinant(
#       base::solve(
#         .contrasts_to_hypotheses(cmat)
#       ),
#       logarithm = FALSE
#     )
#
#   (abs(determinant_value[['modulus']]) - factorial(nrow(cmat))) < 1e-10
# }
