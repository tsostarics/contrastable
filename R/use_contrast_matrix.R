#' Manual code factors
#'
#' Use this when you want to manually specify the contrast matrix
#'
#' @param factor_col The factor column to use, eg data$gender
#' @param code_by The matrix you want to use, obligatory if n factors > 2
#' @param as_is Whether to use the factor levels as labels
#' @param ... Not used
#' @return A matrix for contrast coding schemes
#' @export
#'
#' @importFrom stats contrasts
use_contrast_matrix <- function(factor_col, code_by = NA, as_is = FALSE, ...) {
  if (any(is.na(code_by)) & length(levels(factor_col)) > 2) {
    stop("This factor has more than 2 levels, please provide a matrix.")
  }

  given_comparisons <- colnames(code_by)
  labels <- dimnames(contrasts(factor_col))

  # Handling if the given matrix has prespecified comparison labels
  if (!is.null(given_comparisons))
    labels[[2]] <- given_comparisons

  # Default behavior for 2 level factors
  if (!any(is.na(code_by))) {
    new_contrasts <- code_by
  } else {
    new_contrasts <- -stats::contr.sum(2) / 2
  }

  dimnames(new_contrasts) <- labels
  if (!is.null(given_comparisons) | as_is)
    return(new_contrasts)
  .reset_comparison_labels(new_contrasts)
}

.reset_comparison_labels <- function(contr_mat, coding_fx = NULL) {
  # Avoid running through comparison label setting if this isn't a valid contrast
  .check_if_valid_contrmat(contr_mat)
  n <- nrow(contr_mat)

  if (!missing(coding_fx) && .is_polynomial_scheme(coding_fx))
    colnames(contr_mat) <- colnames(stats::contr.poly(n))
  else if (n != 2 & .check_backward_difference(contr_mat))
      colnames(contr_mat) <- paste(rownames(contr_mat)[-1], rownames(contr_mat)[-n], sep = "-")
  else if (n != 2 & .check_forward_difference(contr_mat))
      colnames(contr_mat) <- paste(rownames(contr_mat)[-n], rownames(contr_mat)[-1], sep = "-")
  else if (n != 2 & .check_reverse_helmert(contr_mat))
      colnames(contr_mat) <- paste0("<",rownames(contr_mat)[-1L])
  else if (n != 2 & .check_helmert(contr_mat))
    colnames(contr_mat) <- paste0(">",rownames(contr_mat)[-n])
  else {
    new_colnames <- unname(apply(contr_mat, 2, \(x) rownames(contr_mat)[x > 0]))
    if (all(vapply(new_colnames, \(x) length(x) == 1, TRUE)))
      colnames(contr_mat) <- new_colnames
  }
  contr_mat
}

.check_forward_difference <- function(contr_mat) {
  all(contr_mat[upper.tri(contr_mat, TRUE)] > 0) & all(contr_mat[lower.tri(contr_mat)] < 0)
}

.check_backward_difference <- function(contr_mat) {
  all(contr_mat[upper.tri(contr_mat, TRUE)] < 0) & all(contr_mat[lower.tri(contr_mat)] > 0)
}

.check_reverse_helmert <- function(contr_mat) {
  all(contr_mat == reverse_helmert_code(nrow(contr_mat)))
}


.check_helmert <- function(contr_mat) {
  all(contr_mat == helmert_code(nrow(contr_mat)))
}
