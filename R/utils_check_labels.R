.reset_comparison_labels <- function(contr_mat, coding_fx = NULL) {
  # Avoid running through comparison label setting if this isn't a valid contrast
  .check_if_valid_contrmat(contr_mat)
  n <- nrow(contr_mat)

  if (!missing(coding_fx) && .is_polynomial_scheme(coding_fx)) {
    colnames(contr_mat) <- colnames(stats::contr.poly(n))
  } else if (n != 2 && .check_backward_difference(contr_mat)) {
    colnames(contr_mat) <- paste(rownames(contr_mat)[-1], rownames(contr_mat)[-n], sep = "-")
  } else if (n != 2 && .check_forward_difference(contr_mat)) {
    colnames(contr_mat) <- paste(rownames(contr_mat)[-n], rownames(contr_mat)[-1], sep = "-")
  } else if (n != 2 && .check_reverse_helmert(contr_mat)) {
    colnames(contr_mat) <- paste0("<", rownames(contr_mat)[-1L])
  } else if (n != 2 && .check_helmert(contr_mat)) {
    colnames(contr_mat) <- paste0(">", rownames(contr_mat)[-n])
  } else {
    new_colnames <- unname(apply(contr_mat, 2, \(x) rownames(contr_mat)[x > 0]))
    if (all(vapply(new_colnames, \(x) length(x) == 1, TRUE))) {
      colnames(contr_mat) <- new_colnames
    }
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
