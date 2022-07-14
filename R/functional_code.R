#' Set contrast coding with function
#'
#' @param factor_col Factor column
#' @param coding_fx A function that generates contrast matrices
#' @param reference_level Label of the desired reference level, will default to
#' the first label in dimnames of the original contrasts
#' @param set_intercept Label of the desired intercept level, will default to
#' scheme's default (typically grand mean, unless using contr.treatment, which does
#' the reference level)
#' @param drop_trends Which trends to drop from the final matrix, should only
#' be used with `contr.poly` to remove higher order trends
#' @param ... Other arguments to pass to `coding_fx`
#'
#' @export
#' @importFrom stats contr.helmert contr.poly contr.treatment
functional_code <- function(factor_col, coding_fx, reference_level=NA, set_intercept = NA, drop_trends = NA, ...) {
  labels <- .get_dimnames(factor_col)
  reference_i <- .get_reference_index(labels, reference_level, coding_fx)

  # Compile any user-provided arguments from the function call
  n_levels <- length(labels[[1L]])
  other_args <- rlang::dots_list(...)[['other']]
  params <- list(n = n_levels)
  if (length(other_args) != 0)
    params <- c(params, other_args)

  new_contrasts <- do.call(coding_fx, params)
  new_contrasts <- .switch_reference_level(new_contrasts,
                                           coding_fx,
                                           n_levels,
                                           reference_i)

  dimnames(new_contrasts) <- labels
  new_contrasts <- .reset_comparison_labels(new_contrasts, coding_fx)

  if (!is.na(set_intercept))
    return(.set_intercept(new_contrasts, set_intercept))

  if (.is_polynomial_scheme(coding_fx) & !any(is.na(drop_trends))) {
    new_contrasts <- new_contrasts[,-drop_trends]
  }

  return(new_contrasts)
}

.get_reference_index <- function(labels, reference_level, coding_fx) {
  reference_i <- NA
  if (is.na(reference_level))
    reference_i <- which(labels[[1L]] == labels[[1L]][1L])
  else
    reference_i <- which(labels[[1L]] == reference_level)
  if (.check_non_references(coding_fx)) {
    if (!is.na(reference_level))
      warning("Ignoring reference level passed for contrast scheme lacking a singular reference.")
    reference_i <- NA
  }

  if (identical(reference_i, integer(0)))
    stop("Reference level not found in contrast dimension names")
  reference_i
}

.get_dimnames <- function(factor_col) {
  labels <- dimnames(contrasts(factor_col))
  if (is.null(labels[[1L]]))
    labels[[1L]] <- levels(factor_col)
  if (is.null(labels[[2L]]))
    labels[[2L]] <- levels(factor_col)[-1L]
  labels
}

.set_intercept <- function(contrast_matrix, intercept_level) {
  if (!intercept_level %in% rownames(contrast_matrix))
    stop("Specified level to use as intercept not found in factor level names")

  n_levels <- nrow(contrast_matrix)
  # Add back the missing intercept, solve the transpose for hypothesis matrix
  hypothesis_matrix <- .contrasts_to_hypotheses(contrast_matrix, n_levels)

  intercept_column <- rep(0, n_levels)
  intercept_index <- which(rownames(contrast_matrix) == intercept_level)
  intercept_column[intercept_index] <- 1
  hypothesis_matrix[,1] <- intercept_column

  # Resolve the new hypothesis matrix and remove intercept column for contrasts
  new_contrasts <- solve(t(hypothesis_matrix))
  new_contrasts <- new_contrasts[,seq_len(n_levels)[-1L]]
  dimnames(new_contrasts) <- dimnames(contrast_matrix)
  new_contrasts
}

.check_non_references <- function(coding_fx) {
  # Evaluates whether a contrast scheme lacking a reference level was passed
  any(
    vapply(c(backward_difference_code,
             forward_difference_code,
             helmert_code,
             reverse_helmert_code,
             contr.poly,
             orth_polynomial_code,
             raw_polynomial_code),
           function(x)
             identical(coding_fx, x),
           TRUE)
  )
}

.contrasts_to_hypotheses <- function(contrast_matrix, n_levels) {
  intercept_matrix <- matrix(c(rep(1,n_levels), contrast_matrix), n_levels)
  solve(t(intercept_matrix))
}
