#' Set contrast coding with function
#'
#' @param factor_col Factor column
#' @param code_by A function that generates contrast matrices
#' @param reference_level Label of the desired reference level, will default to
#' the first label in dimnames of the original contrasts
#' @param set_intercept Label of the desired intercept level, will default to
#' scheme's default (typically grand mean, unless using contr.treatment, which does
#' the reference level)
#' @param drop_trends Which trends to drop from the final matrix, should only
#' be used with `contr.poly` to remove higher order trends
#' @param as_is Logical, default FALSE, whether to suppress auto switching of
#' the reference level to the first level if not specified
#' @param ... Other arguments to pass to `coding_fx`
#'
#' @export
use_contrast_function <- function(factor_col, code_by, reference_level=NA, set_intercept = NA, drop_trends = NA, as_is=FALSE, ...) {
  # Extract labels to use for contrast matrix
  labels <- .get_dimnames(factor_col)
  params <- .bundle_params(factor_col, ...)

  n <- params[['n']]
  # Compose the contrast coding call with whatever params are passed/needed
  if (!'n' %in% names(formals(code_by))) {
    params['n'] <- NULL
  }

  new_contrasts <-
    tryCatch(
      do.call(code_by, params),
      error = \(e) {
        do.call(code_by, c(n, params))
      }
    )

  if (!as_is) {
    # Get indices for the default reference level and user-specified level
    default_reference <- .get_reference_level(new_contrasts)
    new_reference <- which(labels[[1]] == reference_level)

    # Switch reference level if needed, along with various error handling
    new_contrasts <- .switch_reference_if_needed(new_contrasts,
                                                 code_by,
                                                 reference_level,
                                                 default_reference,
                                                 new_reference)


    # Set the contrast labels, reorder as needed
    dimnames(new_contrasts) <- labels
    new_contrasts <- .reset_comparison_labels(new_contrasts, code_by)
  }
  # If an intercept was specified, set it here
  if (!is.na(set_intercept))
    new_contrasts <- .set_intercept(new_contrasts, set_intercept)

  # If one of the polynomial coding functions was used & the user wants to
  # drop one or more of the trends, do so here
  if (.is_polynomial_scheme(code_by) & !any(is.na(drop_trends))) {
    new_contrasts <- new_contrasts[,-drop_trends]
  }

  # Return final contrast matrix
  new_contrasts
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

# Extract parameters to coding function call from user-supplied dots
.bundle_params <- function(factor_col, ...) {
  n <- nlevels(factor_col)
  other_args <- rlang::dots_list(...)[['other']]
  if ('n' %in% names(other_args)) {
    if (n != other_args[['n']])
      warning("Number of factor levels does not match `n` specified in function call, using number of factor levels")
    other_args[['n']] <- NULL
  }

  params <- list(n = n)
  if (length(other_args) != 0)
    params <- c(params, other_args)

  params
}

.get_dimnames <- function(factor_col) {
  labels <- dimnames(stats::contrasts(factor_col))
  if (is.null(labels[[1L]]))
    labels[[1L]] <- levels(factor_col)
  if (is.null(labels[[2L]]))
    labels[[2L]] <- levels(factor_col)[-1L]
  labels
}

.set_intercept <- function(contrast_matrix, intercept_level) {
  if (!intercept_level %in% rownames(contrast_matrix))
    stop("Specified level to use as intercept not found in factor level names")

  n <- nrow(contrast_matrix)
  # Add back the missing intercept, solve the transpose for hypothesis matrix
  hypothesis_matrix <- .contrasts_to_hypotheses(contrast_matrix, n)

  intercept_column <- rep(0, n)
  intercept_index <- which(rownames(contrast_matrix) == intercept_level)
  intercept_column[intercept_index] <- 1
  hypothesis_matrix[,1] <- intercept_column

  # Resolve the new hypothesis matrix and remove intercept column for contrasts
  new_contrasts <- .hypotheses_to_contrasts(hypothesis_matrix)
  dimnames(new_contrasts) <- dimnames(contrast_matrix)
  new_contrasts
}


