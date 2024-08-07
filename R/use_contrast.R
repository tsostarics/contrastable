#' Contrast code factors
#'
#' Helper to do contrast coding. There are two options:
#'  - Manually specify a matrix for code_by (implements use_contrast_matrix).
#'  Reference level is automatically set to the row that's always negative.
#'  - Specify a style of contrast coding as a function. Label of the reference
#'  level should be specified in ...
#'
#' @param factor_col The factor column to use, eg data$gender
#' @param code_by Either a matrix or a function
#' @param labels Labels to use in the contrast matrix, must equal number of
#'   contrasts
#' @param ... Additional arguments to be passed to use_contrast_function,
#'   specifically, which level you want the reference level to be
#' @param as_is Logical, default FALSE, whether to suppress auto switching of
#'   the reference level to the first level if not specified
#' @param reference_level The level to use as the reference level, default NA
#' @param set_intercept The intercept to use, default NA
#' @param drop_trends Whether to drop trends, default NA
#'
#' @return A contrast coding matrix with labels and proper reference level
#' @export
use_contrasts <- function(factor_col,
                          code_by = NA,
                          reference_level = NA,
                          set_intercept = NA,
                          drop_trends = NA,
                          labels = NULL,
                          as_is = FALSE,
                          ...) {
  UseMethod("use_contrasts", code_by)
}


#' Symbol handler for use_contrasts
#'
#' Evaluates `code_by`, then applies the appropriate use_contrasts method
#'
#' @param factor_col A factor vector, eg from df$factorVarName
#' @param code_by A symbol to be evaluated
#' @param labels A vector of labels to apply to the matrix column names, default
#' NULL (no new labels)
#' @param as_is Logical, default FALSE, whether to leave the resulting matrix
#' as-is
#' @param ... Additional arguments to be passed on
#' @param reference_level The level to use as the reference level, default NA
#' @param set_intercept The intercept to use, default NA
#' @param drop_trends The trends to drop, default NA
#'
#' @return A contrast coding matrix with labels and proper reference level
#' @method use_contrasts symbol
#' @export
use_contrasts.symbol <- function(factor_col,
                                 code_by = NA,
                                 reference_level = NA,
                                 set_intercept = NA,
                                 drop_trends = NA,
                                 labels = NULL,
                                 as_is = FALSE, ...) {
  code_by <- get(code_by)
  UseMethod("use_contrasts", code_by)
}


#' Matrix handler for use_contrasts
#' Passing a matrix uses the implicit array class, so just pass it off to the
#' matrix method
#'
#' @param factor_col A factor vector, eg from df$factorVarName
#' @param code_by A matrix to be used as the contrast matrix
#' @param reference_level The level to use as the reference level, default NA
#' @param set_intercept The intercept to use, default NA
#' @param drop_trends The trends to drop, default NA
#' @param labels A vector of labels to apply to the matrix column names, default
#' @param as_is Logical, default FALSE, whether to leave the resulting matrix
#' @param ... Additional arguments to be passed to `code_by()`
#'
#' @return A contrast coding matrix with labels and proper reference level
#' @method use_contrasts array
#' @export
use_contrasts.array <- function(factor_col,
                                code_by = NA, reference_level = NA,
                                set_intercept = NA,
                                drop_trends = NA,
                                labels = NULL,
                                as_is = FALSE,
                                ...) {
  # TODO: change this to use parent frame
  use_contrasts.matrix(
    factor_col,
    code_by,
    reference_level,
    set_intercept,
    drop_trends,
    labels,
    as_is,
    ...
  )
}

#' Function handler for use_contrasts
#'
#' If the user provides a function, use the function and supplied arguments to
#' create a contrast matrix
#'
#' @param factor_col A factor vector, eg from df$factorVarName
#' @param code_by A function to be called, should return a contrast matrix
#' @param reference_level The name of the level to use as the reference level, default NA
#' @param set_intercept The intercept to use, default NA
#' @param drop_trends The trends to drop, default NA
#' @param labels A vector of labels to apply to the matrix column names, default
#' @param as_is Logical, default FALSE, whether to leave the resulting matrix
#' @param ... Additional arguments to be passed to `code_by()`
#'
#' @return A contrast coding matrix with labels and proper reference level
#' @method use_contrasts function
#' @export
use_contrasts.function <- function(factor_col,
                                   code_by = NA,
                                   reference_level = NA,
                                   set_intercept = NA,
                                   drop_trends = NA,
                                   labels = NULL,
                                   as_is = FALSE,
                                   ...) {
  # Extract labels to use for contrast matrix
  matrix_labels <- .get_dimnames(factor_col)
  params <- .bundle_params(factor_col, ...)

  n <- params[["n"]]
  # Compose the contrast coding call with whatever params are passed/needed
  if (!"n" %in% names(formals(code_by))) {
    params["n"] <- NULL
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
    new_reference_index <- which(matrix_labels[[1]] == reference_level)

    # Switch reference level if needed, along with various error handling
    new_contrasts <- .switch_reference_if_needed(
      new_contrasts,
      reference_level,
      new_reference_index
    )


    # Set the contrast labels, reorder as needed
    dimnames(new_contrasts) <- matrix_labels
    new_contrasts <- .reset_comparison_labels(new_contrasts, code_by)
  }
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
    stopifnot("Provided labels must be same length as number of columns in contrast matrix." = ncol(new_contrasts) == length(labels))
    colnames(new_contrasts) <- labels
  }

  if (as_is && is.null(colnames(new_contrasts))) {
    warning("No comparison labels set and as_is=TRUE, contrast labels will be column indices.")
  }
  new_contrasts
}

#' Matrix handler for use_contrasts
#'
#' If a user provides a raw matrix, then use that matrix as the contrast matrix
#'
#' @param factor_col A factor vector, eg from df$factorVarName
#' @param code_by A matrix to be used as the contrast matrix, should have
#' the same dimensions as the contrast matrix already applied to code_by
#' @param labels A vector of labels to apply to the matrix column names, default
#' @param as_is Logical, default FALSE, whether to leave the resulting matrix
#' @param ... Additional arguments, not used
#' @param reference_level Not used
#' @param set_intercept Not used
#' @param drop_trends Not used
#'
#' @return A contrast coding matrix with labels and proper reference level
#' @method use_contrasts matrix
#' @export
use_contrasts.matrix <- function(factor_col,
                                 code_by = NA,
                                 reference_level = NA,
                                 set_intercept = NA,
                                 drop_trends = NA,
                                 labels = NULL,
                                 as_is = FALSE,
                                 ...) {
  if (any(is.na(code_by)) && length(levels(factor_col)) > 2) {
    stop("This factor has more than 2 levels, please provide a matrix.")
  }

  preset_comparisons <- colnames(code_by)
  matrix_labels <- dimnames(stats::contrasts(factor_col))

  # Handling if the given matrix has prespecified comparison labels
  if (!is.null(preset_comparisons)) {
    matrix_labels[[2]] <- preset_comparisons
  }

  given_matrix_size <- dim(code_by)
  factor_size <- dim(stats::contrasts(factor_col))
  if (given_matrix_size[1] != factor_size[1] &&
      given_matrix_size[2] != factor_size[2]) {
    stop(
      "Matrix given to code_by is size ",
      paste(given_matrix_size, collapse = "x"),
      " but factor_col contrast matrix is size ",
      paste(factor_size, collapse = "x"), "."
    )
  }

  new_contrasts <- code_by
  dimnames(new_contrasts) <- matrix_labels

  # If we want to use the matrix as-is, return now
  if (!is.null(preset_comparisons) || as_is) {
    return(new_contrasts)
  }


  # If we provide new labels, as with the | operator, use those
  if (!is.null(labels)) {
    colnames(new_contrasts) <- labels
    return(new_contrasts)
  }

  # If we didn't provide any labels, use the default ones
  # if (is.null(labels))
  .reset_comparison_labels(new_contrasts)
}

#' Default handler for use_contrasts
#'
#' If a user doesn't specify a contrast matrix, use the defaults from options().
#' If the user tries to use something we don't know how to work with, throw a
#' warning that we'll be using the defaults from options().
#'
#' @param factor_col A factor vector, eg from df$factorVarName
#' @param code_by Some object that's not a matrix or function. If NA, no warning
#' will be thrown, and the default contrasts will be used. A warning will be
#' thrown if it's not NA.
#' @param labels A vector of labels to apply to the matrix column names, default
#' @param as_is Logical, default FALSE, whether to leave the resulting matrix
#' @param ... Additional arguments, not used
#' @param reference_level Not used
#' @param set_intercept Not used
#' @param drop_trends Not used
#'
#' @return Contrast matrix, using the ordered or unordered default from options()
#' @export
use_contrasts.default <- function(factor_col,
                                  code_by = NA,
                                  reference_level = NA,
                                  set_intercept = NA,
                                  drop_trends = NA,
                                  labels = NULL,
                                  as_is = FALSE,
                                  ...) {
  if (is.ordered(factor_col)) {
    contrast_function <- options()$contrasts["ordered"]
    contrast_string <- paste0(". Using ordered default ", crayon::red(contrast_function))
  } else {
    contrast_function <- options()$contrasts["unordered"]
    contrast_string <- paste0(". Using unordered default ", crayon::blue(contrast_function))
  }

  if (length(code_by) == 1L && is.na(code_by)) {
    return(get(contrast_function)(nlevels(factor_col)))
  }

  warning(paste0("Can't set contrasts with object of class ", class(code_by), contrast_string))
  get(contrast_function)(nlevels(factor_col))
}

#' Use a hypr object for contrasts
#'
#' @param factor_col A factor vector, eg from df$factorVarName
#' @param code_by A hypr object created with `hypr::hypr()`
#' @param labels A vector of labels to apply to the matrix column names, default
#' @param as_is Logical, default FALSE, whether to leave the resulting matrix
#' @param ... Additional arguments, not used
#' @param reference_level Not used
#' @param set_intercept Not used
#' @param drop_trends Not used
#'
#' @return Contrast matrix specified by the hypr object
#' @export
use_contrasts.hypr <- function(factor_col,
                               code_by = NA,
                               reference_level = NA,
                               set_intercept = NA,
                               drop_trends = NA,
                               labels = NULL,
                               as_is = FALSE,
                               ...) {
  requireNamespace("hypr", quietly = TRUE)


  # If use_contrasts is being called from enlist_contrasts, then it's likely
  # that we can extract a symbol for the factor column that the user is trying
  # to set contrasts for. If we're successful, this should be a character vector
  # of length 1 containing just the name of the factor column
  potential_factor_name <-
    as.character(eval(rlang::get_expr(rlang::enquo(factor_col))[[2L]],
                      envir = rlang::caller_env()
    ))

  stopifnot(hypr::nlevels(code_by) == nlevels(factor_col))

  contrast_matrix <- hypr::cmat(code_by)

  # If we were able to extract a factor column name previously, we'll do a small
  # check to make sure that all the factor levels specified in the hypr object
  # exist in the factor. The level names in the hypr object might be formatted
  # like varnameA, varnameB, varnameC or just A, B, C. We'll let the user use
  # the contrast matrix if there's a level in the hypr object that isn't in
  # the factor column but warn them that the contrasts might be messed up.
  # We already checked that the number of levels are equivalent, so this is
  # solely about names not matching up
  if (length(potential_factor_name) == 1L) {
    potential_level_names <- gsub(potential_factor_name, "", rownames(contrast_matrix))
    is_present_in_factor_levels <- potential_level_names %in% levels(factor_col)
    if (!all(is_present_in_factor_levels)) {
      levels_not_present <- potential_level_names[!is_present_in_factor_levels]
      warning(paste0(
        "Levels in hypr object not found in factor column `", potential_factor_name, "`: ",
        paste0(levels_not_present, collapse = ", "),
        "\nContrasts may be misspecified."
      ))
    }
  }
  if (as_is) {
    return(contrast_matrix)
  }

  # If the user is going through the trouble to use the hypr package then
  # they should really be setting things explicitly in the hypr object itself
  if (!is.na(reference_level)) {
    warning("reference_level ignored when using hypr object")
  }

  if (!is.na(set_intercept)) {
    warning("set_intercept ignored when using hypr object")
  }

  if (!is.na(drop_trends)) {
    warning("drop_trends ignored when using hypr object")
  }

  # If the user specifies labels for the comparisons then use those, otherwise
  # the column names from the hypr object's matrix will be used (either blank
  # or named if the user passed named formulas to the hypr constructor)
  if (!is.null(labels)) {
    colnames(contrast_matrix) <- labels
  }


  contrast_matrix
}




# Extract parameters to coding function call from user-supplied dots
.bundle_params <- function(factor_col, ...) {
  n <- nlevels(factor_col)
  other_args <- rlang::dots_list(...)[["other"]]
  if ("n" %in% names(other_args)) {
    if (n != other_args[["n"]]) {
      warning("Number of factor levels does not match `n` specified in function call, using number of factor levels")
    }
    other_args[["n"]] <- NULL
  }

  params <- list(n = n)
  if (length(other_args) != 0) {
    params <- c(params, other_args)
  }

  params
}

.get_dimnames <- function(factor_col) {
  labels <- dimnames(stats::contrasts(factor_col))
  if (is.null(labels[[1L]])) {
    labels[[1L]] <- levels(factor_col)
  }
  if (is.null(labels[[2L]])) {
    labels[[2L]] <- levels(factor_col)[-1L]
  }
  labels
}

.set_intercept <- function(contrast_matrix, intercept_level) {
  if (!intercept_level %in% rownames(contrast_matrix)) {
    stop("Specified level to use as intercept not found in factor level names")
  }

  n <- nrow(contrast_matrix)
  # Add back the missing intercept, solve the transpose for hypothesis matrix
  hypothesis_matrix <- .contrasts_to_hypotheses(contrast_matrix, n)

  intercept_column <- rep(0, n)
  intercept_index <- which(rownames(contrast_matrix) == intercept_level)
  intercept_column[intercept_index] <- 1
  hypothesis_matrix[, 1] <- intercept_column

  # Resolve the new hypothesis matrix and remove intercept column for contrasts
  new_contrasts <- .hypotheses_to_contrasts(hypothesis_matrix)
  dimnames(new_contrasts) <- dimnames(contrast_matrix)
  new_contrasts
}
