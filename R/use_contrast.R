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
#'
#' @examples
#'
#' # Create a contrast matrix given some factor vector with the specified
#' # reference level
#' use_contrasts(gl(5,2), sum_code, reference_level = 3)
#'
#' # Set column labels; order for labels is the same as the column indices
#' use_contrasts(gl(3,2), scaled_sum_code, labels = c("2-1", "3-1"))
#'
#' my_data <- mtcars
#' my_data$gear <- factor(mtcars$gear)
#'
#' MASS::fractions(use_contrasts(my_data$gear, helmert_code))
#'
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


#' AsIs method for use_contrasts
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
#' @method use_contrasts AsIs
#' @export
#'
#' @examples
#'
#' use_contrasts(gl(5,1), I(scaled_sum_code))
#'
use_contrasts.AsIs <- function(factor_col,
                               code_by = NA,
                               reference_level = NA,
                               set_intercept = NA,
                               drop_trends = NA,
                               labels = NULL,
                               as_is = FALSE, ...) {
  class(code_by) <- class(code_by)[class(code_by) != "AsIs"]
  method_call <- match.call()
  method_call[["code_by"]] <- code_by
  method_call[[1]] <- use_contrasts
  method_call[["as_is"]] <- TRUE
  eval(method_call)
}


#' Symbol method for use_contrasts
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
#' @method use_contrasts name
#' @export
#'
#' @examples
#'
#' aliased_scheme <- sum_code
#' contrast_scheme <- rlang::sym("aliased_scheme")
#'
#' # Result will be as if sum_code was used directly
#' use_contrasts(gl(5,1), contrast_scheme)
#'
use_contrasts.name <- function(factor_col,
                               code_by = NA,
                               reference_level = NA,
                               set_intercept = NA,
                               drop_trends = NA,
                               labels = NULL,
                               as_is = FALSE, ...) {


  code_by <- get(code_by,envir = parent.frame())
  method_call <- match.call()
  method_call[["code_by"]] <- code_by
  method_call[[1]] <- use_contrasts
  eval(method_call)
}



#' Function method for use_contrasts
#'
#' If the user provides a function, use the function and supplied arguments to
#' create a contrast matrix
#'
#' @param factor_col A factor vector, eg from df$factorVarName
#' @param code_by A function to be called, should return a contrast matrix
#' @param reference_level The name of the level to use as the reference level,
#' default NA
#' @param set_intercept The intercept to use, default NA
#' @param drop_trends The trends to drop, default NA
#' @param labels A vector of labels to apply to the matrix column names, default
#' @param as_is Logical, default FALSE, whether to leave the resulting matrix
#' @param ... Additional arguments to be passed to `code_by()`
#'
#' @return A contrast coding matrix with labels and proper reference level
#' @method use_contrasts function
#' @export
#'
#' @examples
#' use_contrasts(gl(5,1), sum_code)
use_contrasts.function <- function(factor_col,
                                   code_by = NA,
                                   reference_level = NA,
                                   set_intercept = NA,
                                   drop_trends = NA,
                                   labels = NULL,
                                   as_is = FALSE,
                                   ...) {
  # Extract labels to use for contrast matrix
  dots <- rlang::dots_list(...)

  matrix_labels <- .get_dimnames(as.unordered(factor_col))
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
        if (grepl('argument "n" is missing', conditionMessage(e)))
          do.call(code_by, c(n, params))

        stopWithMatch(e,
                      "cannot be represented accurately" =
                        c("Polynomial contrasts can only be used with <95 levels.", # nolint
                          "Convert to unordered with  `as.unordered` or use a non-polynomial scheme.")) # nolint
        stop(e)
      }
    )


  .postprocess_matrix(new_contrasts,
                      code_by,
                      reference_level,
                      set_intercept,
                      drop_trends,
                      matrix_labels,
                      labels,
                      as_is,
                      dots)
}

#' Matrix method for use_contrasts
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
#'
#' @examples
#'
#' contrast_matrix <- sum_code(4)
#' use_contrasts(gl(4,1), contrast_matrix)
#'
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

  dots <- rlang::dots_list(...)


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
      paste(factor_size, collapse = "x"),
      "."
    )
  }

  new_contrasts <- code_by

  # If we want to use the matrix as-is, return now
  if (!is.null(preset_comparisons) &&
      !identical(preset_comparisons, matrix_labels[[2L]])) {
    return(new_contrasts)
  }

  # Prevents .reset_comparison_labels from treating this as a function
  dots["symchar"] <- list(NULL)


  .postprocess_matrix(new_contrasts,
                      code_by,
                      reference_level,
                      set_intercept,
                      drop_trends,
                      matrix_labels,
                      labels,
                      as_is,
                      dots)
}

#' Default method for use_contrasts
#'
#' If a user doesn't specify a contrast matrix, use the defaults from options().
#' If the user tries to use something we don't know how to work with, throw a
#' warning that we'll be using the defaults from options().
#'
#' @param factor_col A factor vector, eg from `df$factorVarName`
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
#' @return Contrast matrix, using the ordered or unordered default from
#' `options()`
#' @export
#'
#' @examples
#' use_contrasts(gl(5,1), helmert_code) # a function
#' my_matrix <- helmert_code(5)
#' use_contrasts(gl(5,1), my_matrix) # a matrix
#'
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
    contrast_string <- paste0(". Using ordered default ",
                              crayon::red(contrast_function))
  } else {
    contrast_function <- options()$contrasts["unordered"]
    contrast_string <- paste0(". Using unordered default ",
                              crayon::blue(contrast_function))
  }

  if (length(code_by) == 1L && is.na(code_by)) {
    return(get(contrast_function)(nlevels(factor_col)))
  }

  if (!is.matrix(code_by) && is.atomic(code_by)) {
    stop(
      cli::format_error(
        c("Can't set contrasts with atomic type object, see example below:",
          "x" = "var ~ 1 + sum_code",
          "v" = "var ~ sum_code + 1"))
    )
  }

  warning(paste0("Can't set contrasts with object of class ",
                 class(code_by),
                 contrast_string))




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
#'
#' @examplesIf rlang::is_installed("hypr")
#' hypr_obj <- hypr::hypr(a ~ b, c ~ b) # centered pairwise comparisons to b
#'
#' use_contrasts(factor(c('a', 'b', 'c')), hypr_obj)
#'
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
    potential_level_names <-
      gsub(potential_factor_name, "", rownames(contrast_matrix))

    is_present_in_factor_levels <- potential_level_names %in% levels(factor_col)
    if (!all(is_present_in_factor_levels)) {
      levels_not_present <- potential_level_names[!is_present_in_factor_levels]

      warning(paste0(
        "Levels in hypr object not found in factor column `",
        potential_factor_name,
        "`: ",
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
      warning("# of levels does not match `n` in call, using nlevels instead")
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
  labels <-
    tryMatch(
      dimnames(stats::contrasts(factor_col)),
      "cannot be represented accurately" =
        c("Polynomial contrasts can only be used with <95 levels.",
          "Convert to unordered with  `as.unordered` or use a non-polynomial scheme.") # nolint
    )

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
