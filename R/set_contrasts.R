#' Set contrasts to dataframe
#'
#' @description Uses the same syntax as \link[contrastable]{enlist_contrasts},
#'   but returns the dataframe with the new contrasts applied. Use this when
#'   your model function doesnt have a contrasts argument and you want to avoid
#'   writing `contrasts<-` multiple times. See
#'   \link[contrastable]{enlist_contrasts} for details about the
#'   package-specific syntax.
#'
#' @inherit enlist_contrasts params details
#' @param print_contrasts Logical, default FALSE, whether to print the contrasts
#' set for each factor. Fractions are displayed using `MASS::fractions`
#' @seealso [enlist_contrasts()]
#'
#' @return the model_data dataframe, but with updated contrasts.
#' @export
set_contrasts <- function(model_data,
                          ...,
                          verbose = getOption("contrastable.verbose"),
                          print_contrasts = FALSE) {
  formulas <- rlang::dots_list(...)
  formulas <- .reinstate_dropped_trends(formulas)
  contrast_list <- enlist_contrasts(model_data, !!!formulas, verbose = verbose)
  factor_vars <- names(contrast_list)

  # Always FALSE to avoid printing message twice
  model_data <- .convert_to_factors(model_data, factor_vars, verbose = FALSE)

  for (i in seq_along(contrast_list)) {
    factor_name <- names(contrast_list[i])
    contrasts(model_data[[factor_name]]) <- contrast_list[[i]]
  }

  if (print_contrasts) {
      print(lapply(contrast_list, MASS::fractions))
  }

  model_data
}

#' Ignore attempt to drop trends in set_contrasts
#'
#' Will send a warning if it detects dropped trends with orthogonal polynomial
#' contrasts. If you use a different coding scheme it just gets ignored anyways.
#'
#' @param formulas formulas passed to `set_contrasts`
#'
#' @return Cleaned up formulas as needed
#'
#' @importFrom stats formula contrasts<-
.reinstate_dropped_trends <- function(formulas) {
  char_formulas <- vapply(formulas, deparse1, "char")

  poly_schemes <- "contr\\.poly|orth_polynomial_code|raw_polynomial_code"

  uses_contrpoly <- grepl(poly_schemes, char_formulas)
  has_dropped_trends <- grepl(" - [^ ]+:[^ ]+", char_formulas)

  which_to_ignore <- uses_contrpoly & has_dropped_trends
  which_used_incorrectly <- (!uses_contrpoly) & has_dropped_trends

  num_ignoring <- sum(which_to_ignore)

  # The ignoring part actually happens at the use_contrasts level, where
  # drop trends is simply not used if polynomial contrasts are not detected
  if (any(which_used_incorrectly)) {
    # Note: there's no check to see if a passed matrix is actually equivalent
    # to polynomial contrasts. I'm not sure why someone would do that, but
    # something to note for the future.
    warning(paste("Ignoring dropped trends, `-` used in invalid context (use only with polynomial contrast functions):", # nolint
      crayon::cyan(char_formulas[which_used_incorrectly]),
      sep = "\n",
      collapse = "\n"
    ))
  }

  if (num_ignoring == 0) {
    return(formulas)
  }

  ignore_string <- paste(
    crayon::cyan(num_ignoring),
    ifelse(num_ignoring == 1, "formula", "formulas")
  )

  if (any(has_dropped_trends)) {
    warning(glue::glue("Cannot drop trends with set_contrasts, ignoring in {ignore_string}. Use enlist_contrasts instead.")) # nolint
  }

  formulas_dropped_indices <- which(has_dropped_trends)

  for (i in formulas_dropped_indices) {
    var_envir <- rlang::get_env(formulas[[i]])
    new_formula <- gsub(" - [^ ]+:[^ ]+", "", deparse1(formulas[[i]]))
    formulas[[i]] <- formula(new_formula, env = var_envir)
  }

  formulas
}
