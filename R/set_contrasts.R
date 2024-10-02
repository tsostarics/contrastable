#' Set contrasts to factor columns in dataframe
#'
#' @description Uses the same syntax as [enlist_contrasts()],
#'   but returns the dataframe with the new contrasts applied. Use this when
#'   your model function doesnt have a contrasts argument and you want to avoid
#'   writing `contrasts<-` multiple times. See [enlist_contrasts()] for details
#'   about the package-specific syntax.
#'
#' @inherit enlist_contrasts params details
#' @param print_contrasts Logical, default FALSE, whether to print the contrasts
#' set for each factor. Fractions are displayed using [MASS::fractions()]
#' @seealso [enlist_contrasts()] [glimpse_contrasts()]
#'
#' @return The `model_data` dataframe, but with updated contrasts.
#' @export
#'
#' @importFrom stats contrasts<-
#' @importFrom MASS fractions
#' @examples
#'
#' head(
#'    set_contrasts(mtcars, carb + cyl ~ helmert_code, print_contrasts = TRUE)
#' )
#'
set_contrasts <- function(model_data,
                          ...,
                          verbose = getOption("contrastable.verbose"),
                          print_contrasts = FALSE) {
  formulas <- rlang::dots_list(...)

  # Because enlist_contrasts is being called from within this function, it will
  # return a list with both the contrasts and the factor-coerced data
  attr(formulas, "omit_drop") <- TRUE
  contrast_list <- enlist_contrasts(model_data, !!!formulas, verbose = verbose)
  model_data <- contrast_list$data
  contrast_list <- contrast_list$contrasts

  # Apply the contrasts to each factor column
  for (i in seq_along(contrast_list)) {
    factor_name <- names(contrast_list[i])
    contrasts(model_data[[factor_name]]) <- contrast_list[[i]]
  }

  if (print_contrasts) {
      print(lapply(contrast_list, MASS::fractions))
  }

  model_data
}
