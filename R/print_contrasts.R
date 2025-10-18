#' Print contrasts
#'
#' Utility to print all contrast matrices for a given dataframe.
#' Displays fractional values with [MASS::fractions()].
#' Prints nothing if there are no factor columns in the dataframe.
#'
#' @param x Dataframe containing factor columns
#' @param specific_vars Character vector of specific factor columns to print
#' contrasts for. Use to only print a certain subset of variables.
#'
#' @returns Nothing
#' @export
#'
#' @examples
#'
#' print_contrasts(data.frame(x = gl(3, 1), y = gl(3, 1, ordered = TRUE)))
print_contrasts <- function(x, specific_vars) {
  if (missing(specific_vars)) {
    is_factor <- vapply(colnames(x), \(v) is.factor(x[[v]]), TRUE)
    factor_vars <- colnames(x)[is_factor]
  } else {
    vars_in_df <- specific_vars %in% colnames(x)

    if (!all(vars_in_df)) {
      missing_vars <- paste0(specific_vars[!vars_in_df], collapse = ", ")
      stop(glue::glue("Columns not found in x: {missing_vars}"))
    }

    factor_vars <- specific_vars
  }


  for (v in factor_vars) {
    varname <- if (is.ordered(x[[v]])) crayon::red(v) else crayon::blue(v)

    cat(varname, "\n")
    print(MASS::fractions(contrasts(x[[v]])))
  }
}

