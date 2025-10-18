#' Drop missing levels
#'
#' Missing levels can cause unexpected behavior when fitting a model after
#' setting contrasts. This function removes missing levels and informs the user
#' of how many levels were dropped from each factor.
#'
#' @param model_data Model data
#' @param lhs_variables variables specified for contrast coding from formulas
#' @param verbose Should messages be sent? Defaults to TRUE
#'
#' @returns model_data where existing factor columns have had any missing levels
#' removed
#' @keywords internal
.droplevels_as_needed <- function(model_data, lhs_variables, verbose = TRUE) {

  var_is_factor <- vapply(lhs_variables, \(v) is.factor(model_data[[v]]), TRUE)

  n_levels_to_drop <-
    vapply(
      lhs_variables[var_is_factor],
      \(v) {
        n_levels <- nlevels(model_data[[v]])
        n_unique <- length(unique(model_data[[v]]))
        n_levels - n_unique
      }, integer(1))

  vars_with_drops <- lhs_variables[var_is_factor][n_levels_to_drop>0L]

  if (verbose && length(vars_with_drops) > 0) {
    drop_amounts <- n_levels_to_drop[n_levels_to_drop > 0]

    is_ordered <- vapply(vars_with_drops, \(v) is.ordered(model_data[[v]]), TRUE)

    drop_strings <-
      vapply(seq_along(vars_with_drops),
             \(i) {
               color <- if (is_ordered[i]) crayon::red else crayon::blue

               color(paste0(vars_with_drops[i], " (", drop_amounts[i], ")"))
             }, character(1))

    cli::cli_warn(glue::glue("Dropping missing levels for: {paste0(drop_strings, collapse=', ')}"))
  }

  for (v in vars_with_drops) {
    model_data[[v]] <-  droplevels.factor(model_data[[v]])
  }

  model_data
}
