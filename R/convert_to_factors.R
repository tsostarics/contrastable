#' Convert non factors to factors
#'
#' Helper to convert columns to factors if they aren't already
#'
#' @param model_data Model data
#' @param vars_in_model variables specified for contrast coding from formulas
#' @param verbose Should messages be sent? Defaults to TRUE
#'
#' @return model_data with specified columns coerced to factors
#' @keywords internal
.convert_to_factors <- function(model_data, vars_in_model, verbose = TRUE) {
  which_not_factors <- vapply(
    model_data[vars_in_model],
    function(x) !is.factor(x),
    TRUE
  )

  # Message user if they're resetting an ordered column
  if (verbose) .msg_if_ordered_reset(model_data, vars_in_model)

  # If all specified variables are already factors, no need for coercion
  if (all(!which_not_factors)) {
    return(model_data)
  }

  # Coerce specified columns to factor data type, send message if needed
  which_to_factors <- names(which_not_factors)[which_not_factors]
  if (verbose) .msg_if_coerced_to_factors(which_to_factors)

  for (col in which_to_factors) {
    model_data[[col]] <- factor(model_data[[col]])
  }

  model_data
}
