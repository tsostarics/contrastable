#' Alert user when setting ordered factors
#'
#' Ordered factors use orthogonal polynomials (contr.poly) by default with
#' k number of levels -1 (k-1) of trend comparisons. If you use these functions
#' to set the contrasts to something else, the ordered class (hence ordering
#' to the levels) will remain but the contrasts will be set to something else.
#'
#' @param model_data Data frame to be used with the model
#' @param vars_in_model Variables to check against
#'
#' @return Nothing, sends a message
.msg_if_ordered_reset <- function(model_data, vars_in_model) {
  which_are_ordered <- vapply(model_data[vars_in_model], is.ordered, TRUE)

  if (any(which_are_ordered)) {
    or_default <- crayon::red(options("contrasts")[[1L]]["ordered"])
    ordered_names <-
      crayon::red(
        paste(
          names(which_are_ordered)[which_are_ordered],
          collapse = " "
        )
      )

    message(glue::glue("These factors are ordered, you may lose {or_default}: {ordered_names}"))
  }
}


#' Warn if one level factor
#'
#' Factors with only one level happen if a character vector is converted to a
#' factor using `factor()` but without specifying anything for the `levels`
#' parameter. If you try to access or set the contrasts for a one-level factor,
#' you'll get an error that contrasts are undefined because you have 0 degrees
#' of freedom.
#'
#' @param one_level_factors Character vector of which factors are one level
#' @param model_data Model data to look for factor columns
#' @param attempting_factors Factor column names to check
#'
#' @return Warns if factors with only one level are detected.
.warn_if_onelevel <- function(one_level_factors = NULL,
                              model_data = NULL,
                              attempting_factors = NULL) {
  if (!is.null(one_level_factors)) {
    # If you try to pass the factor names but the vector is actually empty
    if (identical(one_level_factors, character(0))) {
      return(invisible(1))
    }
  } else if (missing(model_data) || missing(attempting_factors)) {
    stop("If factor names are not provided, the model data and factors being set must be provided")
  } else {
    is_one_level <- vapply(
      attempting_factors,
      function(x) nlevels(model_data[[x]]) == 1L,
      logical(1)
    )
    # If it turns out there aren't any one level factors
    if (!any(is_one_level)) {
      return(invisible(1))
    }
    one_level_factors <- names(is_one_level)[is_one_level]
  }

  one_level_string <- paste(crayon::cyan(one_level_factors), collapse = " ")
  warning(glue::glue("Contrasts undefined for factors with only one level: {one_level_string}"),
    call. = FALSE
  )
  return(invisible(1))
}

#' Alert user when factor coercion happens
#'
#' Sends a message if columns in the dataset have been coerced to a factor,
#' useful if you accidentally set a column with continuous data to a factor.
#'
#' @param which_to_factors Names of columns that have been coerced to factors
#'
#' @return Nothing, sends a message if needed
.msg_if_coerced_to_factors <- function(which_to_factors) {
  varnames <- crayon::blue(paste(which_to_factors, collapse = " "))
  message(glue::glue("Converting to factors: {varnames}"))
}


#' Alert user if there are more factors
#'
#' Sends a message if the user has factor columns in their model data frame
#' that weren't specified along with any factors they DID set contrasts for.
#'
#' @param model_data Model data
#' @param specified_vars variables specified by the user from formulas
#'
#' @return nothing, just sends a message if needed
.msg_if_remaining_factors <- function(model_data, specified_vars) {
  which_are_factors <- .cols_where(model_data, is.factor, use_names = TRUE)
  which_are_ordered <- .cols_where(model_data, is.ordered, use_names = TRUE)
  which_are_onelevel <- .cols_where(model_data, .is.onelevel, use_names = TRUE)
  which_are_factors <- which_are_factors[!which_are_onelevel]
  which_are_ordered <- which_are_ordered[!which_are_onelevel]

  # Filter named logical vector to be only those where TRUE
  factor_cols <- which_are_factors[which_are_factors]
  factor_names <- names(factor_cols)

  remaining_factors <- factor_cols[!factor_names %in% specified_vars]

  if (any(remaining_factors)) {
    # Lookup default contrasts and color code accordingly
    uo_default <- crayon::blue(options("contrasts")[[1L]]["unordered"])
    or_default <- crayon::red(options("contrasts")[[1L]]["ordered"])
    varnames <-
      paste(
        vapply(
          names(remaining_factors),
          function(x) {
            ifelse(which_are_ordered[x],
              crayon::red(x),
              crayon::blue(x)
            )
          },
          character(1)
        ),
        collapse = " "
      )
    message(glue::glue("Expect {uo_default} or {or_default} for unset factors: {varnames}"))
  }
}
