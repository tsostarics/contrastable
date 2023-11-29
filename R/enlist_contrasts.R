#' List of contrast matrices
#'
#' Returns a list of contrast matrices to use with modeling functions directly.
#'
#' Typically model functions like lm will have a contrasts argument where you can
#' set the contrasts at model run time, rather than having to manually change
#' the contrasts on the underlying factor columns in your data. If you prefer
#' this way, you can use this to generate the named list of contrast matrices.
#'
#'
#' @param model_data Data frame you intend on passing to your model
#' @param ... A series of 2 sided formulas with factor name on the LHS and
#' desired contrast scheme on the RHS, reference levels can be set with + and the
#' intercept can be overwritten with * (+ should come first if both are set)
#' @param verbose Logical, defaults to FALSE, whether messages should be printed
#'
#' @return List of named contrast matrices
#' @export
#'
#' @examples
#' my_df <- mtcars
#' my_df$gear = factor(my_df$gear)
#' my_df$carb = factor(my_df$carb)
#'
#' # Use formulas where left hand side is the factor column name
#' # and the right hand side is the contrast scheme you want to use
#' enlist_contrasts(my_df,
#'     gear ~ scaled_sum_code, # Using helpers from this package
#'     carb ~ helmert_code)
#'
#' # Add reference levels with +
#' enlist_contrasts(my_df,
#'     gear ~ scaled_sum_code + 5,
#'     carb ~ contr.sum + 6)
#' # Manually specifying matrix also works
#' enlist_contrasts(my_df,
#'     gear ~ matrix(c(1,-1,0,0,-1,1), nrow = 3),
#'     carb ~ forward_difference_code)
#'
#' # User matrices can be assigned to a variable first, but this may make the
#' # comparison labels confusing. You should rename them manually to something
#' # that makes sense. This will invoke manual_code, so reference levels
#' # specified with + will be ignored.
#' my_gear_contrasts <- matrix(c(1,-1,0,0,-1,1), nrow = 3)
#' colnames(my_gear_contrasts) <- c("CMP1", "CMP2")
#' enlist_contrasts(my_df,
#'     gear ~ my_gear_contrasts,
#'     carb ~ forward_difference_code)
#'
#'
#' # Will inform you if there are factors you didn't set
#' enlist_contrasts(my_df, gear ~ scaled_sum_code)
#'
enlist_contrasts <- function(model_data, ...,  verbose=TRUE) {
  # Get the formulas from the dots into list and character formats to work with
  formulas <- purrr::list_flatten(rlang::dots_list(...))
  char_formulas <- .formula_to_char(formulas) # as.character with a better error msg

  # Extract which factor columns are attempting to be set
  vars_in_model <- vapply(char_formulas, function(x) x[[2L]] %in% names(model_data), TRUE)
  names(vars_in_model) <- vapply(char_formulas, function(x) x[[2L]], "char")

  model_data <- .convert_to_factors(model_data, names(vars_in_model), verbose)

  if (verbose)
    .msg_if_remaining_factors(model_data, names(vars_in_model))

  # Ignore factors with only 1 level to avoid undefined contrasts
  is_onelevel_factor <-  vapply(names(vars_in_model),
                                function(x) nlevels(model_data[[x]]) == 1L,
                                TRUE)

  formulas <- formulas[!is_onelevel_factor]
  char_formulas <- char_formulas[!is_onelevel_factor]
  vars_in_model <- vars_in_model[!is_onelevel_factor]

  .warn_if_onelevel(names(is_onelevel_factor)[is_onelevel_factor])

  formula_indices <- seq_along(char_formulas)
  stats::setNames(
    lapply(formula_indices,
           function(x)
             .process_contrasts(model_data,
                                char_formula = char_formulas[[x]],
                                raw_formula = formulas[[x]] # Reference value bindings
             )
    ),
    names(vars_in_model)
  )
}


#' Pass arguments to contrast code
#'
#' Processes a formula and any arguments for the contrast matrix and sets up
#' the contrast_code call
#'
#' @param model_data Data frame with factor column
#' @param raw_formula Raw formula
#' @param char_formula character conversion of formula
#'
#' @return A contrast matrix
.process_contrasts <- function(model_data, char_formula, raw_formula) {
  var_envir <- rlang::get_env(raw_formula)

  params <- .split_if_language(.parse_formula(raw_formula), var_envir)

  contrast_code(
    factor_col = get(params[["factor_col"]], model_data),
    code_by = eval(params[["code_by"]], var_envir),
    use_labels = eval(params[['labels']], var_envir),
    reference_level = eval(params[["reference_level"]], var_envir),
    set_intercept = eval(params[["intercept_level"]], var_envir),
    drop_trends = eval(params[["drop_trends"]], var_envir),
    other = params[['other_args']]
  )

}

#' Split contrast function using parens
#'
#' If something like `set_contrasts(df, var ~ sum_code())` then what's
#' extracted from the formula isn't the symbol for the function `sum_code` but
#' a language object. This can be converted to a list to extract the function
#' symbol and then any arguments provided in the parens. The latter needs to
#' be evaluated in the original environment, for example if
#' `set_contrasts(df, var ~ sum_code(scores=c(.1,.5,.6))` is called, the scores
#' list would not evaluate the `c(...)` call, yielding an error that the scores
#' argument isn't the right length (because it would be length 1, not 3).
#'
#' @param params Parameters extracted from formula parsing
#' @param var_envir Environment to evaluate expressions in
#'
#' @return Parameter list with `code_by` set to the correct symbol & an
#' additional list entry for other arguments, which will be empty if no
#' arguments are provided.
.split_if_language <- function(params, var_envir) {
  params[['other_args']] <- list()

  if (!is.symbol(params[['code_by']]) & is.language(params[['code_by']])) {
    params[['other_args']] <- as.list(params[['code_by']])[-1]
    params[['code_by']] <- params[['code_by']][[1]]

    if (length(params[['other_args']]) > 0) {
      for (arg_i in seq_along(params['other_args'])) {

        params[['other_args']][arg_i] <- list(eval(params[['other_args']][[arg_i]],
                                                   var_envir)
                                              )
      }
    }
  }
  params
}
