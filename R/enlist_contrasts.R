#' List of contrast matrices
#'
#' @description Returns a named list of contrast matrices to use with modeling
#'   functions directly. See \link[contrastable]{set_contrasts} for a function
#'   to set contrasts directly to the dataframe. See details for syntax
#'   information
#'
#' @details
#'
#' \link[contrastable]{enlist_contrasts}, \link[contrastable]{set_contrasts},
#' and \link[contrastable]{glimpse_contrasts} use special syntax to set
#' contrasts for multiple factors. The syntax consists of two-sided formulas
#' with the desired factor column on the left hand side and the contrast
#' specification on the right hand side. For example, `varname ~
#' scaled_sum_code`. Many contrasts support additional kinds of contrast
#' manipulations using overloaded operators:
#'
#'  - `+ X`: Set the reference level to the level named X. Only supported for
#' schemes that have a singular reference level such as
#' \link[contrastable]{sum_code}, \link[contrastable]{scaled_sum_code},
#' \link[contrastable]{treatment_code}, \link[stats]{contr.treatment},
#' \link[stats]{contr.sum}, \link[stats]{contr.SAS}. Ignored for schemes like
#' \link[contrastable]{helmert_code}.
#'  - `* X`: Overwrite the intercept to the mean of the level named X
#'  - `- A:B`: For polynomial coding schemes only, drop comparisons A through B.
#'  - `| c(...)`: Change the comparison labels for the contrast matrix to the
#' character vector `c(...)` of length `n-1`. These labels will appear in the
#' output/summary of a statistical model. Note that for `brms::brm`,
#' instances of `-` (a minus sign) are replaced with `M`.
#'
#' You can also specify multiple variables on the left hand side of a formula
#' using tidyselect helpers. See examples for more information.
#'
#' Typically model functions like lm will have a contrasts argument where you
#' can set the contrasts at model run time, rather than having to manually
#' change the contrasts on the underlying factor columns in your data. This
#' function will return such a named list of contrast matrices to pass to these
#' functions. Note that this function should not be used within a modeling
#' function call, e.g., \code{lm(y~x, data = model_data, contrasts =
#' enlist_contrasts(model_data, x~sum_code))}. Often, this will call
#' `enlist_contrasts` twice, rather than just once.
#'
#' For some model fitting functions, like `brms::brm`, there is no
#' contrasts argument. For such cases, use \link[contrastable]{set_contrasts} to
#' set contrasts directly to the factors in a dataframe.
#'
#' One good way to use \link[contrastable]{enlist_contrasts} is in conjunction
#' with \link[MASS]{fractions} to create a list of matrices that can be printed
#' to explicitly show the entire contrast matrices you're using for your models.
#' This can be especially helpful for supplementary materials in an academic
#' paper.
#'
#' Sometimes when using orthogonal polynomial contrasts from
#' \link[stats]{contr.poly}, people will drop higher level polynomials for
#' parsimony. Note however that these do capture some amount of variation, so
#' even though they're orthogonal contrasts the lower level polynomials will
#' have their estimates changed. Moreover, you cannot reduce a contrast matrix
#' to a matrix smaller than size n*n-1 in the dataframe you pass to a model
#' fitting function itself, as R will try to fill in the gaps with something
#' else. If you want to drop contrasts you'll need to use something like
#' \code{enlist_contrasts(df, x ~ contr.poly - 3:5)} and pass this to the
#' `contrasts` argument in the model fitting function.
#'
#' @param model_data Data frame you intend on passing to your model
#' @param ... A series of 2 sided formulas with factor name on the left hand
#'   side and desired contrast scheme on the right hand side. The reference
#'   level can be set with `+`, the intercept can be overwritten with `*`,
#'   comparison labels can be set using `|`, and trends for polynomial coding
#'   can be removed using `-`.
#' @param verbose Logical, defaults to FALSE, whether messages should be printed
#'
#' @return List of named contrast matrices
#' @seealso [set_contrasts()]
#' @export
#'
#' @examples
#' my_df <- mtcars
#' my_df$gear <- factor(my_df$gear)
#' my_df$carb <- factor(my_df$carb)
#'
#' # Use formulas where left hand side is the factor column name
#' # and the right hand side is the contrast scheme you want to use
#' enlist_contrasts(
#'   my_df,
#'   gear ~ scaled_sum_code, # Using helpers from this package
#'   carb ~ helmert_code
#' )
#'
#' # Add reference levels with +
#' enlist_contrasts(
#'   my_df,
#'   gear ~ scaled_sum_code + 5,
#'   carb ~ contr.sum + 6
#' )
#' # Manually specifying matrix also works
#' enlist_contrasts(
#'   my_df,
#'   gear ~ matrix(c(1, -1, 0, 0, -1, 1), nrow = 3),
#'   carb ~ forward_difference_code
#' )
#'
#' # User matrices can be assigned to a variable first, but this may make the
#' # comparison labels confusing. You should rename them manually to something
#' # that makes sense. This will invoke use_contrast_matrix, so reference levels
#' # specified with + will be ignored.
#' my_gear_contrasts <- matrix(c(1, -1, 0, 0, -1, 1), nrow = 3)
#' colnames(my_gear_contrasts) <- c("CMP1", "CMP2")
#' enlist_contrasts(
#'   my_df,
#'   gear ~ my_gear_contrasts,
#'   carb ~ forward_difference_code
#' )
#'
#'
#' # Will inform you if there are factors you didn't set
#' enlist_contrasts(my_df, gear ~ scaled_sum_code)
#'
#' # Use MASS::fractions to pretty print matrices for academic papers:
#' enlist_contrasts(my_df, gear ~ scaled_sum_code, carb ~ helmert_code) |>
#' lapply(MASS::fractions)
#'
#' # Use a list of formulas to use the same contrasts with different datasets
#' my_contrasts <- list(gear ~ scaled_sum_code, carb ~ helmert_code)
#' enlist_contrasts(my_df,  my_contrasts)
#' enlist_contrasts(mtcars, my_contrasts)
#'
#' \dontrun{
#' # Use tidyselect helpers to set multiple variables at once
#' # These are all equivalent
#' enlist_contrasts(mtcars, cyl ~ sum_code, gear ~ sum_code)
#' enlist_contrasts(mtcars, cyl + gear ~ sum_code)
#' enlist_contrasts(mtcars, c(cyl, gear) ~ sum_code)
#' enlist_contrasts(mtcars, all_of(c('cyl', 'gear')) ~ sum_code)
#' these_vars <- c("cyl", "gear")
#' enlist_contrasts(mtcars, all_of(these_vars) ~ sum_code)
#'
#' # You can also do something like this:
#' enlist_contrasts(mtcars, where(is.numeric) ~ sum_code)
#'
#' # Each variable name must only be set ONCE, eg these will fail:
#' enlist_contrasts(mtcars, cyl ~ sum_code, cyl ~ scaled_sum_code)
#' enlist_contrasts(mtcars, cyl ~ sum_code, all_of(these_vars) ~ scaled_sum_code)
#' enlist_contrasts(mtcars, cyl ~ sum_code, where(is.numeric) ~ scaled_sum_code)
#'}
enlist_contrasts <- function(model_data, ..., verbose = TRUE) {

  if (!inherits(model_data, "data.frame")) {
    if (inherits(model_data, "formula")) {
      stop("Formula passed to model_data, did you forget to pass a data frame?")
    }

    stop(
      "model_data should inherit class data.frame.\nInstead found class(es): ",
      paste0(class(model_data), collapse = ", ")
    )
  }

  # Get the formulas from the dots into list and character formats to work with
  formulas <- purrr::list_flatten(rlang::dots_list(...))
  if (identical(formulas, list())) {
    stop("No contrast formulas provided")
  }

  formulas <- .expand_formulas(formulas, model_data)
  lhs_variables <- names(formulas)

  model_data <- .convert_to_factors(model_data, lhs_variables, verbose)

  if (verbose) {
    .msg_if_remaining_factors(model_data, lhs_variables)
  }

  # Ignore factors with only 1 level to avoid undefined contrasts
  is_onelevel_factor <- vapply(
    lhs_variables,
    function(x) nlevels(model_data[[x]]) == 1L,
    logical(1)
  )

  .warn_if_onelevel(lhs_variables[is_onelevel_factor])

  formulas <- formulas[!is_onelevel_factor]
  lhs_variables <- lhs_variables[!is_onelevel_factor]

  if (length(formulas) == 0) {
    stop("No factors with more than 1 level found")
  }

  stats::setNames(
    lapply(
      seq_along(formulas),
      function(i) {
        # Reference value bindings
        .process_contrasts(model_data, raw_formula = formulas[[i]])
      }
    ),
    lhs_variables
  )
}


#' Pass arguments to contrast code
#'
#' Processes a formula and any arguments for the contrast matrix and sets up
#' the use_contrasts call
#'
#' @param model_data Data frame with factor column
#' @param raw_formula Raw formula
#'
#' @return A contrast matrix
.process_contrasts <- function(model_data, raw_formula) {
  var_envir <- rlang::get_env(raw_formula)

  params <- .split_if_language(.parse_formula(raw_formula), var_envir)

  use_contrasts(
    factor_col = get(params[["factor_col"]], model_data),
    code_by = eval(params[["code_by"]], var_envir),
    labels = eval(params[["labels"]], var_envir),
    reference_level = eval(params[["reference_level"]], var_envir),
    set_intercept = eval(params[["intercept_level"]], var_envir),
    drop_trends = eval(params[["drop_trends"]], var_envir),
    as_is = params[["as_is"]],
    other = params[["other_args"]]
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
  # In the event someone tries to do v ~ as_is(as_is(as_is(foo)))
  while (length(params[["code_by"]]) > 1L &&
         identical(params[["code_by"]][[1]], quote(as_is))) {
    params[["as_is"]] <- TRUE
    params[["code_by"]][1] <- NULL
  }

  params[["other_args"]] <- list()
  language_detected <-
    !is.symbol(params[["code_by"]]) && is.language(params[["code_by"]])

  # If the namespace is passed with the function, it will be a language
  # but we don't want to split it from the function name in that case
  not_namespace_symbol <-
    language_detected &&
    !identical(params[["code_by"]][[1]], quote(`::`)) &&
    !identical(params[["code_by"]][[1]], quote(`:::`))

  if (language_detected && not_namespace_symbol) {
    params[["other_args"]] <- as.list(params[["code_by"]])[-1]
    params[["code_by"]] <- params[["code_by"]][[1]]

    if (length(params[["other_args"]]) > 0) {
      for (arg_i in seq_along(params["other_args"])) {
        params[["other_args"]][arg_i] <-
          list(eval(params[["other_args"]][[arg_i]], var_envir))
      }
    }
  }
  params
}



#' Use contrast matrix as-is
#'
#' The default behavior for setting contrasts with a reference level is to move
#' the reference level to be the first level. However, there are some functions
#' that do not natively specify that already. Consider `contr.treatment` and
#' `contr.SAS`, which provide the same treatment-coded matrices BUT the former
#' moves the reference level to the first level while the latter sets the
#' reference level to the last level. The contrast between the two functions is
#' thus neutralized in this package. If we have reason to avoid this behavior,
#' especially for manually created matrices for niche situations, we can
#' suppress the behavior by wrapping the matrix/contrast function in `as_is`.
#'
#' By itself, this function does nothing more than return its input.
#' For the purposes of this package though, some functions will check to see if
#' a contrast matrix is wrapped in `as_is`.
#'
#' @param x Contrast matrix or contrast function
#'
#' @return x
#' @export
#'
#' @examples
#'
#' # These will return the same thing
#' enlist_contrasts(mtcars, carb ~ contr.treatment)
#' enlist_contrasts(mtcars, carb ~ contr.SAS)
#'
#' # This will suppress moving the reference level
#' enlist_contrasts(mtcars, carb ~ as_is(contr.SAS))
#'
#' # Particularly helpful for external matrices
#' # Imagine this is some other more complicated matrix
#' my_contrasts <- contr.SAS(6)
#'
#' # This is the same as as_is(contr.SAS)
#' enlist_contrasts(mtcars, carb ~ as_is(my_contrasts))
as_is <- function(x) {
  x
}
