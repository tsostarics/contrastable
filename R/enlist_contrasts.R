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
#' @return List of named contrast matrices. Internally, if called within
#' set_contrasts, will return a named list with `contrasts` equal to the list
#' of named contrast matrices and `data` equal to the passed `model_data` with
#' any factor coercions applied (so that `set_contrasts` doesn't need to do
#' it a second time).
#' @seealso [set_contrasts()]
#' @export
#'
#'@importFrom rlang expr enquo enquos sym syms := ensym ensyms f_rhs f_lhs
#'new_formula get_env dots_list get_expr
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
#'   gear ~ scaled_sum_code,
#'   carb ~ helmert_code,
#'   verbose = FALSE
#' )
#'
#' # Add reference levels with +
#' enlist_contrasts(
#'   my_df,
#'   gear ~ scaled_sum_code + 5,
#'   carb ~ contr.sum + 6,
#'   verbose = FALSE
#' )
#' # Manually specifying matrix also works
#' enlist_contrasts(
#'   my_df,
#'   gear ~ matrix(c(1, -1, 0, 0, -1, 1), nrow = 3),
#'   carb ~ forward_difference_code,
#'   verbose = FALSE
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
#'   carb ~ forward_difference_code,
#'   verbose = FALSE
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
#' # Use tidyselect helpers to set multiple variables at once
#' # These are all equivalent
#' contr_list1 <- enlist_contrasts(mtcars,
#'                  cyl ~ sum_code, gear ~ sum_code,
#'                  verbose = FALSE)
#'
#' contr_list2 <- enlist_contrasts(mtcars,
#'                  cyl + gear ~ sum_code,
#'                  verbose = FALSE)
#'
#' contr_list3 <- enlist_contrasts(mtcars,
#'                  c(cyl, gear) ~ sum_code,
#'                  verbose = FALSE)
#'
#' contr_list4 <- enlist_contrasts(mtcars,
#'                  all_of(c('cyl', 'gear')) ~ sum_code,
#'                  verbose = FALSE)
#'
#'
#' these_vars <- c("cyl", "gear")
#' contr_list5 <- enlist_contrasts(mtcars,
#'                                 all_of(these_vars) ~ sum_code,
#'                                 verbose = FALSE)
#'
#' all.equal(contr_list1, contr_list2)
#' all.equal(contr_list2, contr_list3)
#' all.equal(contr_list3, contr_list4)
#' all.equal(contr_list4, contr_list5)
#'
#' # You can also do something like this:
#' contr_list6 <- enlist_contrasts(mtcars,
#'                                 where(is.numeric) ~ sum_code,
#'                                 verbose = FALSE)
#'
#' # Each variable name must only be set ONCE, eg these will fail:
#' try(enlist_contrasts(mtcars,
#'                      cyl ~ sum_code,
#'                      cyl ~ scaled_sum_code,
#'                      verbose = FALSE))
#' try(enlist_contrasts(mtcars,
#'                      cyl ~ sum_code,
#'                      all_of(these_vars) ~ scaled_sum_code,
#'                      verbose = FALSE))
#' try(enlist_contrasts(mtcars,
#'                      cyl ~ sum_code,
#'                      where(is.numeric) ~ scaled_sum_code,
#'                      verbose = FALSE))
enlist_contrasts <- function(model_data,
                             ...,
                             verbose = getOption("contrastable.verbose")) {

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

  # If this is called by means of set_contrasts, then we need to ignore and warn
  # about any usage of the `-` operator and also return the factor coerced data
  embedded_call <- sys.call(sys.parent(1L))[[1L]]

  # Clear any namespacing
  if (length(embedded_call) > 1L)
    embedded_call <- embedded_call[[3L]]

  is_embedded <- identical(rlang::sym("set_contrasts"), embedded_call)

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
    logical(1L)
  )

  .warn_if_onelevel(lhs_variables[is_onelevel_factor])

  formulas <- formulas[!is_onelevel_factor]
  lhs_variables <- lhs_variables[!is_onelevel_factor]

  if (length(formulas) == 0L) {
    stop("No factors with more than 1 level found")
  }

  contrast_list <-
    stats::setNames(
      lapply(
        seq_along(formulas),
        function(i) {
          # Reference value bindings
          .process_contrasts(model_data, raw_formula = formulas[[i]], is_embedded)
        }
      ),
      lhs_variables
    )

  # If called within set_contrasts, also return model_data with the coerced
  # factor columns
  if (is_embedded)
    return(list(contrasts = contrast_list,
                data = model_data))

  contrast_list
}


#' Pass arguments to contrast code
#'
#' Processes a formula and any arguments for the contrast matrix and sets up
#' the use_contrasts call
#'
#' @param model_data Data frame with factor column
#' @param raw_formula Raw formula
#' @param omit_drop Logical, set to TRUE when set_contrasts is used by
#' appending an attribute to the formula list, otherwise is FALSE (as in
#' enlist_contrasts)
#'
#' @return A contrast matrix
.process_contrasts <- function(model_data, raw_formula, omit_drop) {
  var_envir <- rlang::get_env(raw_formula)

  params <- .split_if_language(.make_parameters(raw_formula), var_envir)

  # If the user used set_contrasts, then we need to remove drop_trends from
  # the parameters and warn the user that the - operator can't be used
  # (the missing comparisons are filled in with something else)
  if (omit_drop) {
    if (!identical(params[['drop_trends']], NA))
      warning(
        paste0("Cannot use `-` with set_contrasts, ignoring in ",
               deparse1(raw_formula),
               ".\n  Use enlist_contrasts instead."),
        call. = FALSE
      )
    params[['drop_trends']] <- NA
  }


  code_by_value <- eval(params[["code_by"]], var_envir)
  code_by_sym <- as.character(params[["code_by"]])[1L]

  use_contrasts(
    factor_col = get(params[["factor_col"]], model_data),
    code_by = code_by_value,
    labels = eval(params[["labels"]], var_envir),
    reference_level = eval(params[["reference_level"]], var_envir),
    set_intercept = eval(params[["intercept_level"]], var_envir),
    drop_trends = eval(params[["drop_trends"]], var_envir),
    as_is = params[["as_is"]],
    other = params[["other_args"]],
    symchar = code_by_sym
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
