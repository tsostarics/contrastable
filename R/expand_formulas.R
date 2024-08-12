
#' Handle `+` in formula LHS
#'
#' Given a formula like cyl + gear ~ sum_code, this function recursively
#' replaces the `+` operators with `c` so that the LHS becomes `c(cyl, gear)`.
#'
#' This function doesn't recurse into other function calls, for example:
#' `cyl + gear + factor(1 + 2)` evaluates to `c(cyl, gear, factor(1+2))`.
#'
#' @param plus_expr Expression
#'
#' @return A new expression where `+` is replaced with `c()`
.plus_to_c <- function(plus_expr) {

  is_plus <- tryCatch(identical(plus_expr[[1]],
                                rlang::sym('+'),
                                ignore.environment = TRUE),
                      error = \(e) FALSE)
  if (is_plus) {
    plus_expr[[1]] <- rlang::sym("c")

    # Recurse into all arguments
    for (i in seq_along(plus_expr)[-1]) {
      if (!is.function(plus_expr[[i]])){
        plus_expr[[i]] <- .plus_to_c(plus_expr[[i]])
      }
    }
  }

  plus_expr
}


#' Expand contrast formulas
#'
#' Uses `{tidyselect]` functionality to expand the left hand side of a formula
#' into multiple formulas. See examples of `enlist_contrasts` for examples.
#'
#'
#' @param formulas List of formulas
#' @param data Dataframe to evaluate names in
#'
#' @return Named list of formulas
#' @importFrom tidyselect eval_select
.expand_formulas <- function(formulas, data) {
  formulas <-
    lapply(formulas,
           \(formula) {
             lhs <- tryCatch(rlang::f_lhs(formula),

                             error = function(c) {
                               err <- conditionMessage(c)
                               if (!grepl("must be a formula", err)) {
                                 stop(c)
                               }
                               stop(
                                 paste(err,
                                       "Did you use = instead of ~ when setting the contrast?",
                                       sep = "\n")
                               )
                             }
             )

             rhs <- rlang::f_rhs(formula)
             env <- rlang::f_env(formula)

             # convert LHS like cyl + gear to c(cyl, gear), eval_select
             # will check if the columns exist in the data & handle any
             # selecting helpers like where(is.numeric)
             expanded_expression <- .plus_to_c(lhs)
             varnames <- names(eval_select(expanded_expression, data, env))

             formulas <- lapply(rlang::syms(varnames),
                                \(varname) {
                                  rlang::new_formula(varname, rhs, env)
                                })

             names(formulas) <- varnames
             formulas
           }
    )

  tryCatch(purrr::list_flatten(formulas, name_repair = "check_unique"),
           error = \(c) {
             err <- conditionMessage(c)
             stop(paste(err,
                        "Left hand side of multiple formulas evaluated to the same column name",
                        sep = "\n"))
           })

}

#' Check for unordered factor
#'
#' Helper to check if a factor is exclusively unordered. is.factor(x) is TRUE
#' when x is unordered OR ordered.
#'
#' @param x a vector of data
#'
#' @return logical, TRUE if x is an unordered factor, FALSE if x is not a
#' factor or is an ordered factor
#' @export
is.unordered <- function(x) {
  is.factor(x) & !is.ordered(x)
}
