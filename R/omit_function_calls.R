#' Recursively remove function calls from formula
#'
#' When working with the formula interface, users can pass in function calls
#' containing arguments. The most explicit example of this would be using a full
#' matrix call in the formula to specify the contrast matrix exactly. However,
#' this makes it difficult to check that the user is using the formula correctly.
#' This function recursively removes function calls from the formula and replaces
#' them with the string "CALL". This allows us to work with a simpler formula
#' object that is easier to check.
#'
#' @param rhs_node The right hand side of the formula, either `formula[[3]]` or
#' `rlang::f_rhs(formula)`. This is the node that will be recursed on.
#' @param mothers List of operators that have been encountered so far. This is
#' used to check that the user is not using the same operator more than once.
#'
#' @return Returns the right hand side of the formula with function calls removed
#' @export
#'
#' @examples
#' my_formula <-
#'   varname ~
#'   matrix(c(1,2,3),
#'          nrow = 3,
#'          ncol = 1) + 1 * get_whatever(param = 2) |
#'   c('one', 'two', 'three')
#'
#' # Returns `CALL() + 1 * CALL() | CALL()`
#' .omit_function_calls(rlang::f_rhs(my_formula))
.omit_function_calls <- function(rhs_node, mothers = list(`~`)) {
  # If the node is a function call then we're going to need
  # to do some pruning or recursion on the arguments
  if(is.call(rhs_node) && is.language(rhs_node)) {

    # If the function call is an operator that we've already encountered before,
    # then we can throw an error that we can only use the operator once
    if(.is_reserved_operator(rhs_node[[1L]])) {
      for (mother in mothers){
        if (identical(rhs_node[[1L]], mother))
          stop("You may only use +, -, and * once")
      }

      # If we haven't encountered this operator before, we need to recurse
      # through the arguments
      for (i in seq_along(rhs_node)[-1L]) {
        rhs_node[[i]] <- .omit_function_calls(rhs_node[[i]], c(mothers, rhs_node[[1]]))
      }
    } else {
      # If the function call is not an operator, replace it with the string "CALL"
      # and remove any arguments to the function call from the formula
      rhs_node[[1L]] <- quote(CALL)
      rhs_node[seq_along(rhs_node)[-1L]] <- NULL
    }
  }

  # If the current node is not a function call or all the function call
  # handling is done & the current node has been changed, return the current node
  return(rhs_node)
}

#' Simplify formula by reducing the right hand side
#'
#' This function takes a formula and simplifies the right hand side by removing
#' function calls and replacing them with the string "CALL". See `omit_function_calls`
#' for more details. The environment of the returned formula is the same as the
#' environment of the input formula.
#'
#' @param f Two-sided formula
#'
#' @return Returns a two-sided formula with the right hand side simplified
#' @export
#'
#' @examples
#' my_formula <-
#'   varname ~
#'   matrix(c(1,2,3),
#'          nrow = 3,
#'          ncol = 1) + 1 * get_whatever(param = 2) |
#'   c('one', 'two', 'three')
#'
#'  # Returns `varname ~ CALL() + 1 * CALL() | CALL()`
#' .simplify_formula(my_formula)
.simplify_formula <- function(f) {
  if (length(f) != 3L)
    stop("Formula must be two-sided")
  rhs <- rlang::f_rhs(f)
  lhs <- rlang::f_lhs(f)

  rhs <- .omit_function_calls(rhs)

  rlang::new_formula(lhs, rhs, env = environment(f))
}
