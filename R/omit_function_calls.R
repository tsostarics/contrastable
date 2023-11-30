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
#'
#' @return Returns the right hand side of the formula with function calls removed
#' @export
#'
#' @examples
#' my_formula <-
#'   varname ~
#'   matrix(c(1,2,3),
#'          nrow = 3,
#'          ncol = 1) + 1 * get_whatever(asd = 2) |
#'   c('one', 'two', 'three')
#'
#' # Returns `CALL() + 1 * CALL() | CALL()`
#' .omit_function_calls(rlang::f_rhs(my_formula))
.omit_function_calls <- function(rhs_node) {
  # If the node is a function call then we're going to need
  # to do some pruning or recursion on the arguments
  if(class(rhs_node) == "call" && typeof(rhs_node) == "language") {
    # If the function call is an operator
    if(.is_reserved_operator(rhs_node[[1]])) {
      # Recursively call omit_function_calls on the arguments to the operator
      for (i in seq_along(rhs_node)[-1]) {
        rhs_node[[i]] <- .omit_function_calls(rhs_node[[i]])
      }
    } else {
      # If the function call is not an operator, replace it with the string "CALL"
      # and remove any arguments to the function call from the formula
      rhs_node[[1]] <- quote(CALL)
      rhs_node[seq_along(rhs_node)[-1]] <- NULL
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
#'          ncol = 1) + 1 * get_whatever(asd = 2) |
#'   c('one', 'two', 'three')
#'
#'  # Returns `varname ~ CALL() + 1 * CALL() | CALL()`
#' .simplify_formula(my_formula)
.simplify_formula <- function(f) {
  if (length(f) != 3)
    stop("Formula must be two-sided")
  rhs <- rlang::f_rhs(f)
  lhs <- rlang::f_lhs(f)

  rhs <- .omit_function_calls(rhs)

  rlang::new_formula(lhs, rhs, env = environment(f))
}
