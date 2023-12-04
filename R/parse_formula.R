#' Parse contrast formula
#'
#' Takes a formula and extracts the parameters for `use_contrasts` if it is
#' a valid formula. Validity checked via regexes, parameters extracted via
#' recursively parsing the abstract syntax tree.
#'
#' @param raw_formula Raw formula passed by user
#'
#' @return A list of parameters to use for a use_contrasts call
.parse_formula <- function(raw_formula) {
  .check_if_valid_formula(raw_formula)
  .make_parameters(raw_formula)
}

#' Formula validator
#'
#' Given a formula (and various preprocessed versions of it), run diagnostics
#' to ensure that it's a valid formula.
#'
#' @param formula Raw formula
#'
#' @return Nothing, throws an error if any are found
.check_if_valid_formula <- function(formula) {
  simplified_formula <- .simplify_formula(formula)
  simplified_formula_string <- deparse1(simplified_formula)

  if (grepl("[|][^*+-]+[*+-]", simplified_formula_string))
    stop("If using labels, | must be the last operator in the formula")

  if (grepl("[*+-][^~]+~",simplified_formula_string))
    stop("Formula must have 1 variable name on left hand side.")

  if (grepl("([|+*-]).+(\\1)", simplified_formula_string))
    stop("You may only use +, *, -, and | once")

  if (grepl(" ~ ([|+*-]|\\d)", simplified_formula_string))
    stop("First term in right hand side must be a symbol or function call")

  return(invisible(TRUE))
}
