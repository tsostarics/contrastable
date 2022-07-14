#' Parse contrast formula
#'
#' Takes a formula and extracts the parameters for `contrast_code` if it is
#' a valid formula. Validity checked via regexes, parameters extracted via
#' recursively parsing the abstract syntax tree.
#'
#' @param raw_formula Raw formula passed by user
#'
#' @return A list of parameters to use for a contrast_code call
.parse_formula <- function(raw_formula) {
  char_formula <-  deparse1(raw_formula)
  no_matrix_string <- gsub(r"(matrix\((.+\(.+\)?)(, .+)*\) ?)","",char_formula)
  .check_if_valid_formula(raw_formula, char_formula, no_matrix_string)

  .make_parameters(raw_formula)
}

#' Formula validator
#'
#' Given a formula (and various preprocessed versions of it), run diagnostics
#' to ensure that it's a valid formula.
#'
#' @param formula Raw formula
#' @param char_formula Character string of the formula from deparse
#' @param no_matrix_string String without any matrix calls
#'
#' @return Nothing, throws an error if any are found
.check_if_valid_formula <- function(formula, char_formula, no_matrix_string) {
  if (length(formula) != 3L)
    stop("Formula must be two sided.")
  if (grepl("[|]", no_matrix_string) && length(formula[[3]][[3]]) > 1 && .is_operator(formula[[3L]][[3L]][[1L]]))
      stop("If using labels, | must be the last operator in the formula")
  no_matrix_string <- gsub("[|].+$", "", no_matrix_string)
  if (grepl("[*+-][^~]+~",no_matrix_string))
    stop("Formula must have 1 variable name on left hand side.")
  if (any(stringr::str_count(no_matrix_string, c("\\+", "\\*", "\\-", "\\|")) > 1))
    stop("You may only use +, *, -, and | once")
  if (any(stringr::str_detect(no_matrix_string, c("%in%", "\\^"))))
    stop("You cannot use the ^ or %in% operators in this formula")
  if (grepl("[^-=] [^ ]+:[^ ]+", no_matrix_string))
    stop("Sequences of the form a:b may only be used to drop trends with the - operator")
  if (grepl(" ~ ([+-]|\\d)", char_formula))
    stop("First term in right hand side must be a contrast matrix or contrast function")
  return(invisible(TRUE))
}
