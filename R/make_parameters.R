
#' Make parameters for contrast code call
#'
#' Given a formula, recursively go through the abstract syntax tree and
#' extract the necessary parameters for `contrast_code`. While this method is
#' more involved than extracting from a parsed string representation, I think
#' it handles matrix calls better since it plucks the call right out of
#' the syntax tree.
#'
#' @param formula Formula given by user
#' @param params Parameter accumulator
#' @param EMBEDDED Whether the current operation is embedded in another, helps
#' to handle operator precedence with *
#'
#' @return Named list of parameters that can be evaluated in `enlist_contrasts`
.make_parameters <- function(formula,
                             params = list("factor_col" = NA,
                                           "code_by" = NA,
                                           "reference_level" = NA,
                                           "intercept_level" = NA,
                                           "drop_trends" = NA,
                                           "labels" = NULL),
                             EMBEDDED = FALSE){
  cur_expr <- as.list(formula)
  node <- cur_expr[[1]]

  if (identical(node, sym("~"))){
    params[["factor_col"]] <- cur_expr[[2L]] # LHS is factor name
    params <- .make_parameters(cur_expr[[3L]], params)


  } else if (.is_operator(node, "+")) {
    LHS <- cur_expr[[2L]]
    RHS <- cur_expr[[3L]]
    r_has_child <- length(RHS) == 3
    params <- .make_parameters(LHS, params)

    # Must check if rhs has children before subsetting with +
    if (r_has_child && .is_operator(RHS[[1L]], "*")){
      params[["reference_level"]] <- RHS[[2L]]
      params <- .make_parameters(RHS, params, TRUE)
    } else {
      params[["reference_level"]] <- RHS
    }


  } else if (.is_operator(node, "-")) {
    LHS <- cur_expr[[2L]]
    RHS <- cur_expr[[3L]]
    if (.is_operator(RHS[[1L]], "*")){
      params[["drop_trends"]] <- RHS[[2L]]
      params <- .make_parameters(RHS, params, TRUE)
      params <- .make_parameters(LHS, params)
    } else {
      params[["drop_trends"]] <- RHS
      params <- .make_parameters(LHS, params)
    }


  } else if (.is_operator(node, "*")) {
    LHS <- cur_expr[[2L]]
    RHS <- cur_expr[[3L]]
    params[["intercept_level"]] <- RHS
    # If we don't check whether * is embedded in - or + before recursing
    # we will overwrite code_by on accident
    if (!EMBEDDED)
      params <- .make_parameters(LHS, params)


  } else if (.is_operator(node, "|")) {
    LHS <- cur_expr[[2L]]
    RHS <- cur_expr[[3L]]
    params[["labels"]] <- RHS
    params <- .make_parameters(LHS, params)
  } else {
    params[["code_by"]] <- formula
  }

  return(params)
}

.is_operator <- function(node, check.sym = NULL) {
  if (!missing(check.sym))
    ops <- syms(check.sym)
  else
    ops <- syms(c("+","-","*","|"))

  any(vapply(ops, function(x) identical(node, x), FUN.VALUE = TRUE))
}
