#' Make parameters for contrast code call
#'
#' Given a formula, recursively go through the abstract syntax tree and
#' extract the necessary parameters for `use_contrasts`. While this method is
#' more involved than extracting from a parsed string representation, I think
#' it handles matrix calls better since it plucks the call right out of
#' the syntax tree.
#'
#' @param formula Formula given by user
#' @param params Parameter accumulator
#' @param env Environment for the formula, on the first recursion this will be
#' pulled from `formula` and then passed to subsequent recursions. Needed to
#' check whether a function is actually a function.
#' @param EMBEDDED Whether the current operation is embedded in another, helps
#' to handle operator precedence with *
#'
#' @return Named list of parameters that can be evaluated in `enlist_contrasts`
.make_parameters <- function(formula,
                             params = list(
                               "factor_col" = NA,
                               "code_by" = NA,
                               "reference_level" = NA,
                               "intercept_level" = NA,
                               "drop_trends" = NA,
                               "labels" = NULL,
                               "as_is" = FALSE
                             ),
                             env = NULL,
                             EMBEDDED = FALSE) {
  cur_expr <- as.list(formula)
  node <- cur_expr[[1]]
  if (is.null(env)) {
    env <- tryCatch(rlang::get_env(formula), error = \(e) NULL)
  }

  # Is the current node an operator for the package syntax? if so, process
  # the arguments for that operator appropriately and continue recursing.
  # if not, then we're at the top level and need to process the whole formula
    switch(
      .get_reserved_operator(node),
      "~" = {params <- .process_factor_col(cur_expr, params, env)},
      "+" = {params <- .set_param(cur_expr, params, env, "reference_level")},
      "-" = {params <- .set_param(cur_expr, params, env, "drop_trends")},
      "*" = {params <- .set_param(cur_expr, params, env, "intercept_level")},
      "|" = {params <- .set_param(cur_expr, params, env, "labels")},
      {params <- .process_code_by(formula, params, env)}
    )

  params
}

#' Check for reserved operators
#'
#' This is a helper for `.make_parameters`, which takes a symbol and checks
#' whether it corresponds to one of the reserved operators for the package
#' syntax.
#'
#' @param node A symbol, extracted from a formula
#'
#' @return If `node` is a reserved operator, then return the operator as a
#' string. Otherwise return the string "none".
.get_reserved_operator <- function(node) {
  for (op_symbol in c("~","+", "-", "*", "|")) {
    if (identical(node, sym(op_symbol)))
      return(op_symbol)
  }

  "none"
}

.is_reserved_operator <- function(node, check_sym = NULL) {
  if (!missing(check_sym)) {
    ops <- syms(check_sym)
  } else {
    ops <- syms(c("+", "-", "*", "|"))
  }

  any(vapply(ops, function(x) identical(node, x), FUN.VALUE = TRUE))
}

## Process each of the various operators and assign the relevant parameters
## while recursing into the rest of the expression

.process_factor_col <- function(cur_expr, params, env) {
  params[["factor_col"]] <- cur_expr[[2L]]
  params <- .make_parameters(cur_expr[[3L]], params, env)
  params
}


#' Process and set parameter
#'
#' Unpacks the given expression to set the parameter specified by `which_param`
#' to `params`. Continues recursively setting parameters via `make_parameters`.
#'
#' @param cur_expr Current expression, a formula or list representation thereof
#' @param params Named list of parameters
#' @param env Environment to evaluate expressions in `cur_expr` in
#' @param which_param Which parameter to set, a string, see `make_parameters`
#' for usage
#'
#' @return `params`
.set_param <- function(cur_expr, params, env, which_param) {
  LHS <- cur_expr[[2L]]
  RHS <- tryCatch(cur_expr[[3L]],
                  error = \(e) {
                    err <- conditionMessage(e)
                    stop(
                      cli::format_error(c(err,
                                          " " = "Is the contrast object/function in the wrong place? See example:",
                                          "x" = "var ~ 1 + sum_code",
                                          "v" = "var ~ sum_code + 1"))
                    )
                  })

  if (which_param == "labels") {
    already_set <- !is.null(params[[which_param]])
  } else {
    already_set <- !is.na(params[[which_param]])
  }

  if (already_set)
    stop("You may only use *, -, and * once")

  # Unnest the RHS if there are more reserved operators present
  while (length(RHS) > 1L && .is_reserved_operator(RHS[[1L]])) {
    LHS <- list(RHS[[1]], LHS, RHS[[3]])
    RHS <- RHS[[2]]
  }

  params[[which_param]] <- RHS
  params <- .make_parameters(LHS, params, env)
  params

  params
}

.process_code_by <- function(formula, params, env) {

  # Check if the formula contains AsIs specification
  if (length(formula) > 1L && (identical(formula[[1]], sym("I")))) {

    not_singleton <- TRUE

    # Unwrap nested I()s
    while(not_singleton && identical(formula[[1]], sym("I"))){
      formula <- formula[[2L]]
      not_singleton <- length(formula) > 1
    }

    params[["as_is"]] <- TRUE

    if (not_singleton)
      formula <- formula[[2]]
  }

  # Remove parentheses to treat as symbol
  if (is.call(formula) && length(formula) == 1L) {
      formula <- formula[[1]]
  }

  params[["code_by"]] <- formula

  params
}
