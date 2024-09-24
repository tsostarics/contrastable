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
#' @param verbose Logical, default `TRUE`, whether to show drop_trends warning
#' if used incorrectly
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
                             verbose = TRUE) {
  cur_expr <- as.list(formula)
  node <- cur_expr[[1]]
  if (is.null(env)) {
    env <- tryCatch(rlang::get_env(formula), error = \(e) NULL)
  }

  set <- \(x) .set_param(cur_expr, params, env, x, verbose)

  # Is the current node an operator for the package syntax? if so, process
  # the arguments for that operator appropriately and continue recursing.
  # if not, then we're at the top level and need to process the whole formula
  params <-
    switch(
      .get_reserved_operator(node),
      "~" = .process_factor_col(cur_expr, params, env, verbose),
      "+" = set("reference_level"),
      "-" = set("drop_trends"),
      "*" = set("intercept_level"),
      "|" = set("labels"),
      .process_code_by(formula, params, env, verbose)
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

.process_factor_col <- function(cur_expr, params, env, verbose) {
  params[["factor_col"]] <- cur_expr[[2L]]
  params <- .make_parameters(cur_expr[[3L]], params, env, verbose)
  params
}


#' Process and set parameter
#'
#' Unpacks the given expression to set the parameter specified by `which_param`
#' to `params`. Continues recursively setting parameters via `make_parameters`.
#'
#' @param cur_expr Current expression, a formula or list representation thereof
#' @param which_param Which parameter to set, a string, see `make_parameters`
#' for usage
#' @inheritParams .make_parameters
#'
#' @return `params`
.set_param <- function(cur_expr, params, env, which_param, verbose) {
  LHS <- cur_expr[[2L]]
  RHS <-
    tryCatch(
      cur_expr[[3L]],
      error = \(e) {
        err <- conditionMessage(e)
        stop(
          cli::format_error(
            c(err,
              " " = "Is the contrast object/function in the wrong place? See example:", # nolint
              "x" = "var ~ +1 + sum_code",
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
  params <- .make_parameters(LHS, params, env, verbose)
  params

  params
}

.process_code_by <- function(formula, params, env, verbose) {
  node_is_I <- \(f) identical(f[[1]], sym("I"))

  # Check if the formula contains AsIs specification
  if (length(formula) > 1L && node_is_I(formula)) {

    not_singleton <- TRUE

    # Unwrap nested I()s
    while(not_singleton && node_is_I(formula)) {
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
  has_drop_trends <- !identical(params[["drop_trends"]], NA)
  is_not_polynomial <- !.is_polynomial_scheme(as.character(params[['code_by']]))

  if (has_drop_trends && is_not_polynomial) {
    params[["drop_trends"]] <- NA
    if (verbose)
      warning("Ignoring the `-` operator: should only be used with polynomial contrasts", # nolint
              call. = FALSE)
  }

  params
}
