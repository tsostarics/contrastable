#' Glimpse contrasts in dataframe
#'
#' Uses the same syntax as `enlist_contrasts` and `set_contrasts`. Returns
#' a summary table of the contrasts you've set. If you set `return.list` to TRUE
#' then you can access a list of contrasts in the second element of the resulting
#' list. The glimpse dataframe is the first element. FALSE will return just
#' the glimpse data frame.
#'
#' Todo: Include an all.factors argument that will get the contrasts from
#' anything not explicitly set in the function call. May need to be a separate
#' helper whose output is rbinded to avoid dealing with different lengths. maybe
#' use make parameters on the contrast arguments?
#'
#' @param model_data Data to be passed to a model fitting function
#' @param ... Series of formulas
#' @param return.list Logical, defaults to FALSE, whether the output of enlist_contrasts should be
#' returned
#' @param verbose Logical, defaults to FALSE, whether messages should be printed
#'
#' @return A dataframe is return.list is FALSE, a list with a dataframe and list
#' of named contrasts if TRUE.
#' @export
glimpse_contrasts <- function(model_data, ..., return.list = FALSE,verbose=FALSE) {
  formulas <- suppressWarnings(rlang::dots_splice(...)) # outer names warning?
  contrast_list <- enlist_contrasts(model_data, ..., verbose=verbose)
  params <- lapply(formulas, .make_parameters)

  # Extract various information
  factor_names <- names(contrast_list)
  factor_sizes <- vapply(contrast_list, nrow, 1L, USE.NAMES = FALSE)
  level_names <- unname(lapply(contrast_list, rownames))
  scheme_labels <- .get_scheme_labels(params, formulas)
  reference_levels <- .get_reference_levels(contrast_list, params, formulas)
  orthogonal_contrasts <- vapply(contrast_list, is_orthogonal, TRUE, USE.NAMES = FALSE)
  intercept_interpretations <- vapply(contrast_list, interpret_intercept,"char", USE.NAMES = FALSE)

  # Double check that dropped trends are only included for polynomial contrasts
  dropped_trends <-  .get_dropped_trends(params, formulas)
  which_are_polynomials <- vapply(scheme_labels, .is_polynomial_scheme, TRUE)
  dropped_trends[!which_are_polynomials] <- NA

  glimpse <- tibble::tibble("factor" = factor_names,
                        "n_levels" = factor_sizes,
                        "level_names" = level_names,
                        "scheme" = scheme_labels,
                        "reference_level" = reference_levels,
                        "intercept" = intercept_interpretations,
                        "orthogonal" = orthogonal_contrasts,
                        "dropped_trends" = dropped_trends)



  if (return.list)
    return(list("glimpse" = glimpse, "contrasts" = contrast_list))

  glimpse
}

.get_dropped_trends <- function(params, formulas) {
  vapply(seq_along(params),
         function(i){
           trends <- eval(params[[i]][['drop_trends']], rlang::get_env(formulas[[i]]))
           if (NA %in% trends)
             return(NA_character_)
           paste(trends, collapse = ",")
         },
         "char")
}

.get_scheme_labels <- function(params, formulas) {
  vapply(seq_along(params), \(i) {
    scheme <- deparse1(params[[i]][["code_by"]])
    if (grepl("^matrix\\(", scheme))
      return("custom")
    function_used <- is.function(get(scheme, rlang::get_env(formulas[[i]])))
    if (function_used)
      return(scheme)
    return("custom")
  },
  FUN.VALUE = "char",
  USE.NAMES = FALSE
  )
}

.get_reference_levels <- function(contrast_list, params, formulas) {
  reference_levels <- vapply(params,
                             function(param)
                               ifelse(is.na(param[['reference_level']]),
                                      NA_character_,
                                      deparse1(param[["reference_level"]])),
                             "char")

  for(i in seq_along(reference_levels)) {
    if (is.na(reference_levels[i])){
      intuition <- .intuit_reference_level(contrast_list[[i]],
                                           rownames(contrast_list[[i]]))
      if (!is.na(intuition))
        reference_levels[[i]] <- intuition
    }
  }
  reference_levels
}

.intuit_reference_level <- function(contr_mat, factor_levels) {
  if (ncol(contr_mat) < (length(factor_levels) - 1))
    return(NA)
  hyp_mat <- .contrasts_to_hypotheses(contr_mat, nrow(contr_mat))
  which_is_ref <- apply(hyp_mat[,-1], 1, function(x) all(x < 0))
  if (sum(which_is_ref) != 1)
    return(NA)

  factor_levels[which_is_ref]
}

interpret_intercept <- function(contr_mat) {
  .nlevels <- nrow(contr_mat)
  # Account for polynomial contrasts with dropped trends, resulting in non-square
  if (ncol(contr_mat) < (.nlevels - 1))
    return("grand mean")
  intercept_column <- .contrasts_to_hypotheses(contr_mat, nrow(contr_mat))[,1]

  # Check if grand mean (most common)
  is_grandmean <- all(round(intercept_column - (1/.nlevels), 10) == 0)
  if (is_grandmean)
    return("grand mean")


  level_names <- rownames(contr_mat)

  # Check if 1 level (eg contr.treatment)
  contributing_levels <- intercept_column != 0
  if (sum(contributing_levels) == 1){
    mean_levels <- paste(level_names[contributing_levels], collapse = ", ")
    return(glue::glue("mean({mean_levels})"))
  }

  return("custom weights")
}