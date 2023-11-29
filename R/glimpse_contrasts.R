#' Glimpse contrasts in dataframe
#'
#' Uses the same syntax as `enlist_contrasts` and `set_contrasts`. Returns
#' a summary table of the contrasts you've set. If you set `return.list` to TRUE
#' then you can access a list of contrasts in the second element of the resulting
#' list. The glimpse dataframe is the first element. FALSE will return just
#' the glimpse data frame.
#'
#' @param model_data Data to be passed to a model fitting function
#' @param ... Series of formulas
#' @param return.list Logical, defaults to FALSE, whether the output of enlist_contrasts should be
#' returned
#' @param verbose Logical, defaults to FALSE, whether messages should be printed
#' @param all.factors Logical, defaults to TRUE, whether the factors not
#' explicitly set with formulas should be included
#' @param add_namespace Logical, defaults to FALSE, whether to append the
#' namespace of the contrast scheme to the scheme name
#' @param incl.one.levels Logical, should factors with only one level be included
#' in the output? Default is FALSE to omit
#'
#' @return A dataframe is return.list is FALSE, a list with a dataframe and list
#' of named contrasts if TRUE.
#' @export
#'
#' @importFrom tibble tibble
glimpse_contrasts <- function(model_data,
                              ...,
                              return.list = FALSE,
                              verbose = FALSE,
                              all.factors = TRUE,
                              add_namespace = FALSE,
                              incl.one.levels = FALSE) {
  formulas <- purrr::list_flatten(rlang::dots_list(...)) # outer names warning?
  contrast_list <- enlist_contrasts(model_data, ..., 'verbose' = verbose)
  params <- lapply(formulas, .make_parameters)

  # Ignore explicitly set factors that actually only have one level
  is_onelevel_factor <- vapply(params,
                               function(x) nlevels(model_data[[x[["factor_col"]]]]) == 1L,
                               TRUE)

  formulas <- formulas[!is_onelevel_factor]
  params <- params[!is_onelevel_factor]

  # Extract various information
  # factor_names <- .cols_where(model_data, is.factor, return.names = TRUE)
  set_factors <- names(contrast_list)
  factor_sizes <- vapply(contrast_list, nrow, 1L, USE.NAMES = FALSE)
  level_names <- unname(lapply(contrast_list, rownames))
  scheme_labels <- .get_scheme_labels(params, formulas)
  reference_levels <- .get_reference_levels(contrast_list, params, formulas)
  orthogonal_contrasts <- is_orthogonal(contrast_list)
  centered_contrasts <- is_centered(contrast_list)
  intercept_interpretations <- vapply(contrast_list, interpret_intercept,"char", USE.NAMES = FALSE)

  # Double check that dropped trends are only included for polynomial contrasts
  dropped_trends <-  .get_dropped_trends(params, formulas)
  which_are_polynomials <- vapply(scheme_labels, .is_polynomial_scheme, TRUE)
  dropped_trends[!which_are_polynomials] <- NA

  glimpse <- tibble::tibble("factor" = set_factors,
                            "n_levels" = factor_sizes,
                            "level_names" = level_names,
                            "scheme" = scheme_labels,
                            "reference" = reference_levels,
                            "intercept" = intercept_interpretations,
                            "orthogonal" = orthogonal_contrasts,
                            "centered" = centered_contrasts,
                            "dropped_trends" = dropped_trends,
                            "explicitly_set" = TRUE)



  if (all.factors)
    glimpse <- rbind(glimpse, .glimpse_default_factors(model_data, set_factors, incl.one.levels, verbose))

  if (add_namespace)
    glimpse$scheme <- .add_namespace(glimpse$scheme)

  # The default factors don't need to be specified in the contrast list,
  # they'll just use their respective defaults by the model fitting function
  if (return.list)
    return(list("glimpse" = glimpse, "contrasts" = contrast_list))


  glimpse
}

.add_namespace <- function(scheme_names) {
  vapply(scheme_names,
         \(n) {
           namespace <- gsub("package:", "", utils::find(n), perl = TRUE)
           separator <- "::"
           if (identical(namespace, character(0))){
             namespace <- ""
             separator <- ""
           }

           paste0(namespace, separator, n)
         }, "char", USE.NAMES = FALSE)
}

.clean_schemes <- function(scheme_labels) {
  vapply(scheme_labels,
         function(x) {
           x <- gsub("contr\\.poly","orth_polynomial",x)
           gsub("(^contr\\.)|(_code$)","",x)
         },
         "char",
         USE.NAMES = FALSE
  )
}

#' Glimpse default factors
#'
#' Given a dataframe with some factor columns and a character vector of
#' which columns you've already set yourself, look at all the other
#' factor columns and get a glimpse at how they're treated by the defaults
#' specified in `options('contrasts')`. Reference level is assumed to be the
#' first level for unordered factors and nonexistent for ordered factors.
#'
#' @param model_data Dataframe
#' @param set_factors Explicitly set columns to ignore
#' @param incl.one.levels Should factor columns with only 1 level be included? Default FALSE
#' @param verbose Defaults to FALSE, should messages and warnings be printed?
#'
#' @return A table with information about the contrasts for all remaining factor
#' columns
.glimpse_default_factors <- function(model_data,
                                     set_factors = NULL,
                                     incl.one.levels = FALSE,
                                     verbose = TRUE) {
  fct_info <- .get_factor_info(model_data, set_factors, verbose)
  unset_factors <- fct_info[["unset_factors"]]
  is_ordered_factor <- fct_info[["is_ordered_factor"]]
  unset_factors <- unset_factors[!unset_factors %in% fct_info[["one_level_factors"]]]
  # Extract contrasts from the factor columns
  new_contrasts <- lapply(unset_factors, function(x) contrasts(model_data[[x]]))

  factor_sizes <- vapply(unset_factors,
                         function(x) nlevels(model_data[[x]]),
                         FUN.VALUE = 1L)
  level_names <- lapply(unset_factors, function(x) levels(model_data[[x]]))
  scheme_labels <- vapply(unset_factors,
                          function(x)
                            ifelse(is_ordered_factor[x],
                                   options('contrasts')[[1]]["ordered"],
                                   options('contrasts')[[1]]["unordered"]),
                          FUN.VALUE = "char")
  reference_levels <- vapply(unset_factors,
                             function(x)
                               ifelse(is_ordered_factor[x],
                                      NA_character_,
                                      as.character(levels(model_data[[x]])[[1L]])),
                             "char")
  intercept_interpretations <- vapply(new_contrasts, interpret_intercept, "char", USE.NAMES = FALSE)
  orthogonal_contrasts <-  is_orthogonal(new_contrasts)
  centered_contrasts <- is_centered(new_contrasts)

  dropped_trends <- rep(NA, length(unset_factors)) # Trends are never dropped w/ R's defaults

  .warn_if_nondefault(new_contrasts, unset_factors, factor_sizes, is_ordered_factor)

  glimpse <- tibble::tibble("factor"         = unset_factors,
                            "n_levels"       = factor_sizes,
                            "level_names"    = level_names,
                            "scheme"         = scheme_labels,
                            "reference"      = reference_levels,
                            "intercept"      = intercept_interpretations,
                            "orthogonal"     = orthogonal_contrasts,
                            "centered"       = centered_contrasts,
                            "dropped_trends" = dropped_trends,
                            "explicitly_set" = FALSE)

  if (incl.one.levels)
    glimpse <- rbind(glimpse, .make_placeholder_glimpse(model_data, fct_info[["one_level_factors"]]))

  glimpse
}

#' Make glimpse for 1-level factors
#'
#' If the user wants factors with only one level included, this will create
#' the table to present that information. It's a lot of NAs because contrasts
#' aren't defined for only 1 level.
#'
#' @param model_data Model data
#' @param one_level_factors Which factors are one level
#'
#' @return A tibble with limited information about one level factors
.make_placeholder_glimpse <- function(model_data, one_level_factors) {
  # Will be a list of 1-length character vectors to match list column type
  level_names <- lapply(one_level_factors,
                        function(x) levels(model_data[[x]]))

  tibble::tibble("factor"         = one_level_factors,
                 "n_levels"       = 1L,
                 "level_names"    = level_names,
                 "scheme"         = NA_character_,
                 "reference"      = NA_character_,
                 "intercept"      = NA_character_,
                 "orthogonal"     = NA,
                 "centered"       = NA,
                 "dropped_trends" = NA_character_,
                 "explicitly_set" = NA)
}

.get_factor_info <- function(model_data, set_factors = NULL, verbose = TRUE){
  all_factors <- .cols_where(model_data, is.factor, return.names = TRUE)
  unset_factors <- all_factors[!all_factors %in% set_factors]
  is_ordered_factor <- unset_factors %in% .cols_where(model_data, is.ordered, return.names = TRUE)
  names(is_ordered_factor) <- unset_factors

  # Filter out any factors that only have 1 level to avoid undefined contrasts
  # Happens when character vectors with only 1 value are converted to a factor
  # without specifying the levels parameter in factor()
  is_one_level <- .cols_where(model_data, .is.onelevel, use.names = TRUE)
  is_one_level <- is_one_level[names(is_one_level) %in% unset_factors]
  is_ordered_factor <- is_ordered_factor[!is_one_level]
  unset_factors <- unset_factors[!is_one_level]
  one_level_factors <- names(is_one_level)[is_one_level]

  list("unset_factors" = unset_factors,
       "is_ordered_factor" = is_ordered_factor,
       "one_level_factors" = one_level_factors)

}



#' Warn user if nondefault contrasts are set
#'
#' R automatically assigns specific contrast schemes to ordered and unordered
#' factors as specified in `options('contrasts')` but users are of course
#' free to set these on the factor themselves. But, if they do this outside
#' of a call to `glimpse_contrasts` it's hard and time consuming to check what they
#' set against the different possible common schemes. So, rather than checking
#' all possible combinations, this will only check against the defaults R already
#' uses and alert the user if something else is set.
#'
#' @param contrast_list List of contrasts like that generated by `enlist_contrasts`
#' @param factor_names Names of the factors, also the names of the contrast list
#' @param factor_sizes Number of levels for each factor
#' @param which_ordered Which of the factors are ordered
#'
#' @return Warns if non default contrasts are set
.warn_if_nondefault <- function(contrast_list, factor_names, factor_sizes, which_ordered) {
  indices <- seq_along(factor_sizes)
  factor_sizes <- unname(factor_sizes)
  ord_fx <- str2lang(options('contrasts')[[1]][["ordered"]])
  unord_fx <- str2lang(options('contrasts')[[1]][["unordered"]])

  same_as_default <-
    vapply(indices,
           function(i) {
             contr_mat <- contrast_list[[i]]
             if (which_ordered[i])
               default_mat <- eval(as.call(c(ord_fx, factor_sizes[i])))
             else
               default_mat <- eval(as.call(c(unord_fx, factor_sizes[i])))

             # Check if factor's contrast matrix is same as default settings
             all(round(contr_mat - default_mat, 4) == 0)
           },
           TRUE)

  if (sum(same_as_default) == length(factor_names))
    return(invisible(1))

  unord_str <- crayon::blue(as.character(unord_fx))
  ord_str <- crayon::red(as.character(ord_fx))
  names(which_ordered) <- factor_names
  nondefaults <- vapply(factor_names[!same_as_default],
                        function(x)
                          ifelse(which_ordered[x], crayon::red(x), crayon::blue(x)),
                        "char")
  nondefaults <- paste(" - ", nondefaults, sep = "") |> paste(collapse = "\n")

  warning(glue::glue("Unset factors do not use default {unord_str} or {ord_str}. Glimpse table may be unreliable.
             {nondefaults}"))

}

.get_dropped_trends <- function(params, formulas) {
  vapply(seq_along(params),
         function(i){
           trends <- eval(params[[i]][['drop_trends']],
                          rlang::get_env(formulas[[i]]))
           # trends is NA if nothing was passed
           if (NA %in% trends)
             return(NA_character_)
           paste(trends, collapse = ",")
         },
         "char")
}

.get_scheme_labels <- function(params, formulas) {
  vapply(seq_along(params), \(i) {
    scheme <- deparse1(params[[i]][["code_by"]]) # code_by param is a symbol or NA
    # If it's a matrix call then it's taken to be custom contrasts
    if (grepl("^matrix\\(", scheme))
      return("custom")

    # If it's a function name like contr.poly then use the name of the function
    function_used <- is.function(get(scheme,
                                     rlang::get_env(formulas[[i]])))
    if (function_used)
      return(scheme)

    # Else it's a variable name, which is taken to be custom
    return("custom")
  },
  FUN.VALUE = "char",
  USE.NAMES = FALSE
  )
}

.get_reference_levels <- function(contrast_list, params, formulas) {
  reference_levels <- vapply(seq_along(params),
                             function(i){
                               ref_level <- params[[i]][["reference_level"]]

                               if (!is.symbol(ref_level) && is.na(ref_level))
                                 return(NA_character_)

                               # Will evaluate variables and syntactic literals accordingly
                               as.character(eval(ref_level,
                                                 rlang::get_env(formulas[[i]])))
                             },
                             "char")

  # If a reference level wasn't specified, try to figure it out from the matrix
  for (i in seq_along(reference_levels)) {
    is_contrast_matrix <- diff(dim(contrast_list[[i]])) == -1 # Dont bother with contr.poly - x:y
    if (is.na(reference_levels[i]) &&  is_contrast_matrix) {
      intuition <- .get_reference_level(contrast_list[[i]])
      if (!is.na(intuition))
        reference_levels[[i]] <- intuition
    }
  }
  reference_levels
}


