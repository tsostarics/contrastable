#' Glimpse contrasts in dataframe
#'
#' Uses the same syntax as \link[contrastable]{enlist_contrasts} and
#' \link[contrastable]{set_contrasts}. Returns a summary table of the contrasts
#' you've set. If you set `return.list=TRUE` then you can access a list of
#' contrasts in the second element of the resulting list. The glimpse dataframe
#' is the first element. `FALSE` will return just the glimpse data frame.
#'
#' @details
#' Generally, `glimpse_contrasts` will give warnings about mismatches between
#' the specified contrasts and what's actually set on the factors in a
#' dataframe. The warnings will typically tell you how to resolve these
#' mismatches. See the `contrasts` and `warnings` vignettes for more
#' information.
#'
#'
#' @param model_data Data to be passed to a model fitting function
#' @param ... Series of formulas
#' @param return_list Logical, defaults to FALSE, whether the output of
#'   enlist_contrasts should be returned
#' @param show_all_factors Logical, defaults to TRUE, whether the factors not
#'   explicitly set with formulas should be included
#' @param add_namespace Logical, defaults to FALSE, whether to append the
#'   namespace of the contrast scheme to the scheme name
#' @param show_one_level_factors Logical, should factors with only one level be
#'   included in the output? Default is FALSE to omit
#' @param minimal Logical, default TRUE, whether to omit the orthogonal,
#'   centered, dropped_trends, and explicitly_set columns from the output table
#' @param verbose Logical, defaults to TRUE, whether messages should be printed
#'
#' @inherit enlist_contrasts details
#' @return A dataframe if return.list is FALSE, a list with a dataframe and list
#'   of named contrasts if TRUE.
#' @export
#'
#' @examples
#'
#' my_contrasts <- list(cyl ~ sum_code, carb ~ helmert_code)
#' my_data <- set_contrasts(mtcars, my_contrasts)
#' my_data$gear <- factor(my_data$gear) # Make gear a factor manually
#'
#' # View information about contrasts; gear will use default for unordered
#' glimpse_contrasts(my_data, my_contrasts)
#'
#' \dontrun{
#' # Not passing the contrasts to `glimpse_contrasts` will show various warnings
#' glimpse_contrasts(my_data)
#'
#' # A warning is thrown if the contrasts don't match what's on the dataset,
#' # my_data$gear uses contr.treatment, not sum_code
#' glimpse_contrasts(my_data, my_contrasts, gear ~ sum_code)
#' }
glimpse_contrasts <- function(model_data,
                              ...,
                              return_list = FALSE,
                              show_all_factors = TRUE,
                              add_namespace = FALSE,
                              show_one_level_factors = FALSE,
                              minimal = TRUE,
                              verbose = getOption("contrastable.verbose")) {
  formulas <- purrr::list_flatten(rlang::dots_list(...))

  # Get symbols passed to ... and model_data for .warn_if_mismatched_contrasts
  # ensyms(...) when ... is not a symbol will throw an error, in which case we
  # can just use "..." as in the function definition
  dots_names <- tryCatch(as.character(rlang::ensyms(...)), error = \(e) "...")
  model_data_name <- as.character(rlang::ensym(model_data))

  # If no formulas are provided but we want to glimpse all factors, use
  # glimpse_default_factors and return early. If all.factors is FALSE, then
  # we're going to get an error from enlist_contrasts anyways
  if (identical(formulas, list()) && show_all_factors) {
    glimpse <- .glimpse_default_factors(model_data,
                                        set_factors = c(),
                                        show_one_level_factors,
                                        verbose
    )

    if (add_namespace) {
      glimpse$scheme <- .add_namespace(glimpse$scheme)
    }

    # The default factors don't need to be specified in the contrast list,
    # they'll just use their respective defaults by the model fitting function
    if (return_list) {
      return(list("glimpse" = glimpse, "contrasts" = list()))
    }

    return(glimpse)
  }

  # We do need to compute the contrast matrices so we can get information
  # about orthogonality, centering, etc.
  contrast_list <- enlist_contrasts(model_data, ..., "verbose" = verbose)

  # The formulas need to be expanded to extract the parameters correctly,
  # but we still need the unexpanded formulas provided by the user so we
  # can give a more parsimonious warning if there are mismatches
  unexpanded_formulas <- formulas
  formulas <- .expand_formulas(unexpanded_formulas, model_data)
  params <- lapply(formulas, .make_parameters)


  # Need to check whether the contrasts on the data frame are the same as
  # what was provided by the formulas
  .warn_if_mismatched_contrasts(model_data,
                                contrast_list,
                                model_data_name,
                                dots_names,
                                unexpanded_formulas)

  # Ignore explicitly set factors that actually only have one level
  is_onelevel_factor <-
    vapply(
      params,
      function(x) nlevels(model_data[[x[["factor_col"]]]]) == 1L,
      logical(1)
    )

  formulas <- formulas[!is_onelevel_factor]
  params <- params[!is_onelevel_factor]

  # Extract various information
  set_factors <- names(contrast_list)
  factor_sizes <- vapply(contrast_list, nrow, 1L, USE.NAMES = FALSE)
  level_names <- unname(lapply(contrast_list, rownames))
  scheme_labels <- .get_scheme_labels(params, formulas)
  reference_levels <- .get_reference_levels(contrast_list)
  intercept_interpretations <- vapply(contrast_list,
                                      interpret_intercept,
                                      character(1),
                                      USE.NAMES = FALSE
  )

  orthogonal_contrasts <- is_orthogonal(contrast_list)
  centered_contrasts <- is_centered(contrast_list)


  # Double check that dropped trends are only included for polynomial contrasts
  dropped_trends <- .get_dropped_trends(params, formulas)
  which_are_polynomials <- vapply(scheme_labels, .is_polynomial_scheme, TRUE)
  dropped_trends[!which_are_polynomials] <- NA

  glimpse <- data.frame(
    "factor" = set_factors,
    "n" = factor_sizes,
    "level_names" = I(level_names),
    "scheme" = scheme_labels,
    "reference" = reference_levels,
    "intercept" = intercept_interpretations,
    "orthogonal" = orthogonal_contrasts,
    "centered" = centered_contrasts,
    "dropped_trends" = dropped_trends,
    "explicitly_set" = TRUE
  )



  if (show_all_factors) {
    glimpse <- rbind(
      glimpse,
      .glimpse_default_factors(
        model_data,
        set_factors,
        show_one_level_factors,
        verbose
      )
    )
  }

  if (add_namespace) {
    glimpse[["scheme"]] <- .add_namespace(glimpse[["scheme"]])
  }

  # TODO: Rewrite so that the additional columns are only computed if FALSE
  if (minimal) {
    glimpse <- glimpse[, c(
      "factor",
      "n",
      "level_names",
      "scheme",
      "reference",
      "intercept"
    )]
  }

  # The default factors don't need to be specified in the contrast list,
  # they'll just use their respective defaults by the model fitting function
  if (return_list) {
    return(list("glimpse" = glimpse, "contrasts" = contrast_list))
  }


  glimpse
}

#' Lookup namespace of contrast scheme function
#'
#' Given the name of a contrast scheme (ie the function name that creates
#' the contrast matrix), lookup which namespace it belongs to and add it
#' to the string. Used to report which packages are used in the glimpse table,
#' e.g., stats, contrastable, bayesTestR
#'
#' @param scheme_names Character vector
#'
#' @return character vector of updated function names with namespaces
.add_namespace <- function(scheme_names) {
  vapply(scheme_names,
         \(n) {
           namespace <- gsub("package:", "", utils::find(n), perl = TRUE)
           separator <- "::"
           if (identical(namespace, character(0))) {
             namespace <- ""
             separator <- ""
           }

           paste0(namespace, separator, n)
         },
         character(1),
         USE.NAMES = FALSE
  )
}


#' Glimpse default factors
#'
#' Given a dataframe with some factor columns and a character vector of which
#' columns you've already set yourself, look at all the other factor columns and
#' get a glimpse at how they're treated by the defaults specified in
#' `options('contrasts')`. Reference level is assumed to be the first level for
#' unordered factors and nonexistent for ordered factors.
#'
#' @param model_data Dataframe
#' @param set_factors Explicitly set columns to ignore
#' @param show_one_level_factors Should factor columns with only 1 level be
#'   included? Default FALSE
#' @param verbose Defaults to FALSE, should messages and warnings be printed?
#'
#' @return A table with information about the contrasts for all remaining factor
#'   columns
.glimpse_default_factors <- function(model_data,
                                     set_factors = NULL,
                                     show_one_level_factors = FALSE,
                                     verbose = TRUE) {
  fct_info <- .get_factor_info(model_data, set_factors, verbose)
  unset_factors <- fct_info[["unset_factors"]]
  is_ordered_factor <- fct_info[["is_ordered_factor"]]

  # Some factors may not be explicitly set, but if they only have one level
  # then we need to ignore them
  unset_factors <-
    unset_factors[!unset_factors %in% fct_info[["one_level_factors"]]]

  # Extract contrasts from the factor columns
  default_contrasts <- lapply(unset_factors,
                              function(x) stats::contrasts(model_data[[x]]))
  names(default_contrasts) <- unset_factors

  # Extract the number of levels levels for each factor
  factor_sizes <- vapply(unset_factors,
                         function(x) nlevels(model_data[[x]]),
                         FUN.VALUE = integer(1)
  )

  # Extract the names of the levels for each factor
  level_names <- lapply(unset_factors, function(x) levels(model_data[[x]]))

  # Since the unset factors are using the default contrasts, look up what the
  # defaults are in the options
  schemes_to_use <- ifelse(is_ordered_factor[unset_factors],
                           options("contrasts")[[1]]["ordered"],
                           options("contrasts")[[1]]["unordered"]
  )

  # For most users, the reference level for unordered factors will use
  # contr.treatment by default, and so the reference level will be the first
  # level. But, if they set their default contrast to something like contr.sum
  # or contr.SAS, then the reference level will be the last level. So, we need
  # to lookup the reference level manually for each contrast
  reference_levels <- .get_reference_levels(default_contrasts)

  # Interpret each contrast matrix, then check if they're orthogonal or centered
  intercept_interpretations <- vapply(default_contrasts,
                                      interpret_intercept,
                                      character(1),
                                      USE.NAMES = FALSE
  )
  orthogonal_contrasts <- is_orthogonal(default_contrasts)
  centered_contrasts <- is_centered(default_contrasts)

  # Trends are never dropped w/ R's defaults
  dropped_trends <- rep(NA, length(unset_factors))

  # Check if any factors have not been specified in the call but are
  # different from the defaults. Reporting the default scheme is misleading,
  # so we will warn the user and change the scheme to ???
  schemes_to_use <-
    .warn_if_nondefault(default_contrasts,
                        unset_factors,
                        factor_sizes,
                        is_ordered_factor,
                        schemes_to_use)

  explicitly_set <- if (length(unset_factors) == 0) logical(0) else FALSE

  glimpse <- data.frame(
    "factor" = unset_factors,
    "n" = factor_sizes,
    "level_names" = I(level_names),
    "scheme" = schemes_to_use,
    "reference" = reference_levels,
    "intercept" = intercept_interpretations,
    "orthogonal" = orthogonal_contrasts,
    "centered" = centered_contrasts,
    "dropped_trends" = dropped_trends,
    "explicitly_set" = explicitly_set
  )

  # If we want to show the one-level factors, add those rows in
  if (show_one_level_factors) {
    glimpse <- rbind(
      glimpse,
      .make_placeholder_glimpse(
        model_data,
        fct_info[["one_level_factors"]]
      )
    )
  }


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
#' @return A data.frame with limited information about one level factors
.make_placeholder_glimpse <- function(model_data, one_level_factors) {
  # Will be a list of 1-length character vectors to match list column type
  level_names <- lapply(
    one_level_factors,
    function(x) levels(model_data[[x]])
  )

  data.frame(
    "factor" = one_level_factors,
    "n" = 1L,
    "level_names" = I(level_names),
    "scheme" = NA_character_,
    "reference" = NA_character_,
    "intercept" = NA_character_,
    "orthogonal" = NA,
    "centered" = NA,
    "dropped_trends" = NA_character_,
    "explicitly_set" = NA
  )
}

.get_factor_info <- function(model_data, set_factors = NULL, verbose = TRUE) {
  # Look up all the factor columns inthe dataframe, then check to see if
  # which ones have been explicitly set given `set_factors`
  all_factors <- .cols_where(model_data, is.factor, return_names = TRUE)
  unset_factors <- all_factors[!all_factors %in% set_factors]
  is_ordered_factor <-
    unset_factors %in% .cols_where(model_data, is.ordered, return_names = TRUE)
  names(is_ordered_factor) <- unset_factors

  # Filter out any factors that only have 1 level to avoid undefined contrasts
  # Happens when character vectors with only 1 value are converted to a factor
  # without specifying the levels parameter in factor()
  is_one_level <- .cols_where(model_data, .is.onelevel, use_names = TRUE)
  is_one_level <- is_one_level[names(is_one_level) %in% unset_factors]
  is_ordered_factor <- is_ordered_factor[!is_one_level]
  unset_factors <- unset_factors[!is_one_level]
  one_level_factors <- names(is_one_level)[is_one_level]

  list(
    "unset_factors" = unset_factors,
    "is_ordered_factor" = is_ordered_factor,
    "one_level_factors" = one_level_factors
  )
}



#' Warn user if nondefault contrasts are set
#'
#' R automatically assigns specific contrast schemes to ordered and unordered
#' factors as specified in `options('contrasts')` but users are of course free
#' to set these on the factor themselves. But, if they do this outside of a call
#' to `glimpse_contrasts` it's hard and time consuming to check what they set
#' against the different possible common schemes. So, rather than checking all
#' possible combinations, this will only check against the defaults R already
#' uses and alert the user if something else is set.
#'
#' @param contrast_list List of contrasts like that generated by
#'   `enlist_contrasts`
#' @param factor_names Names of the factors, also the names of the contrast list
#' @param factor_sizes Number of levels for each factor
#' @param which_ordered Which of the factors are ordered
#' @param schemes_to_use Character vector of schemes, if any don't match the
#' default for a factor, it will be replaced wtih ??? in th eoutput
#'
#' @return Warns if non default contrasts are set, returns schemes_to_use with
#' modifications if necessary
.warn_if_nondefault <- function(contrast_list,
                                factor_names,
                                factor_sizes,
                                which_ordered,
                                schemes_to_use) {
  indices <- seq_along(factor_sizes)
  factor_sizes <- unname(factor_sizes)
  ord_fx <- str2lang(options("contrasts")[[1]][["ordered"]])
  unord_fx <- str2lang(options("contrasts")[[1]][["unordered"]])

  same_as_default <-
    vapply(
      indices,
      function(i) {
        contr_mat <- contrast_list[[i]]
        if (which_ordered[i]) {
          default_mat <- eval(as.call(c(ord_fx, factor_sizes[i])))
        } else {
          default_mat <- eval(as.call(c(unord_fx, factor_sizes[i])))
        }

        # Check if factor's contrast matrix is same as default settings
        all(round(contr_mat - default_mat, 4) == 0)
      },
      TRUE
    )

  if (sum(same_as_default) == length(factor_names)) {
    return(schemes_to_use)
  }

  unord_str <- crayon::blue(as.character(unord_fx))
  ord_str <- crayon::red(as.character(ord_fx))
  names(which_ordered) <- factor_names
  nondefaults <- vapply(
    factor_names[!same_as_default],
    function(x) {
      if (which_ordered[x]) {
        return(crayon::red(x))
      }

      crayon::blue(x)
    },
    character(1)
  )
  nondefaults <- paste(paste(" - ", nondefaults, sep = ""), collapse = "\n")

  schemes_to_use[!same_as_default] <- "???"

  warning(glue::glue("Unset factors do not use default {unord_str} or {ord_str}. Glimpse table may be unreliable.\n{nondefaults}")) # nolint

  schemes_to_use
}

.get_dropped_trends <- function(params, formulas) {
  vapply(
    seq_along(params),
    function(i) {
      trends <- eval(
        params[[i]][["drop_trends"]],
        rlang::get_env(formulas[[i]])
      )
      # trends is NA if nothing was passed
      if (NA %in% trends) {
        return(NA_character_)
      }
      paste(trends, collapse = ",")
    },
    "char"
  )
}

.get_scheme_labels <- function(params, formulas) {
  vapply(seq_along(params), \(i) {
    scheme <- deparse1(params[[i]][["code_by"]]) # code_by param is a sym or NA
    # If it's a matrix call then it's taken to be custom contrasts
    if (grepl("^matrix\\(", scheme)) {
      return("custom")
    }

    # If it's a function name like contr.poly then use the name of the function
    is_namespaced <- grepl("::", scheme)

    # If the user passes the namespace with the function then get(scheme, env)
    # won't work if the namespace isn't loaded in the current R session.
    # Instead we'll just always resolve the namespace when it's specified, then
    # double check to make sure it's a function
    if (is_namespaced) {
      # third : in case user tries an unexported function
      scheme_parts <- strsplit(scheme, ":::?")[[1]]
      scheme_fx <-
        utils::getFromNamespace(scheme_parts[2],
                                scheme_parts[1],
                                envir = rlang::get_env(formulas[[i]]))
      if (is.function(scheme_fx))
        return(scheme)
    } else {
      if (is.function(get(scheme, rlang::get_env(formulas[[i]]))))
        return(scheme)
    }

    # Else it's a variable name, which is taken to be custom
    return("custom")
  },
  FUN.VALUE = "char",
  USE.NAMES = FALSE
  )
}


.get_reference_level <- function(cmat) {
  if (is.null(cmat)) {
    stop("Contrast matrix is NULL, did you try to index a list of contrasts by a name that didn't exist in names(list)?") # nolint
  }

  if (diff(dim(cmat)) != -1L) {
    stop(paste0("Contrast matrix has invalid size: ",
                paste0(dim(cmat),
                       collapse = ", ")))
  }

  # Compute the inverse matrix of the contrast matrix. The reference level is
  # the index of the column that has the same positive value in each row.
  inverse_matrix <- solve(.contrasts_to_hypotheses(cmat))
  find_same_col(inverse_matrix)
}


#' Get reference levels from a (possibly set) list of contrasts
#'
#' Given a list of contrast matrices, if the contrast matrices were explicitly
#' generated via formulas and they have already had their parameters parsed, use
#' the parameters to look up the reference level for each contrast matrix. If
#' the parameters have not been computed (usually because the contrasts are
#' using the defaults) then look up the reference level manually.
#'
#'
#' @param contrast_list List of contrasts, does not need to be named
#' @param params Optional list of parameters from `.make_parameters()`, if NULL,
#'   then the reference level is determined from the contrast matrix directly.
#' @param formulas Optional list of formulas, needed if `params` are passed.
#'   Used to get the correct environment for evaluating expressions in `params`.
#'   If NULL, then the reference level is determiend from the contrast matrix
#'   directly.
#'
#' @return Character vector of reference levels. If a contrast matrix is not
#'   specified for row names, the character value will denote the integer index
#'   of the row for the reference level (usually 1).
.get_reference_levels <- function(contrast_list,
                                  params = NULL,
                                  formulas = NULL) {
  if (!is.null(params) && !is.null(formulas)) {
    reference_levels <- .get_from_params("reference_level", params, formulas)
  } else {
    reference_levels <- rep(NA_character_, length(contrast_list))
    # If a reference level wasn't specified, try to figure it out from the
    # matrix
    for (i in seq_along(reference_levels)) {
      # Dont bother with contr.poly - x:y
      is_contrast_matrix <- diff(dim(contrast_list[[i]])) == -1
      if (is_contrast_matrix) {
        reference_index <- .get_reference_level(contrast_list[[i]])
        if (!is.na(reference_index)) {
          reference_levels[[i]] <- rownames(contrast_list[[i]])[reference_index]
        }
      }
    }
  }
  reference_levels
}

.get_from_params <- function(what, params, formulas) {
  stopifnot(what %in% names(params[[1]]))

  vapply(
    seq_along(params),
    function(i) {
      ref_level <- params[[i]][[what]]
      if (!is.symbol(ref_level) && is.na(ref_level)) {
        return(NA_character_)
      }

      # Will evaluate variables and syntactic literals accordingly
      as.character(eval(ref_level, rlang::get_env(formulas[[i]])))
    },
    character(1)
  )
}

.warn_if_mismatched_contrasts <- function(model_data,
                                          contrast_list,
                                          model_data_name,
                                          dots_names,
                                          formulas) {
  # set_contrasts will coerce non-factors but glimpse_contrasts will not
  var_is_factor <-
    vapply(names(contrast_list),
           \(varname) is.factor(model_data[[varname]]),
           logical(1), USE.NAMES = TRUE)

  # If there are no issues we won't give the user a reminder of how to use
  # set_contrasts with the correct arguments
  remind_about_set_contrasts <- FALSE

  # We'll build up each warning message separately then concatenate any needed
  # ones at the end so we only need to send one warning
  WARN_non_factor <- NULL
  WARN_mismatched_contrasts <- NULL
  WARN_mismatched_labels <- NULL
  WARN_reminder <- NULL

  if (any(!var_is_factor)) {
    non_factor_vars <- paste0(" - ",
                              names(contrast_list)[!var_is_factor],
                              collapse = "\n")
    WARN_non_factor <- glue::glue("These vars in `{model_data_name}` are not factors:\n{non_factor_vars}") # nolint
    remind_about_set_contrasts <- TRUE
  }

  # TODO: This can be refactored to only iterate once over everything
  doesnt_match_set_contrasts <-
    vapply(names(contrast_list[var_is_factor]),
           \(varname) {
             !identical(unname(contrasts(model_data[[varname]])),
                        unname(contrast_list[[varname]]))
           }, logical(1), USE.NAMES = TRUE)

  labels_dont_match <-
    vapply(names(contrast_list[var_is_factor]),
           \(varname) {
             !identical(colnames(contrasts(model_data[[varname]])),
                        colnames(contrast_list[[varname]]))
           }, logical(1), USE.NAMES = TRUE)

  if (any(doesnt_match_set_contrasts) || any(labels_dont_match)) {
    # Get which variables have mismatching matrices XOR mismatching labels
    # (practically speaking mismatching matrices will entail mismatching labels)
    mismatched_varnames <- names(which(doesnt_match_set_contrasts))
    mismatched_labels <-
      names(labels_dont_match)[labels_dont_match & !doesnt_match_set_contrasts]

    remind_about_set_contrasts <- TRUE

    if (!identical(mismatched_varnames, character(0))) {
      mismatched_varnames <- paste0(" - ", mismatched_varnames, collapse = "\n")
      WARN_mismatched_contrasts <-
        glue::glue("Contrasts for factors in `{model_data_name}` don't match matrices in formulas:\n{mismatched_varnames}") # nolint
    }

    if (!identical(mismatched_labels, character(0))) {
      # For each variable with only mismatching labels, look up what the
      # expected (via formulas) and actual comparison labels are
      warning_lines <-
        vapply(mismatched_labels,
               \(varname) {
                 expected_labels <-
                   paste0(colnames(contrast_list[[varname]]),
                          collapse = ", ")
                 set_labels <-
                   paste0(colnames(contrasts(model_data[[varname]])),
                          collapse = ", ")

                 paste0(" - ",
                        varname,
                        "\t(expected `",
                        expected_labels,
                        "` but found `",
                        set_labels,
                        "`)")
               }, character(1))

      mismatched_labels <- paste0(warning_lines, collapse = "\n")
      WARN_mismatched_labels <-
        glue::glue("Comparison labels for contrasts in `{model_data_name}` don't match:\n{mismatched_labels}") # nolint
    }

  }

  if (remind_about_set_contrasts) {
    # If the user typed out formulas manually, then we need to expand the ...
    if (dots_names[1L] == "...") {
      # number of chars in: nchar(" <- set_contrasts(")
      padding <- strrep(" ", nchar(model_data_name) + 18L)

      # need an extra \n on the first formula
      initial_newlines <- c("\n", rep("", length(formulas) - 1L))
      dots_names <-
        paste0(initial_newlines,
               padding,
               # as.character(formula) gives a character vector of length 3,
               # format.formula() will give a single string
               vapply(formulas, format, character(1)),
               collapse = ",\n")
    }
    WARN_reminder <- glue::glue("To fix, be sure to run:\n{model_data_name} <- set_contrasts({model_data_name}, {dots_names})") # nolint
  }

  # Any NULLs won't be included; if all are none the length will be 0
  warning_messages <- c(WARN_non_factor,
                        WARN_mismatched_contrasts,
                        WARN_mismatched_labels,
                        WARN_reminder)

  if (length(warning_messages) > 0) {
    warning(paste0(warning_messages, collapse = "\n"), call. = FALSE)
  }

}
