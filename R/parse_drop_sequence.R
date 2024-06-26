#' Parse comparisons to drop
#'
#' This parses any sequences following a `-` operator and expands them to
#' a numeric vector.
#'
#' @param scheme_argument A scheme argument of the form "a:b", where a and b
#' can correspond to variable names bound in var_envir or numbers.
#' @param var_envir Environment to look up values if a and b are variable names
#'
#' @return A sequence of numbers
#'
#' @importFrom stringr str_match
.parse_drop_sequence <- function(scheme_argument, var_envir) {
  sequence_args <- stringr::str_match(scheme_argument, "(.+):(.+)")[, 2L:3L]
  # Strings like "3L" cannot be converted to numbers, L must be removed first
  sequence_args <- vapply(
    sequence_args,
    function(x) {
      ifelse(grepl("^\\d+L$", x),
        gsub("L$", "", x),
        x
      )
    },
    "char"
  )
  numeric_args <- suppressWarnings(vapply(sequence_args, as.numeric, NA_real_))
  arg_is_non_numeric <- is.na(numeric_args)

  sequence_ends <-
    vapply(
      c(1L, 2L),
      function(i) {
        ifelse(arg_is_non_numeric[[i]],
          get(sequence_args[[i]], envir = var_envir),
          numeric_args[[i]]
        )
      },
      1.0
    )

  seq(sequence_ends[[1L]], sequence_ends[[2L]])
}
