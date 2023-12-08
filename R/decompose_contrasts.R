#' Decompose contrasts into new columns
#'
#' Given a dataframe with factor columns, this function will extract the contrasts
#' from the factor column and place them inside new columns. This is useful for
#' cases where you want to work with the numeric values of the contrasts. For
#' a pedagogical example, you can explicitly show how factor variables are
#' transformed into numeric values. For a practical example, you're typically
#' allowed n-1 contrasts for n levels of a factor. If you don't want to use all
#' of the contrasts, you can extract the ones you want and use them in your
#' model. This is sometimes used with polynomial contrasts when you don't want
#' to use higher order polynomials.
#'
#' @param model_data Dataframe with factor columns
#' @param extract_from Character vector of column names to extract contrasts from
#' @param extract_to Optional names to give the new columns, must equal number of
#' extracted components.
#' @param which_contrasts Optional named list of which columns to extract.
#' The names of the list should match some or all of the values in `extract_from`
#' if used. The values should be integer vectors corresponding to the
#' column indices to extract.
#' @param extract_intercept Logical, whether to extract the intercept column from
#' each contrast. Default `FALSE` since this is typically dropped since it's
#' all 1s.
#' @param remove_original Logical, whether to remove the original factor column
#' after decomposing into separate columns. Default `FALSE`.
#' @param retain_factor_name Either a logical value or a character vector of
#' which factor names should have their original variable name prepended to the
#' extracted components. Default `TRUE` will prepend the factor name to all
#' extracted components. `FALSE` will not prepend the factor name to any
#' extracted components. A character vector will prepend the factor name to
#' only the factors specified in the vector. Note that `FALSE` will only affect
#' columns that are extracted with `extract_to` set. If `extract_to` is not set,
#' then the factor name will still be prepended to the extracted components, as
#' this is the default behavior of `model.matrix()`.
#'
#' @return model_data but with new columns corresponding to the numeric coding
#' of the given factor's contrasts
#'
#' @export
#' @examples
#'
#' # Decomose contrasts for carb and gear columns into new columns, using
#' # the contrast labels used when setting the contrasts
#' mtcars |>
#' set_contrasts(carb ~ scaled_sum_code,
#'               gear ~ contr.sum | c("4-mean", "5-mean")) |>
#'   decompose_contrasts(c('carb', 'gear'))
#'
#' # Decompose contrasts, but set new labels for gear using extract_to,
#' # and don't prepend 'gear' to these new columns. Note that this isn't
#' # recommended in practice since you'll need to escape the column names with
#' # backticks when using them.
#' mtcars |>
#'   set_contrasts(mtcars,
#'                 carb ~ scaled_sum_code,
#'                 gear ~ contr.sum) |>
#'   decompose_contrasts(c('carb', 'gear'),
#'                       extract_to = list(gear = c("4-mean", "5-mean")),
#'                       retain_factor_name = FALSE)
#'
#'
decompose_contrasts <- function(model_data,
                                extract_from,
                                extract_to = NULL,
                                which_contrasts = NULL,
                                extract_intercept = FALSE,
                                remove_original = FALSE,
                                retain_factor_name = TRUE) {

  if (length(retain_factor_name) == 1L) {
    if (retain_factor_name) {
      retain_factor_name <- extract_from
    } else{
      retain_factor_name <- character(0)
    }
  }
  all_components <-
    lapply(seq_along(extract_from), \(i) {
      current_factor <- extract_from[i]

      # Get the indices of which contrasts to extract
      extract_these <- seq_len(nlevels(model_data[[current_factor]]))
      if (!is.null(which_contrasts)) {
        # Only use values in extract_from if they're specified for this factor
        if (extract_from %in% names(which_contrasts))
          extract_these <- which_contrasts[[current_factor]]
      }

      if (!extract_intercept)
        extract_these <- extract_these[-1]

      # Get the numeric values of the contrasts
      components <- stats::model.matrix(formula(paste("~",current_factor)), model_data)[,extract_these]

      # Rename the columns if extract_to is set for the current factor
      if (!is.null(extract_to)) {
        if (current_factor %in% names(extract_to)){
          new_column_names <- extract_to[[current_factor]]

          # If retain_factor_name is TRUE, prepend the factor name to the new column names
          if (current_factor %in% retain_factor_name)
            new_column_names <- paste0(current_factor, new_column_names)


          if (length(new_column_names) != length(extract_these))
            stop("Number of names in extract.to should equal number of desired components")
          colnames(components) <- new_column_names
        }
      }

      components
    })

  # Add the new columns to the model data
  model_data <- cbind(model_data, do.call(cbind, args = all_components))

  if (remove_original)
    model_data <- model_data[,colnames(model_data) != extract_from]

  model_data
}
