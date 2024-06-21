#' Decompose contrasts into new columns
#'
#' Given a dataframe with factor columns, this function will extract the
#' contrasts from the factor column and place them inside new columns. This is
#' useful for cases where you want to work with the numeric values of the
#' contrasts. For a pedagogical example, you can explicitly show how factor
#' variables are transformed into numeric values. For a practical example,
#' you're typically allowed n-1 contrasts for n levels of a factor. If you don't
#' want to use all of the contrasts, you can extract the ones you want and use
#' them in your model. This is sometimes used with polynomial contrasts when you
#' don't want to use higher order polynomials.
#'
#' An additional usage for this function is to compute the contrasts for
#' interaction terms in a model. In `lm(y ~ A * B)`, where A and B are factors,
#' the expanded form is `lm(y ~ A + B + A:B)` with an equation of \eqn{y =
#' \beta_Ax_A + \beta_Bx_B + \beta_{A:B}x_Ax_B}. The thing to note is that the
#' coefficient for the interaction(s) are multiplied by the product of \eqn{x_A}
#' and \eqn{x_B}. Let's call this product \eqn{x_C}. For example, if one value
#' of \eqn{x_A} is
#' -1/3 and one value of \eqn{x_B} is 2/3, then the product \eqn{x_C} is -2/9.
#' But, if there are 3 levels for \eqn{x_A} and 3 levels for \eqn{x_B}, then we
#' get 4 columns for the fixed effects *and* 4 *more* columns for the
#' interaction terms. It can be a lot of tedious work to precompute the products
#' manually, so we can use this function with `extract_interaction = TRUE` to
#' compute everything at once.
#'
#' @param model_data Dataframe with factor columns
#' @param extract_from Character vector of column names to extract contrasts
#'   from
#' @param extract_to Optional names to give the new columns, must equal number
#'   of extracted components.
#' @param extract_intercept Logical, whether to extract the intercept column
#'   from each contrast. Default `FALSE` since this is typically dropped since
#'   it's all 1s.
#' @param remove_original Logical, whether to remove the original factor column
#'   after decomposing into separate columns. Default `FALSE`.
#' @param extract_interaction Logical, whether the interaction between two
#'   variables should be extracted.
#'
#' @return model_data but with new columns corresponding to the numeric coding
#'   of the given factor's contrasts
#'
#' @export
#' @examples
#'
#' # Decomose contrasts for carb and gear columns into new columns, using
#' # the contrast labels used when setting the contrasts
#' mtcars |>
#'   set_contrasts(
#'     carb ~ scaled_sum_code,
#'     gear ~ contr.sum | c("4-mean", "5-mean")
#'   ) |>
#'   decompose_contrasts(c("carb", "gear"))
#'
#' # Decompose contrasts, but set new labels for gear using extract_to.
#' mtcars |>
#'   set_contrasts(
#'     carb ~ scaled_sum_code,
#'     gear ~ contr.sum
#'   ) |>
#'   decompose_contrasts(c("carb", "gear"),
#'     extract_to = list(gear = c("4-mean", "5-mean")))
#'
#' @importFrom stats contrasts
decompose_contrasts <- function(model_data,
                                extract_from,
                                extract_to = NULL,
                                extract_intercept = FALSE,
                                extract_interaction = FALSE,
                                remove_original = FALSE) {
  # If we want the interactions between the factors, then we'd use * in the
  # formula. If we don't want it, then we can just use +.
  collapse_by <- " + "
  if (extract_interaction)
    collapse_by <- " * "

  # If we want to rename any comparisons, do so in the contrast matrices first
  # so that the model matrix can expand everything for us
  if (!is.null(extract_to)) {
    for (factor_name in names(extract_to)) {
      ncomparisons <- nlevels(model_data[[factor_name]]) - 1L

      if (length(extract_to[[factor_name]]) != ncomparisons)
        stop("Number of names in extract_to should equal number of desired components")

      colnames(contrasts(model_data[[factor_name]])) <-
        extract_to[[factor_name]]
    }
  }

  # Expand all contrasts into individual columns
  components <-
    stats::model.matrix(
      formula(paste("~", paste0(extract_from, collapse = collapse_by))),
      model_data
    )

  # Model matrix gives us an intercept column that we usually do not care about
  if (!extract_intercept)
    components <- components[, -1]

  # Remove the original columns as needed
  if (remove_original) {
    model_data <- model_data[, !colnames(model_data) %in% extract_from]
  }

  # Add back in the components
  cbind(model_data, components)
}
