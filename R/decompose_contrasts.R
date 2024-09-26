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
#' @details
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
#' @param extract A one-sided formula denoting the factors to extract. Note this
#' should ideally be what you would pass to your model fitting function, sans
#' any non-factors.
#' @param remove_intercept Logical, whether to remove the column corresponding
#' to the intercept. Default `TRUE`  since it's always just a column of 1s
#' @param remove_original Logical, whether to remove the original columns in
#' the data frame after decomposing into separate columns. Default `FALSE`.
#'
#' @return `model_data` but with new columns corresponding to the numeric coding
#'   of the given factor's contrasts
#'
#' @export
#' @seealso [set_contrasts()]
#' @examples
#'
#' # Decompose contrasts for carb and gear columns into new columns, using
#' # the contrast labels used when setting the contrasts
#' mtcars |>
#'   set_contrasts(
#'     carb ~ scaled_sum_code,
#'     gear ~ contr.sum | c("4-mean", "5-mean")
#'   ) |>
#'   decompose_contrasts(~ carb + gear) |>
#'   str()
#'
#' # Decompose an interaction term between the two factors
#' mtcars |>
#'   set_contrasts(
#'     carb ~ scaled_sum_code,
#'     gear ~ contr.sum | c("4-mean", "5-mean")
#'   ) |>
#'   decompose_contrasts(~ carb * gear) |>
#'   str()
#'
#' @importFrom stats contrasts
decompose_contrasts <- function(model_data,
                                extract,
                                remove_intercept = TRUE,
                                remove_original = FALSE) {
  # Expand all contrasts into individual columns
  components <- stats::model.matrix(extract, model_data)

  # Model matrix gives us an intercept column that we usually do not care about
  if (remove_intercept)
    components <- components[, -1L]

  # Remove the original columns as needed
  if (remove_original) {
    original_varnames <-
      as.character(attr(stats::terms(extract), "variables")[-1L])
    model_data <- model_data[, !colnames(model_data) %in% original_varnames]
  }

  # Add back in the components
  cbind(model_data, components)
}
