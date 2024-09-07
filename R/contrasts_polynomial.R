#' Orthogonal Polynomial code
#'
#' @description
#' Wrapper around \link[stats]{contr.poly}. You can also use `polynomial_code`
#' as an alias.
#'
#' @details
#' For n levels of factors where k in 1:n, generate a matrix with n-1
#' comparisons where each comparison looks for a polynomial trend of degree k
#' where each polynomial is independent of the others.
#'
#' @inherit scaled_sum_code params return
#'
#' @aliases polynomial_code
#' @export
#'
#' @examples
#'
#' mydf <- data.frame(
#'   grp = rep(c("a", "b", "c", "d"), each = 2000),
#'   val = c(
#'     rnorm(200, 2, 1),
#'     rnorm(200, 5, 1),
#'     rnorm(200, 7.5, 1),
#'     rnorm(200, 15, 1)
#'   )
#' ) |>
#'   set_contrasts(grp ~ polynomial_code)
#'
#' stats::lm(val ~ grp, data = mydf)
#'
orth_polynomial_code <- function(n) {
  stats::contr.poly(n)
}

#' @rdname orth_polynomial_code
#' @export
polynomial_code <- orth_polynomial_code

#' Raw Polynomial code
#'
#' @description
#' Make raw polynomial contrast, rather than orthogonal ones. Normally you
#' would use orthogonal polynomials, so make sure this is what you want. Using
#' raw polynomials may increase the collinearity in your model, especially with
#' higher numbers of levels.
#'
#' @details
#' For n levels of factors where k in 1:n, generate a matrix with n-1
#' comparisons where each comparison looks for a polynomial trend of degree k,
#' where each polynomial may be correlated with the others. Normally you would
#' use orthogonal polynomials, see \link[stats]{contr.poly} and
#' \link[contrastable]{orth_polynomial_code}
#'
#' @inherit scaled_sum_code params return
#'
#' @export
#'
#' @importFrom stats poly
#'
#' @examples
#' mydf <- data.frame(
#'   grp = rep(c("a", "b", "c", "d"), each = 2000),
#'   val = c(
#'     rnorm(200, 2, 1),
#'     rnorm(200, 5, 1),
#'     rnorm(200, 7.5, 1),
#'     rnorm(200, 15, 1)
#'   )
#' ) |>
#'   set_contrasts(grp ~ raw_polynomial_code)
#'
#' stats::lm(val ~ grp, data = mydf)
raw_polynomial_code <- function(n) {
  contrmat <- poly(seq_len(n), n - 1, raw = TRUE)
  attr(contrmat, "degree") <- NULL
  class(contrmat) <- c("matrix", "array")
  contrmat
}


.is_polynomial_scheme <- function(scheme) {
  checks <- list(
    stats::contr.poly, orth_polynomial_code,
    raw_polynomial_code, polynomial_code,
    "contr.poly", "orth_polynomial_code",
    "raw_polynomial_code", "polynomial_code"
  )

  any(vapply(checks, function(x) identical(x, scheme), FUN.VALUE = TRUE))
}
