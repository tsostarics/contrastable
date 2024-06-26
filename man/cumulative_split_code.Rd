% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contrasts_cumulative_split.R
\name{cumulative_split_code}
\alias{cumulative_split_code}
\title{Cumulative split contrasts}
\usage{
cumulative_split_code(n)
}
\arguments{
\item{n}{Integer umber of factor levels to compute contrasts for.}
}
\value{
A contrast matrix with dimensions n rows and (n-1) columns.
}
\description{
Contrast coding scheme that repeatedly dichotomizes the factor levels.
}
\details{
This scheme is similar to Helmert contrasts, but instead of comparing one
level to the accumulated mean of all previous levels, each comparison with
this scheme splits the levels into two groups: those below and including
the current level, and those above the current level. Conceptually this is
similar to continuation ratio logits used in ordinal models. For example,
with a four level factor with levels A, B, C, and D, the comparisons would
be:
\itemize{
\item A vs. BCD
\item AB vs. CD
\item ABC vs. D
}

In other words, each comparison splits the levels into two groups.
Each of these comparisons uses the cumulative mean of all the levels in
each group. The intercept is the grand mean.
}
\examples{


set.seed(111)
df <- data.frame(
  grp = rep(c("a", "b", "c", "d"), each = 2000),
  val = c(
    rnorm(200, 2, 1),
    rnorm(200, 5, 1),
    rnorm(200, 7.5, 1),
    rnorm(200, 15, 1)
  )
) |>
  set_contrasts(grp ~ cumulative_split_code |
    c("a-rest", "ab-rest", "abc-rest"))

lm(val ~ grp, data = df)

}
