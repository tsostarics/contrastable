% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_contrast.R
\name{use_contrasts.hypr}
\alias{use_contrasts.hypr}
\title{hypr method for use_contrasts}
\usage{
\method{use_contrasts}{hypr}(
  factor_col,
  code_by = NA,
  reference_level = NA,
  set_intercept = NA,
  drop_trends = NA,
  labels = NULL,
  as_is = FALSE,
  ...
)
}
\arguments{
\item{factor_col}{A factor vector, eg from df$factorVarName}

\item{code_by}{A hypr object created with \code{hypr::hypr()}}

\item{reference_level}{Not used}

\item{set_intercept}{Not used}

\item{drop_trends}{Not used}

\item{labels}{A vector of labels to apply to the matrix column names, default}

\item{as_is}{Logical, default FALSE, whether to leave the resulting matrix}

\item{...}{Additional arguments, not used}
}
\value{
Contrast matrix specified by the hypr object
}
\description{
hypr method for use_contrasts
}
\examples{
\dontshow{if (rlang::is_installed("hypr")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
hypr_obj <- hypr::hypr(a ~ b, c ~ b) # centered pairwise comparisons to b

use_contrasts(factor(c('a', 'b', 'c')), hypr_obj)
\dontshow{\}) # examplesIf}
}
