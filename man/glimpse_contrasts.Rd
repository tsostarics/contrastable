% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glimpse_contrasts.R
\name{glimpse_contrasts}
\alias{glimpse_contrasts}
\title{Glimpse contrasts in dataframe}
\usage{
glimpse_contrasts(
  model_data,
  ...,
  return.list = FALSE,
  verbose = TRUE,
  all.factors = TRUE,
  clean.schemes = FALSE
)
}
\arguments{
\item{model_data}{Data to be passed to a model fitting function}

\item{...}{Series of formulas}

\item{return.list}{Logical, defaults to FALSE, whether the output of enlist_contrasts should be
returned}

\item{verbose}{Logical, defaults to FALSE, whether messages should be printed}

\item{all.factors}{Logical, defaults to TRUE, whether the factors not
explicitly set with formulas should be included}

\item{clean.schemes}{Logical, defaults to FALSE, whether the contrast schemes
should remove "contr." and "_code" from the names ("sum" and not "contr.sum"
or "sum_code")}
}
\value{
A dataframe is return.list is FALSE, a list with a dataframe and list
of named contrasts if TRUE.
}
\description{
Uses the same syntax as `enlist_contrasts` and `set_contrasts`. Returns
a summary table of the contrasts you've set. If you set `return.list` to TRUE
then you can access a list of contrasts in the second element of the resulting
list. The glimpse dataframe is the first element. FALSE will return just
the glimpse data frame.
}