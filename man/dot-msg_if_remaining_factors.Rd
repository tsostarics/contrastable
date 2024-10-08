% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor_coercion_messages.R
\name{.msg_if_remaining_factors}
\alias{.msg_if_remaining_factors}
\title{Alert user if there are more factors}
\usage{
.msg_if_remaining_factors(model_data, specified_vars)
}
\arguments{
\item{model_data}{Model data}

\item{specified_vars}{Variables specified by the user from formulas}
}
\value{
Nothing, messages the user if needed.
}
\description{
messages the user if the user has factor columns in their model data frame
that weren't specified along with any factors they DID set contrasts for.
}
\keyword{internal}
