% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_parameters.R
\name{.is_reserved_operator}
\alias{.is_reserved_operator}
\title{Check if node is a contrast-manipulation operator}
\usage{
.is_reserved_operator(node, check_sym = NULL)
}
\arguments{
\item{node}{List element}

\item{check_sym}{If NULL (default), check all reserved operators. Otherwise,
a string that will be converted to a symbol.}
}
\value{
\code{TRUE} if \code{node} is identical to a reserved operator, otherwise
\code{FALSE}
}
\description{
Some symbols are reserved for the special syntax implemented by the package.
This function checks if a given node (i.e., a call) is one or more of
\verb{+ - * |}
}
\keyword{internal}
