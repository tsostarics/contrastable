% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enlist_contrasts.R
\name{enlist_contrasts}
\alias{enlist_contrasts}
\title{List of contrast matrices}
\usage{
enlist_contrasts(model_data, ..., verbose = TRUE)
}
\arguments{
\item{model_data}{Data frame you intend on passing to your model}

\item{...}{A series of 2 sided formulas with factor name on the left hand
side and desired contrast scheme on the right hand side. The reference
level can be set with \code{+}, the intercept can be overwritten with \code{*},
comparison labels can be set using \code{|}, and trends for polynomial coding
can be removed using \code{-}.}

\item{verbose}{Logical, defaults to FALSE, whether messages should be printed}
}
\value{
List of named contrast matrices
}
\description{
Returns a named list of contrast matrices to use with modeling
functions directly. See \link[contrastable]{set_contrasts} for a function
to set contrasts directly to the dataframe. See details for syntax
information
}
\details{
\link[contrastable]{enlist_contrasts}, \link[contrastable]{set_contrasts},
and \link[contrastable]{glimpse_contrasts} use special syntax to set
contrasts for multiple factors. The syntax consists of two-sided formulas
with the desired factor column on the left hand side and the contrast
specification on the right hand side. For example, \code{varname ~ scaled_sum_code}. Many contrasts support additional kinds of contrast
manipulations using overloaded operators:
\itemize{
\item \code{+ X}: Set the reference level to the level named X. Only supported for
schemes that have a singular reference level such as
\link[contrastable]{sum_code}, \link[contrastable]{scaled_sum_code},
\link[contrastable]{treatment_code}, \link[stats]{contr.treatment},
\link[stats]{contr.sum}, \link[stats]{contr.SAS}. Ignored for schemes like
\link[contrastable]{helmert_code}.
\item \verb{* X}: Overwrite the intercept to the mean of the level named X
\item \code{- A:B}: For polynomial coding schemes only, drop comparisons A through B.
\item \verb{| c(...)}: Change the comparison labels for the contrast matrix to the
character vector \code{c(...)} of length \code{n-1}. These labels will appear in the
output/summary of a statistical model. Note that for \link[brms]{brm},
instances of \code{-} (a minus sign) are replaced with \code{M}.
}

Typically model functions like lm will have a contrasts argument where you
can set the contrasts at model run time, rather than having to manually
change the contrasts on the underlying factor columns in your data. This
function will return such a named list of contrast matrices to pass to these
functions. Note that this function should not be used within a modeling
function call, e.g., \code{lm(y~x, data = model_data, contrasts =
enlist_contrasts(model_data, x~sum_code))}. Often, this will call
\code{enlist_contrasts} twice, rather than just once.

For some model fitting functions, like \link[brms]{brm}, there is no
contrasts argument. For such cases, use \link[contrastable]{set_contrasts} to
set contrasts directly to the factors in a dataframe.

One good way to use \link[contrastable]{enlist_contrasts} is in conjunction
with \link[MASS]{fractions} to create a list of matrices that can be printed
to explicitly show the entire contrast matrices you're using for your models.
This can be especially helpful for supplementary materials in an academic
paper.

Sometimes when using orthogonal polynomial contrasts from
\link[stats]{contr.poly}, people will drop higher level polynomials for
parsimony. Note however that these do capture some amount of variation, so
even though they're orthogonal contrasts the lower level polynomials will
have their estimates changed. Moreover, you cannot reduce a contrast matrix
to a matrix smaller than size n*n-1 in the dataframe you pass to a model
fitting function itself, as R will try to fill in the gaps with something
else. If you want to drop contrasts you'll need to use something like
\code{enlist_contrasts(df, x ~ contr.poly - 3:5)} and pass this to the
\code{contrasts} argument in the model fitting function.
}
\examples{
my_df <- mtcars
my_df$gear <- factor(my_df$gear)
my_df$carb <- factor(my_df$carb)

# Use formulas where left hand side is the factor column name
# and the right hand side is the contrast scheme you want to use
enlist_contrasts(
  my_df,
  gear ~ scaled_sum_code, # Using helpers from this package
  carb ~ helmert_code
)

# Add reference levels with +
enlist_contrasts(
  my_df,
  gear ~ scaled_sum_code + 5,
  carb ~ contr.sum + 6
)
# Manually specifying matrix also works
enlist_contrasts(
  my_df,
  gear ~ matrix(c(1, -1, 0, 0, -1, 1), nrow = 3),
  carb ~ forward_difference_code
)

# User matrices can be assigned to a variable first, but this may make the
# comparison labels confusing. You should rename them manually to something
# that makes sense. This will invoke use_contrast_matrix, so reference levels
# specified with + will be ignored.
my_gear_contrasts <- matrix(c(1, -1, 0, 0, -1, 1), nrow = 3)
colnames(my_gear_contrasts) <- c("CMP1", "CMP2")
enlist_contrasts(
  my_df,
  gear ~ my_gear_contrasts,
  carb ~ forward_difference_code
)


# Will inform you if there are factors you didn't set
enlist_contrasts(my_df, gear ~ scaled_sum_code)

# Use MASS::fractions to pretty print matrices for academic papers:
enlist_contrasts(my_df, gear ~ scaled_sum_code, carb ~ helmert_code) |>
lapply(MASS::fractions)

# Use a list of formulas to use the same contrasts with different datasets
my_contrasts <- list(gear ~ scaled_sum_code, carb ~ helmert_code)
enlist_contrasts(my_df,  my_contrasts)
enlist_contrasts(mtcars, my_contrasts)

}
\seealso{
\code{\link[=set_contrasts]{set_contrasts()}}
}
