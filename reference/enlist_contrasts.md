# Get a list of contrast matrices

Returns a named list of contrast matrices to use with modeling functions
directly. See
[`set_contrasts()`](https://tsostarics.github.io/contrastable/reference/set_contrasts.md)
for a function to set contrasts directly to the dataframe. See details
for syntax information

## Usage

``` r
enlist_contrasts(
  model_data,
  ...,
  verbose = getOption("contrastable.verbose"),
  .droplevels = getOption("contrastable.droplevels")
)
```

## Arguments

- model_data:

  Data frame you intend on passing to your model

- ...:

  A series of 2 sided formulas with factor name on the left hand side
  and desired contrast scheme on the right hand side. The reference
  level can be set with `+`, the intercept can be overwritten with `*`,
  comparison labels can be set using `|`, and trends for polynomial
  coding can be removed using `-`.

- verbose:

  Logical, defaults to TRUE, whether messages should be printed. See
  `Options("contrastable.verbose")`

- .droplevels:

  Logical, defaults to TRUE, whether missing levels from existing factor
  columns should be dropped before applying new contrasts. See
  `Options("contrastable.droplevels")`

## Value

List of named contrast matrices. Internally, if called within
set_contrasts, will return a named list with `contrasts` equal to the
list of named contrast matrices and `data` equal to the passed
`model_data` with any factor coercions applied (so that
[`set_contrasts()`](https://tsostarics.github.io/contrastable/reference/set_contrasts.md)
doesn't need to do it a second time).

## Details

`enlist_contrasts()`,
[`set_contrasts()`](https://tsostarics.github.io/contrastable/reference/set_contrasts.md),
and
[`glimpse_contrasts()`](https://tsostarics.github.io/contrastable/reference/glimpse_contrasts.md)
use special syntax to set contrasts for multiple factors. The syntax
consists of two-sided formulas with the desired factor column on the
left hand side and the contrast specification on the right hand side.
For example, `varname ~ scaled_sum_code`. Many contrasts support
additional kinds of contrast manipulations using overloaded operators:

- `+ X`: Set the reference level to the level named X. Only supported
  for schemes that have a singular reference level such as
  [`sum_code()`](https://tsostarics.github.io/contrastable/reference/sum_code.md),
  [`scaled_sum_code()`](https://tsostarics.github.io/contrastable/reference/scaled_sum_code.md),
  [`treatment_code()`](https://tsostarics.github.io/contrastable/reference/treatment_code.md),
  [`stats::contr.treatment()`](https://rdrr.io/r/stats/contrast.html),
  [`stats::contr.sum()`](https://rdrr.io/r/stats/contrast.html),
  [`stats::contr.SAS()`](https://rdrr.io/r/stats/contrast.html). Ignored
  for schemes like
  [`helmert_code()`](https://tsostarics.github.io/contrastable/reference/helmert_code.md).

- `* X`: Overwrite the intercept to the mean of the level named X

- `- A:B`: For polynomial coding schemes only, drop comparisons A
  through B.

- `| c(...)`: Change the comparison labels for the contrast matrix to
  the character vector `c(...)` of length `n-1`. These labels will
  appear in the output/summary of a statistical model. Note that for
  `brms::brm`, instances of `-` (a minus sign) are replaced with `M`.

You can also specify multiple variables on the left hand side of a
formula using tidyselect helpers. See examples for more information.

Typically model functions like lm will have a contrasts argument where
you can set the contrasts at model run time, rather than having to
manually change the contrasts on the underlying factor columns in your
data. This function will return such a named list of contrast matrices
to pass to these functions. Note that this function should not be used
within a modeling function call, e.g.,
`lm(y~x, data = model_data, contrasts = enlist_contrasts(model_data, x~sum_code))`.
Often, this will call `enlist_contrasts` twice, rather than just once.

For some model fitting functions, like `brms::brm`, there is no
contrasts argument. For such cases, use
[`set_contrasts()`](https://tsostarics.github.io/contrastable/reference/set_contrasts.md)
to set contrasts directly to the factors in a dataframe.

One good way to use `enlist_contrasts()` is in conjunction with
[`MASS::fractions()`](https://rdrr.io/pkg/MASS/man/fractions.html) to
create a list of matrices that can be printed to explicitly show the
entire contrast matrices you're using for your models. This can be
especially helpful for supplementary materials in an academic paper.

Sometimes when using orthogonal polynomial contrasts from
[`stats::contr.poly()`](https://rdrr.io/r/stats/contrast.html) people
will drop higher level polynomials for parsimony. Note however that
these do capture some amount of variation, so even though they're
orthogonal contrasts the lower level polynomials will have their
estimates changed. Moreover, you cannot reduce a contrast matrix to a
matrix smaller than size n\*n-1 in the dataframe you pass to a model
fitting function itself, as R will try to fill in the gaps with
something else. If you want to drop contrasts you'll need to use
something like `enlist_contrasts(df, x ~ contr.poly - 3:5)` and pass
this to the `contrasts` argument in the model fitting function.

## See also

[`set_contrasts()`](https://tsostarics.github.io/contrastable/reference/set_contrasts.md)
[`glimpse_contrasts()`](https://tsostarics.github.io/contrastable/reference/glimpse_contrasts.md)

## Examples

``` r
my_df <- mtcars
my_df$gear <- factor(my_df$gear)
my_df$carb <- factor(my_df$carb)

# Use formulas where left hand side is the factor column name
# and the right hand side is the contrast scheme you want to use
enlist_contrasts(
  my_df,
  gear ~ scaled_sum_code,
  carb ~ helmert_code,
  verbose = FALSE
)
#> $gear
#>            4          5
#> 3 -0.3333333 -0.3333333
#> 4  0.6666667 -0.3333333
#> 5 -0.3333333  0.6666667
#> 
#> $carb
#>     <2         <3    <4   <6         <8
#> 1 -0.5 -0.3333333 -0.25 -0.2 -0.1666667
#> 2  0.5 -0.3333333 -0.25 -0.2 -0.1666667
#> 3  0.0  0.6666667 -0.25 -0.2 -0.1666667
#> 4  0.0  0.0000000  0.75 -0.2 -0.1666667
#> 6  0.0  0.0000000  0.00  0.8 -0.1666667
#> 8  0.0  0.0000000  0.00  0.0  0.8333333
#> 

# Add reference levels with +
enlist_contrasts(
  my_df,
  gear ~ scaled_sum_code + 5,
  carb ~ contr.sum + 6,
  verbose = FALSE
)
#> $gear
#>            3          4
#> 3  0.6666667 -0.3333333
#> 4 -0.3333333  0.6666667
#> 5 -0.3333333 -0.3333333
#> 
#> $carb
#>    1  2  3  4  8
#> 1  1  0  0  0  0
#> 2  0  1  0  0  0
#> 3  0  0  1  0  0
#> 4  0  0  0  1  0
#> 6 -1 -1 -1 -1 -1
#> 8  0  0  0  0  1
#> 
# Manually specifying matrix also works
enlist_contrasts(
  my_df,
  gear ~ matrix(c(1, -1, 0, 0, -1, 1), nrow = 3),
  carb ~ forward_difference_code,
  verbose = FALSE
)
#> $gear
#>    4  5
#> 3 -1 -1
#> 4  1  0
#> 5  0  1
#> 
#> $carb
#>          1-2        2-3  3-4        4-6        6-8
#> 1  0.8333333  0.6666667  0.5  0.3333333  0.1666667
#> 2 -0.1666667  0.6666667  0.5  0.3333333  0.1666667
#> 3 -0.1666667 -0.3333333  0.5  0.3333333  0.1666667
#> 4 -0.1666667 -0.3333333 -0.5  0.3333333  0.1666667
#> 6 -0.1666667 -0.3333333 -0.5 -0.6666667  0.1666667
#> 8 -0.1666667 -0.3333333 -0.5 -0.6666667 -0.8333333
#> 

# User matrices can be assigned to a variable first, but this may make the
# comparison labels confusing. You should rename them manually to something
# that makes sense. This will invoke use_contrast_matrix, so reference levels
# specified with + will be ignored.
my_gear_contrasts <- matrix(c(1, -1, 0, 0, -1, 1), nrow = 3)
colnames(my_gear_contrasts) <- c("CMP1", "CMP2")
enlist_contrasts(
  my_df,
  gear ~ my_gear_contrasts,
  carb ~ forward_difference_code,
  verbose = FALSE
)
#> $gear
#>    4  5
#> 3 -1 -1
#> 4  1  0
#> 5  0  1
#> 
#> $carb
#>          1-2        2-3  3-4        4-6        6-8
#> 1  0.8333333  0.6666667  0.5  0.3333333  0.1666667
#> 2 -0.1666667  0.6666667  0.5  0.3333333  0.1666667
#> 3 -0.1666667 -0.3333333  0.5  0.3333333  0.1666667
#> 4 -0.1666667 -0.3333333 -0.5  0.3333333  0.1666667
#> 6 -0.1666667 -0.3333333 -0.5 -0.6666667  0.1666667
#> 8 -0.1666667 -0.3333333 -0.5 -0.6666667 -0.8333333
#> 


# Will inform you if there are factors you didn't set
enlist_contrasts(my_df, gear ~ scaled_sum_code)
#> Expect contr.treatment or contr.poly for unset factors: carb
#> $gear
#>            4          5
#> 3 -0.3333333 -0.3333333
#> 4  0.6666667 -0.3333333
#> 5 -0.3333333  0.6666667
#> 

# Use MASS::fractions to pretty print matrices for academic papers:
lapply(enlist_contrasts(my_df, gear ~ scaled_sum_code, carb ~ helmert_code),
       MASS::fractions)
#> $gear
#>   4    5   
#> 3 -1/3 -1/3
#> 4  2/3 -1/3
#> 5 -1/3  2/3
#> 
#> $carb
#>   <2   <3   <4   <6   <8  
#> 1 -1/2 -1/3 -1/4 -1/5 -1/6
#> 2  1/2 -1/3 -1/4 -1/5 -1/6
#> 3    0  2/3 -1/4 -1/5 -1/6
#> 4    0    0  3/4 -1/5 -1/6
#> 6    0    0    0  4/5 -1/6
#> 8    0    0    0    0  5/6
#> 

# Use a list of formulas to use the same contrasts with different datasets
my_contrasts <- list(gear ~ scaled_sum_code, carb ~ helmert_code)
enlist_contrasts(my_df,  my_contrasts)
#> $gear
#>            4          5
#> 3 -0.3333333 -0.3333333
#> 4  0.6666667 -0.3333333
#> 5 -0.3333333  0.6666667
#> 
#> $carb
#>     <2         <3    <4   <6         <8
#> 1 -0.5 -0.3333333 -0.25 -0.2 -0.1666667
#> 2  0.5 -0.3333333 -0.25 -0.2 -0.1666667
#> 3  0.0  0.6666667 -0.25 -0.2 -0.1666667
#> 4  0.0  0.0000000  0.75 -0.2 -0.1666667
#> 6  0.0  0.0000000  0.00  0.8 -0.1666667
#> 8  0.0  0.0000000  0.00  0.0  0.8333333
#> 
enlist_contrasts(mtcars, my_contrasts)
#> Converting to factors: gear carb
#> $gear
#>            4          5
#> 3 -0.3333333 -0.3333333
#> 4  0.6666667 -0.3333333
#> 5 -0.3333333  0.6666667
#> 
#> $carb
#>     <2         <3    <4   <6         <8
#> 1 -0.5 -0.3333333 -0.25 -0.2 -0.1666667
#> 2  0.5 -0.3333333 -0.25 -0.2 -0.1666667
#> 3  0.0  0.6666667 -0.25 -0.2 -0.1666667
#> 4  0.0  0.0000000  0.75 -0.2 -0.1666667
#> 6  0.0  0.0000000  0.00  0.8 -0.1666667
#> 8  0.0  0.0000000  0.00  0.0  0.8333333
#> 

# Use tidyselect helpers to set multiple variables at once
# These are all equivalent
contr_list1 <- enlist_contrasts(mtcars,
                 cyl ~ sum_code, gear ~ sum_code,
                 verbose = FALSE)

contr_list2 <- enlist_contrasts(mtcars,
                 cyl + gear ~ sum_code,
                 verbose = FALSE)

contr_list3 <- enlist_contrasts(mtcars,
                 c(cyl, gear) ~ sum_code,
                 verbose = FALSE)

contr_list4 <- enlist_contrasts(mtcars,
                 all_of(c('cyl', 'gear')) ~ sum_code,
                 verbose = FALSE)


these_vars <- c("cyl", "gear")
contr_list5 <- enlist_contrasts(mtcars,
                                all_of(these_vars) ~ sum_code,
                                verbose = FALSE)

all.equal(contr_list1, contr_list2)
#> [1] TRUE
all.equal(contr_list2, contr_list3)
#> [1] TRUE
all.equal(contr_list3, contr_list4)
#> [1] TRUE
all.equal(contr_list4, contr_list5)
#> [1] TRUE

# You can also use [tidyselect::where()] with class checking helpers:
contr_list6 <- enlist_contrasts(mtcars,
                                where(is.numeric) ~ sum_code,
                                verbose = FALSE)

# Each variable name must only be set ONCE, e.g. these will fail:
try(enlist_contrasts(mtcars,
                     cyl ~ sum_code,
                     cyl ~ scaled_sum_code,
                     verbose = FALSE))
#> Error : In `pkgdown::build_site_github_pages(new_process = FALSE, install = ...`: 
#> Names must be unique.
#> ✖ These names are duplicated:
#>   * "cyl" at locations 1 and 2.
#> ℹ Left hand side of multiple formulas evaluated to the same column name
try(enlist_contrasts(mtcars,
                     cyl ~ sum_code,
                     all_of(these_vars) ~ scaled_sum_code,
                     verbose = FALSE))
#> Error : In `pkgdown::build_site_github_pages(new_process = FALSE, install = ...`: 
#> Names must be unique.
#> ✖ These names are duplicated:
#>   * "cyl" at locations 1 and 2.
#> ℹ Left hand side of multiple formulas evaluated to the same column name
try(enlist_contrasts(mtcars,
                     cyl ~ sum_code,
                     where(is.numeric) ~ scaled_sum_code,
                     verbose = FALSE))
#> Error : In `pkgdown::build_site_github_pages(new_process = FALSE, install = ...`: 
#> Names must be unique.
#> ✖ These names are duplicated:
#>   * "cyl" at locations 1 and 3.
#> ℹ Left hand side of multiple formulas evaluated to the same column name
```
