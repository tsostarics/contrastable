# Default method for use_contrasts

If a user doesn't specify a contrast matrix, use the defaults from
options(). If the user tries to use something we don't know how to work
with, throw a warning that we'll be using the defaults from options().

## Usage

``` r
# Default S3 method
use_contrasts(
  factor_col,
  code_by = NA,
  reference_level = NA,
  set_intercept = NA,
  drop_trends = NA,
  labels = NULL,
  as_is = FALSE,
  ...
)
```

## Arguments

- factor_col:

  A factor vector, eg from `df$factorVarName`

- code_by:

  Some object that's not a matrix or function. If NA, no warning will be
  thrown, and the default contrasts will be used. A warning will be
  thrown if it's not NA.

- reference_level:

  Not used

- set_intercept:

  Not used

- drop_trends:

  Not used

- labels:

  A vector of labels to apply to the matrix column names, default

- as_is:

  Logical, default FALSE, whether to leave the resulting matrix

- ...:

  Additional arguments, not used

## Value

Contrast matrix, using the ordered or unordered default from
[`options()`](https://rdrr.io/r/base/options.html)

## Examples

``` r
use_contrasts(gl(5,1), helmert_code) # a function
#>      2          3     4    5
#> 1 -0.5 -0.3333333 -0.25 -0.2
#> 2  0.5 -0.3333333 -0.25 -0.2
#> 3  0.0  0.6666667 -0.25 -0.2
#> 4  0.0  0.0000000  0.75 -0.2
#> 5  0.0  0.0000000  0.00  0.8
my_matrix <- helmert_code(5)
use_contrasts(gl(5,1), my_matrix) # a matrix
#>      2          3     4    5
#> 1 -0.5 -0.3333333 -0.25 -0.2
#> 2  0.5 -0.3333333 -0.25 -0.2
#> 3  0.0  0.6666667 -0.25 -0.2
#> 4  0.0  0.0000000  0.75 -0.2
#> 5  0.0  0.0000000  0.00  0.8
```
