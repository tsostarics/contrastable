# Function method for use_contrasts

If the user provides a function, use the function and supplied arguments
to create a contrast matrix

## Usage

``` r
# S3 method for class '`function`'
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

  A factor vector, eg from df\$factorVarName

- code_by:

  A function to be called, should return a contrast matrix

- reference_level:

  The name of the level to use as the reference level, default NA

- set_intercept:

  The intercept to use, default NA

- drop_trends:

  The trends to drop, default NA

- labels:

  A vector of labels to apply to the matrix column names, default

- as_is:

  Logical, default FALSE, whether to leave the resulting matrix

- ...:

  Additional arguments to be passed to `code_by()`

## Value

A contrast coding matrix with labels and proper reference level

## Examples

``` r
use_contrasts(gl(5,1), sum_code)
#>    2  3  4  5
#> 1 -1 -1 -1 -1
#> 2  1  0  0  0
#> 3  0  1  0  0
#> 4  0  0  1  0
#> 5  0  0  0  1
```
