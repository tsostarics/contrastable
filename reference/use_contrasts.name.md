# Symbol method for use_contrasts

Evaluates `code_by`, then applies the appropriate use_contrasts method

## Usage

``` r
# S3 method for class 'name'
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

  A symbol to be evaluated

- reference_level:

  The level to use as the reference level, default NA

- set_intercept:

  The intercept to use, default NA

- drop_trends:

  The trends to drop, default NA

- labels:

  A vector of labels to apply to the matrix column names, default NULL
  (no new labels)

- as_is:

  Logical, default FALSE, whether to leave the resulting matrix as-is

- ...:

  Additional arguments to be passed on

## Value

A contrast coding matrix with labels and proper reference level

## Examples

``` r
aliased_scheme <- sum_code
contrast_scheme <- rlang::sym("aliased_scheme")

# Result will be as if sum_code was used directly
use_contrasts(gl(5,1), contrast_scheme)
#>    2  3  4  5
#> 1 -1 -1 -1 -1
#> 2  1  0  0  0
#> 3  0  1  0  0
#> 4  0  0  1  0
#> 5  0  0  0  1
```
