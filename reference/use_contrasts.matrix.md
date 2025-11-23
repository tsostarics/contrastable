# Matrix method for use_contrasts

If a user provides a raw matrix, then use that matrix as the contrast
matrix

## Usage

``` r
# S3 method for class 'matrix'
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

  A matrix to be used as the contrast matrix, should have the same
  dimensions as the contrast matrix already applied to code_by

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

A contrast coding matrix with labels and proper reference level

## Examples

``` r
contrast_matrix <- sum_code(4)
use_contrasts(gl(4,1), contrast_matrix)
#>    2  3  4
#> 1 -1 -1 -1
#> 2  1  0  0
#> 3  0  1  0
#> 4  0  0  1
```
