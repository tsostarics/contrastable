# hypr method for use_contrasts

hypr method for use_contrasts

## Usage

``` r
# S3 method for class 'hypr'
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

  A hypr object created with
  [`hypr::hypr()`](https://rdrr.io/pkg/hypr/man/hypr.html)

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

Contrast matrix specified by the hypr object

## Examples

``` r
hypr_obj <- hypr::hypr(a ~ b, c ~ b) # centered pairwise comparisons to b

use_contrasts(factor(c('a', 'b', 'c')), hypr_obj)
#>   [,1] [,2]
#> a  2/3 -1/3
#> b -1/3 -1/3
#> c -1/3  2/3
```
