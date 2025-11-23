# Use contrast specification for given factor

Generic for setting contrasts, primarily intended for internal use.

## Usage

``` r
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

  The factor column to use, eg data\$gender

- code_by:

  Either a matrix or a function

- reference_level:

  The level to use as the reference level, default NA

- set_intercept:

  The intercept to use, default NA

- drop_trends:

  Whether to drop trends, default NA

- labels:

  Labels to use in the contrast matrix, must equal number of contrasts

- as_is:

  Logical, default FALSE, whether to suppress auto switching of the
  reference level to the first level if not specified

- ...:

  Additional arguments to be passed to use_contrast_function,
  specifically, which level you want the reference level to be

## Value

A contrast coding matrix with labels and proper reference level

## See also

[`set_contrasts()`](https://tsostarics.github.io/contrastable/reference/set_contrasts.md)
[`enlist_contrasts()`](https://tsostarics.github.io/contrastable/reference/enlist_contrasts.md)

## Examples

``` r
# Create a contrast matrix given some factor vector with the specified
# reference level
use_contrasts(gl(5,2), sum_code, reference_level = 3)
#>    1  2  4  5
#> 1  1  0  0  0
#> 2  0  1  0  0
#> 3 -1 -1 -1 -1
#> 4  0  0  1  0
#> 5  0  0  0  1

# Set column labels; order for labels is the same as the column indices
use_contrasts(gl(3,2), scaled_sum_code, labels = c("2-1", "3-1"))
#>          2-1        3-1
#> 1 -0.3333333 -0.3333333
#> 2  0.6666667 -0.3333333
#> 3 -0.3333333  0.6666667

my_data <- mtcars
my_data$gear <- factor(mtcars$gear)

MASS::fractions(use_contrasts(my_data$gear, helmert_code))
#>   4    5   
#> 3 -1/2 -1/3
#> 4  1/2 -1/3
#> 5    0  2/3
```
