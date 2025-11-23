# Check for intercept centering

Given a contrast matrix or list of contrast matrices (eg from
[`enlist_contrasts()`](https://tsostarics.github.io/contrastable/reference/enlist_contrasts.md)),
return a logical vector of whether each contrast is centered or not.

## Usage

``` r
is_centered(contrast_matrices, USE.NAMES = FALSE)
```

## Arguments

- contrast_matrices:

  Contrast matrix or list of contrast matrices

- USE.NAMES:

  Logical, whether vector should be named

## Value

Logical vector, will retain names of a passed list

## See also

[`is_orthogonal()`](https://tsostarics.github.io/contrastable/reference/is_orthogonal.md)

## Examples

``` r
is_centered(treatment_code(5)) # FALSE
#> [1] FALSE
is_centered(scaled_sum_code(5)) # TRUE
#> [1] TRUE
```
