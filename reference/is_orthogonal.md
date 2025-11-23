# Check for orthogonality

Given a contrast matrix or list of contrast matrices (eg from
[`enlist_contrasts()`](https://tsostarics.github.io/contrastable/reference/enlist_contrasts.md)
), return a logical vector of whether each contrast is orthogonal or
not.

## Usage

``` r
is_orthogonal(contrast_matrices, USE.NAMES = FALSE)
```

## Arguments

- contrast_matrices:

  Contrast matrix or list of contrast matrices

- USE.NAMES:

  Logical, whether vector should be named

## Value

Logical vector, will retain names of a passed list

## See also

[`is_centered()`](https://tsostarics.github.io/contrastable/reference/is_centered.md)

## Examples

``` r
is_orthogonal(treatment_code(5)) # FALSE
#> [1] FALSE
is_orthogonal(helmert_code(5)) # TRUE
#> [1] TRUE
```
