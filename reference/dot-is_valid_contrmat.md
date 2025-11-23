# Check if valid contrast

Sometimes a user might pass a matrix that can't actually be used, in
which case we can avoid some calculations by stopping earlier.

## Usage

``` r
.is_valid_contrmat(contrast_matrix)
```

## Arguments

- contrast_matrix:

  Contrast matrix

## Value

invisibly returns TRUE if the contrast matrix is valid
