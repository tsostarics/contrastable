# Set intercept for contrast matrix

Given a contrast matrix and a desired level, change the intercept via
manipulating underlying hypothesis matrix.

## Usage

``` r
.set_intercept(contrast_matrix, intercept_level)
```

## Arguments

- contrast_matrix:

  Contrast matrix to use

- intercept_level:

  Level to use as intercept, must be present in the row names of the
  contrast matrix

## Value

Manipulated contrast matrix
