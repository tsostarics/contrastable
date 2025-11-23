# Switch reference level in contrast matrix

Reorders the rows of a contrast matrix to properly set the reference
level

## Usage

``` r
.switch_reference_level(contrast_matrix, old_reference, new_reference)
```

## Arguments

- contrast_matrix:

  An unlabeled contrast matrix

- old_reference:

  The previously specified reference level, usually the default for the
  scheme found by `.get_reference_level`

- new_reference:

  The index of the row to use as the new reference level

## Value

A matrix with the reordered rows and correct values for the reference
level
