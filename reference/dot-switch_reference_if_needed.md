# Check if reference switching is necessary

Check if the contrast matrix is able to switch reference levels, and if
it is requested, do so

## Usage

``` r
.switch_reference_if_needed(
  contrast_matrix,
  new_reference_label = NA,
  new_reference_index
)
```

## Arguments

- contrast_matrix:

  Contrast matrix

- new_reference_label:

  Name of the level to use

- new_reference_index:

  Index of the level to use

## Value

Contrast matrix with reference level swapped if needed
