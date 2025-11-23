# Postprocess contrast matrices

When using
[`use_contrasts()`](https://tsostarics.github.io/contrastable/reference/use_contrasts.md)
with a function or matrix, there are shared postprocessing steps for
manipulating the contrasts to set the reference level, intercept, and
labels. If as_is is TRUE, then these post processing steps aren't
included.

## Usage

``` r
.postprocess_matrix(
  new_contrasts,
  code_by,
  reference_level,
  set_intercept,
  drop_trends,
  matrix_labels,
  labels,
  as_is,
  dots
)
```

## Arguments

- new_contrasts:

  Contrast matrix passed from
  [`use_contrasts()`](https://tsostarics.github.io/contrastable/reference/use_contrasts.md)

- code_by:

  What to code with, passed from
  [`use_contrasts()`](https://tsostarics.github.io/contrastable/reference/use_contrasts.md)

- reference_level:

  Reference level specification

- set_intercept:

  Intercept specification

- drop_trends:

  Any trends to drop

- matrix_labels:

  Labels from the matrix dimension names

- labels:

  User-specified labels

- as_is:

  Whether to use matrix as-is

- dots:

  Any additional arguments passed with code_by

## Value

Contrast matrix

## See also

[`use_contrasts()`](https://tsostarics.github.io/contrastable/reference/use_contrasts.md)
