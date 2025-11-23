# Make parameters for contrast code call

Given a formula, recursively go through the abstract syntax tree and
extract the necessary parameters for
[`use_contrasts()`](https://tsostarics.github.io/contrastable/reference/use_contrasts.md).
While this method is more involved than extracting from a parsed string
representation, I think it handles matrix calls better since it plucks
the call right out of the syntax tree.

## Usage

``` r
.make_parameters(
  formula,
  params = list(factor_col = NA, code_by = NA, reference_level = NA, intercept_level =
    NA, drop_trends = NA, labels = NULL, as_is = FALSE),
  env = NULL,
  verbose = TRUE
)
```

## Arguments

- formula:

  Formula given by user

- params:

  Parameter accumulator

- env:

  Environment for the formula, on the first recursion this will be
  pulled from `formula` and then passed to subsequent recursions. Needed
  to check whether a function is actually a function.

- verbose:

  Logical, default `TRUE`, whether to show drop_trends warning if used
  incorrectly

## Value

Named list of parameters that can be evaluated in
[`.process_contrasts()`](https://tsostarics.github.io/contrastable/reference/dot-process_contrasts.md)
