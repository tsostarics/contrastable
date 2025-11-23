# Process factor column

This is usually the first thing to be processed in a formula like
`varname ~ x`; ensures that `varname` is treated as a factor vector

## Usage

``` r
.process_factor_col(cur_expr, params, env, verbose)
```

## Arguments

- cur_expr:

  Current expression, see
  [`.make_parameters()`](https://tsostarics.github.io/contrastable/reference/dot-make_parameters.md)

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

Parameter list with `factor_col` updated
