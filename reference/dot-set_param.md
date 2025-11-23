# Process and set parameter

Unpacks the given expression to set the parameter specified by
`which_param` to `params`. Continues recursively setting parameters via
[`.make_parameters()`](https://tsostarics.github.io/contrastable/reference/dot-make_parameters.md).

## Usage

``` r
.set_param(cur_expr, params, env, which_param, verbose)
```

## Arguments

- cur_expr:

  Current expression, a formula or list representation thereof

- params:

  Parameter accumulator

- env:

  Environment for the formula, on the first recursion this will be
  pulled from `formula` and then passed to subsequent recursions. Needed
  to check whether a function is actually a function.

- which_param:

  Which parameter to set, a string for usage

- verbose:

  Logical, default `TRUE`, whether to show drop_trends warning if used
  incorrectly

## Value

`params`

## See also

[`.make_parameters()`](https://tsostarics.github.io/contrastable/reference/dot-make_parameters.md)
