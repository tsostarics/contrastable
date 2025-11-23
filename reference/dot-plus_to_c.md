# Handle `+` in formula LHS

Given a formula like cyl + gear ~ sum_code, this function recursively
replaces the `+` operators with `c` so that the LHS becomes
`c(cyl, gear)`.

## Usage

``` r
.plus_to_c(plus_expr)
```

## Arguments

- plus_expr:

  Expression

## Value

A new expression where `+` is replaced with
[`c()`](https://rdrr.io/r/base/c.html)

## Details

This function doesn't recurse into other function calls, for example:
`cyl + gear + factor(1 + 2)` evaluates to `c(cyl, gear, factor(1+2))`.
