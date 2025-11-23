# Check for reserved operators

A helper for
[`.make_parameters()`](https://tsostarics.github.io/contrastable/reference/dot-make_parameters.md),
which takes a symbol and checks whether it corresponds to one of the
reserved operators for the package syntax.

## Usage

``` r
.get_reserved_operator(node)
```

## Arguments

- node:

  A symbol, extracted from a formula

## Value

If `node` is a reserved operator, then return the operator as a string.
Otherwise return the string "none".
