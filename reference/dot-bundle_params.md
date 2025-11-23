# Extract parameters from dots

If there are any other arguments passed to a contrast coding function by
the user, ensure they're bundled together correctly. This is especially
important for setting the number of levels correctly.

## Usage

``` r
.bundle_params(factor_col, ...)
```

## Arguments

- factor_col:

  Factor to set contrasts to

- ...:

  Other arguments passed by the user

## Value

Function call parameters as a list
