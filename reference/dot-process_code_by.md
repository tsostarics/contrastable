# Process code_by

Handles the `code_by` parameter and checks to make sure whether we can
safely drop trends or not. Also handles any usage of
[`I()`](https://rdrr.io/r/base/AsIs.html)

## Usage

``` r
.process_code_by(formula, params, env, verbose)
```

## Arguments

- formula:

  Formula used to set contrast

- params:

  Parameter list

- env:

  Not used

- verbose:

  Whether to throw the warning about invalid `-` usage

## Value

Modified parameter list with updated `code_by`
