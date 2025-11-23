# Expand contrast formulas

Uses `{tidyselect}` functionality to expand the left hand side of a
formula into multiple formulas. See examples of `enlist_contrasts` for
examples.

## Usage

``` r
.expand_formulas(formulas, data)
```

## Arguments

- formulas:

  List of formulas

- data:

  Dataframe to evaluate names in

## Value

Named list of formulas
