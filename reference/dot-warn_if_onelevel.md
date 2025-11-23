# Warn if one level factor

Factors with only one level happen if a character vector is converted to
a factor using [`factor()`](https://rdrr.io/r/base/factor.html) but
without specifying anything for the `levels` parameter. If you try to
access or set the contrasts for a one-level factor, you'll get an error
that contrasts are undefined because you have 0 degrees of freedom.

## Usage

``` r
.warn_if_onelevel(
  one_level_factors = NULL,
  model_data = NULL,
  attempting_factors = NULL
)
```

## Arguments

- one_level_factors:

  Character vector of which factors are one level

- model_data:

  Model data to look for factor columns

- attempting_factors:

  Factor column names to check

## Value

Nothing, warns if factors with only one level are detected.
