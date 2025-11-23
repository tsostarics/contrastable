# Glimpse default factors

Given a dataframe with some factor columns and a character vector of
which columns you've already set yourself, look at all the other factor
columns and get a glimpse at how they're treated by the defaults
specified in `options('contrasts')`. Reference level is assumed to be
the first level for unordered factors and nonexistent for ordered
factors.

## Usage

``` r
.glimpse_default_factors(
  model_data,
  set_factors = NULL,
  show_one_level_factors = FALSE,
  minimal = TRUE,
  verbose = TRUE
)
```

## Arguments

- model_data:

  Data to be passed to a model fitting function

- set_factors:

  Explicitly set columns to ignore

- show_one_level_factors:

  Logical, should factors with only one level be included in the output?
  Default is FALSE to omit

- minimal:

  Logical, default TRUE, whether to omit the orthogonal, centered,
  dropped_trends, and explicitly_set columns from the output table

- verbose:

  Logical, defaults to TRUE, whether messages should be printed

## Value

A table with information about the contrasts for all remaining factor
columns
