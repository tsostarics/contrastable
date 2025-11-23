# Make glimpse for 1-level factors

If the user wants factors with only one level included, this will create
the table to present that information. It's a lot of NAs because
contrasts aren't defined for only 1 level.

## Usage

``` r
.make_placeholder_glimpse(model_data, one_level_factors)
```

## Arguments

- model_data:

  Model data

- one_level_factors:

  Which factors are one level

## Value

A data.frame with limited information about one level factors
