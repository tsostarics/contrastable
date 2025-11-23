# Drop missing levels

Missing levels can cause unexpected behavior when fitting a model after
setting contrasts. This function removes missing levels and informs the
user of how many levels were dropped from each factor.

## Usage

``` r
.droplevels_as_needed(model_data, lhs_variables, verbose = TRUE)
```

## Arguments

- model_data:

  Model data

- lhs_variables:

  variables specified for contrast coding from formulas

- verbose:

  Should messages be sent? Defaults to TRUE

## Value

model_data where existing factor columns have had any missing levels
removed
