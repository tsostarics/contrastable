# Convert non factors to factors

Helper to convert columns to factors if they aren't already

## Usage

``` r
.convert_to_factors(model_data, vars_in_model, verbose = TRUE)
```

## Arguments

- model_data:

  Model data

- vars_in_model:

  variables specified for contrast coding from formulas

- verbose:

  Should messages be sent? Defaults to TRUE

## Value

model_data with specified columns coerced to factors
