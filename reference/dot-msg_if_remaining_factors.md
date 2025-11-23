# Alert user if there are more factors

messages the user if the user has factor columns in their model data
frame that weren't specified along with any factors they DID set
contrasts for.

## Usage

``` r
.msg_if_remaining_factors(model_data, specified_vars)
```

## Arguments

- model_data:

  Model data

- specified_vars:

  Variables specified by the user from formulas

## Value

Nothing, messages the user if needed.
