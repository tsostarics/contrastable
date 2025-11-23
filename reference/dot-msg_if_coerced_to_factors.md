# Alert user when factor coercion happens

messages the user if columns in the dataset have been coerced to a
factor, useful if you accidentally set a column with continuous data to
a factor.

## Usage

``` r
.msg_if_coerced_to_factors(which_to_factors)
```

## Arguments

- which_to_factors:

  Names of columns that have been coerced to factors

## Value

Nothing, messages the user if needed.
