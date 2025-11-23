# Get columns where

Helper to avoid the use of tidyselect and dplyr::select, returns either
a logical vector (optionally named) or a character vector of which
columns satisfy the given function

## Usage

``` r
.cols_where(model_data, fx, use_names = FALSE, return_names = FALSE)
```

## Arguments

- model_data:

  Model data

- fx:

  Function to apply, must be something that returns a logical value.
  Usually either `is.factor` or `is.ordered`

- use_names:

  Whether the resulting vector should be named

- return_names:

  Whether names (where the fx returns TRUE) should be returned instead
  of a logical vector. Overwrites use.names.

## Value

Optionally named logical vector or character vector depending on values
of `use_names` and `return_names`
