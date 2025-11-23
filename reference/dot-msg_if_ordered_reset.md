# Alert user when setting ordered factors

Ordered factors use orthogonal polynomials (contr.poly) by default with
k number of levels -1 (k-1) of trend comparisons. If you use these
functions to set the contrasts to something else, the ordered class
(hence ordering to the levels) will remain but the contrasts will be set
to something else.

## Usage

``` r
.msg_if_ordered_reset(model_data, vars_in_model)
```

## Arguments

- model_data:

  Data frame to be used with the model

- vars_in_model:

  Variables to check against

## Value

Nothing, messages the user.
