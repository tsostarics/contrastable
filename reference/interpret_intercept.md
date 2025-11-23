# Interpret intercept from contrasts

Given a contrast matrix, try and interpret the intercept. Will usually
be either the grand mean, the mean of a reference level (e.g.
contr.treatment), the unweighted mean of multiple levels. Anything else
would indicate custom weights that the user provided, hence they should
know how to interpret it.

## Usage

``` r
interpret_intercept(contrast_matrix)
```

## Arguments

- contrast_matrix:

  Contrast matrix

## Value

A string describing how to interpret the effect on the intercept this
coding scheme has

## Examples

``` r
interpret_intercept(contr.treatment(2)) # mean(1)
#> [1] "mean(1)"
interpret_intercept(contr.SAS(2)) # mean(2)
#> [1] "mean(2)"
interpret_intercept(contr.sum(2)) # grand mean
#> [1] "grand mean"

# Here there are 3 levels but the intercept is either an unweighted
# mean of 2 levels or a weighted mean of 2 levels
unweighted_intercept <-
  solve(t(matrix(c(.5, .5, 0, -1, 1, 0, -1, 0, 1), nrow = 3)))[, 2:3]
weighted_intercept <-
  solve(t(matrix(c(.8, .2, 0, -1, 1, 0, -1, 0, 1), nrow = 3)))[, 2:3]

interpret_intercept(unweighted_intercept) # mean(1,2)
#> [1] "mean(1,2)"
interpret_intercept(weighted_intercept) # custom weights
#> [1] "custom weights"
```
