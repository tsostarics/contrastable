# Get dimnames of contrasts from factor

Given a factor, extract the row and column names of the contrasts. If
they're not set, then use default values. Also helps to catch invalid
usage of polynomial contrasts.

## Usage

``` r
.get_dimnames(factor_col)
```

## Arguments

- factor_col:

  Factor to extract contrasts from

## Value

List of rownames and column names
