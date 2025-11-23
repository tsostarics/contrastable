# Check for unordered factor

Helper to check if a factor is exclusively unordered. is.factor(x) is
TRUE when x is unordered OR ordered.

## Usage

``` r
is.unordered(x)
```

## Arguments

- x:

  a vector of data

## Value

TRUE if x is an unordered factor, FALSE if x is not a factor or is an
ordered factor

## Examples

``` r
is.unordered(gl(5,1)) # True
#> [1] TRUE
is.unordered(gl(5,1,ordered = TRUE)) # False
#> [1] FALSE
```
