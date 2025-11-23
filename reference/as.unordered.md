# Convert to unordered factor

Unordered analogue of base R's `as.ordered`. Will convert `x` to an
unordered factor; unlike
[`as.factor()`](https://rdrr.io/r/base/factor.html), this will convert
ordered factors to unordered factors.

## Usage

``` r
as.unordered(x)
```

## Arguments

- x:

  Object to convert to unordered factor

## Value

`x` as an unordered factor

## See also

[`as.factor()`](https://rdrr.io/r/base/factor.html)

## Examples

``` r
# Convert an ordered factor to unordered
as.unordered(gl(5,1,ordered = TRUE))
#> [1] 1 2 3 4 5
#> Levels: 1 2 3 4 5

# If level order is pre-specified differently from default alphabetical order
# then the ordering will be retained
as.unordered(ordered(c("a", "b", "c"), levels = c("c", "a", "b")))
#> [1] a b c
#> Levels: c a b

# Otherwise the vector will be converted to an unordered factor with levels
# in the default alphabetical order
as.unordered(c("c", "a", "b"))
#> [1] c a b
#> Levels: a b c

# Note that coercing integer values will sort the values to use as the levels
as.unordered(4:1)
#> [1] 4 3 2 1
#> Levels: 1 2 3 4
```
