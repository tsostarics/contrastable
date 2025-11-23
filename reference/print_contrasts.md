# Print contrasts

Utility to print all contrast matrices for a given dataframe. Displays
fractional values with
[`MASS::fractions()`](https://rdrr.io/pkg/MASS/man/fractions.html).
Prints nothing if there are no factor columns in the dataframe.

## Usage

``` r
print_contrasts(x, specific_vars)
```

## Arguments

- x:

  Dataframe containing factor columns

- specific_vars:

  Character vector of specific factor columns to print contrasts for.
  Use to only print a certain subset of variables.

## Value

Nothing

## Examples

``` r
print_contrasts(data.frame(x = gl(3, 1), y = gl(3, 1, ordered = TRUE)))
#> x 
#>   2 3
#> 1 0 0
#> 2 1 0
#> 3 0 1
#> y 
#>      .L            .Q           
#> [1,]    -5741/8119   19402/47525
#> [2,]             0 -86329/105731
#> [3,]     2378/3363   19402/47525
```
