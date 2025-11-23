# Use cumulative split coding

Contrast coding scheme that repeatedly dichotomizes the factor levels.

## Usage

``` r
cumulative_split_code(n)
```

## Arguments

- n:

  Integer umber of factor levels to compute contrasts for.

## Value

A contrast matrix with dimensions n rows and (n-1) columns.

## Details

This scheme is similar to Helmert contrasts, but instead of comparing
one level to the accumulated mean of all previous levels, each
comparison with this scheme splits the levels into two groups: those
below and including the current level, and those above the current
level. Conceptually this is similar to continuation ratio logits used in
ordinal models. For example, with a four level factor with levels A, B,
C, and D, the comparisons would be:

- A vs. BCD

- AB vs. CD

- ABC vs. D

In other words, each comparison splits the levels into two groups. Each
of these comparisons uses the cumulative mean of all the levels in each
group. The intercept is the grand mean.

## Examples

``` r

set.seed(111)
mydf <- data.frame(
  grp = rep(c("a", "b", "c", "d"), each = 400),
  val = c(
    rnorm(400, 2,   .05),
    rnorm(400, 4,   .05),
    rnorm(400, 12,  .05),
    rnorm(400, 20,  .05)
  )
) |>
  set_contrasts(grp ~ cumulative_split_code |
                      c("a-rest", "ab-rest", "abc-rest"))
#> Converting to factors: grp

# Coefficients: ~ 9.5, -10, -13, -14
lm(val ~ grp, data = mydf)
#> 
#> Call:
#> lm(formula = val ~ grp, data = mydf)
#> 
#> Coefficients:
#> (Intercept)    grpa-rest   grpab-rest  grpabc-rest  
#>       9.499       -9.996      -12.996      -13.993  
#> 
```
