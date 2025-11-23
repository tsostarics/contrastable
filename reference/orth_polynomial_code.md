# Use orthogonal polynomial coding

Wrapper around
[`stats::contr.poly()`](https://rdrr.io/r/stats/contrast.html). You can
also use `polynomial_code()` as an alias.

## Usage

``` r
orth_polynomial_code(n)

polynomial_code(n)
```

## Arguments

- n:

  Integer umber of factor levels to compute contrasts for.

## Value

A contrast matrix with dimensions n rows and (n-1) columns.

## Details

For n levels of factors where k in 1:n, generate a matrix with n-1
comparisons where each comparison looks for a polynomial trend of degree
k where each polynomial is independent of the others.

## Examples

``` r
mydf <- data.frame(
  grp = rep(c("a", "b", "c", "d"), each = 2000),
  val = c(
    rnorm(200, 2, 1),
    rnorm(200, 5, 1),
    rnorm(200, 7.5, 1),
    rnorm(200, 15, 1)
  )
) |>
  set_contrasts(grp ~ polynomial_code)
#> Converting to factors: grp

stats::lm(val ~ grp, data = mydf)
#> 
#> Call:
#> stats::lm(formula = val ~ grp, data = mydf)
#> 
#> Coefficients:
#> (Intercept)        grp.L        grp.Q        grp.C  
#>   7.374e+00    6.960e-01    7.048e-17    1.392e+00  
#> 
```
