# Use raw polynomial coding

Make raw polynomial contrast, rather than orthogonal ones. Normally you
would use orthogonal polynomials, so make sure this is what you want.
Using raw polynomials may increase the collinearity in your model,
especially with higher numbers of levels.

## Usage

``` r
raw_polynomial_code(n)
```

## Arguments

- n:

  Integer umber of factor levels to compute contrasts for.

## Value

A contrast matrix with dimensions n rows and (n-1) columns.

## Details

For n levels of factors where k in 1:n, generate a matrix with n-1
comparisons where each comparison looks for a polynomial trend of degree
k, where each polynomial may be correlated with the others. Normally you
would use orthogonal polynomials, see
[`stats::contr.poly()`](https://rdrr.io/r/stats/contrast.html) and
[`orth_polynomial_code()`](https://tsostarics.github.io/contrastable/reference/orth_polynomial_code.md)

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
  set_contrasts(grp ~ raw_polynomial_code)
#> Converting to factors: grp

stats::lm(val ~ grp, data = mydf)
#> 
#> Call:
#> stats::lm(formula = val ~ grp, data = mydf)
#> 
#> Coefficients:
#> (Intercept)        grp.L        grp.Q        grp.C  
#>      -4.234       17.639       -7.782        1.038  
#> 
```
