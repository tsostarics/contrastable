# Use helmert coding

R's [`stats::contr.helmert()`](https://rdrr.io/r/stats/contrast.html)
function is unscaled, meaning that you need to scale the coefficients of
a model fit to get the actual comparisons of interest. This version will
automatically scale the contrast matrix such that the coefficients are
the expected scaled values.

## Usage

``` r
helmert_code(n)
```

## Arguments

- n:

  Integer umber of factor levels to compute contrasts for.

## Value

A contrast matrix with dimensions n rows and (n-1) columns.

## Details

Helmert coding compares each level to the total mean of all levels that
have come before it. Differs from backward difference coding, which
compares only pairs of levels (not a level to a cumulative mean of
levels)

Example interpretation for a 4 level factor:

- Intercept = Grand mean (mean of the means of each level)

- grp2 = mean(grp2) - mean(grp1)

- grp3 = mean(grp3) - mean(grp1, grp2)

- grp4 = mean(grp4) - mean(grp1, grp2, grp3)

## Examples

``` r
mydf <- data.frame(
  grp = gl(4,5),
  resp = c(seq(1, 5), seq(5, 9), seq(10, 14), seq(15, 19))
)

mydf <- set_contrasts(mydf, grp ~ helmert_code)
lm(resp ~ grp, data = mydf)
#> 
#> Call:
#> lm(formula = resp ~ grp, data = mydf)
#> 
#> Coefficients:
#> (Intercept)        grp<2        grp<3        grp<4  
#>       9.750        4.000        7.000        9.667  
#> 
```
