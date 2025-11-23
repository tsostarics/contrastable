# Use forward difference coding

Compares the mean of level k to level k+1. Differs in direction from
[backward_difference_code](https://tsostarics.github.io/contrastable/reference/backward_difference_code.md),
so be careful to pick the right function. See also
[contr.sdif](https://rdrr.io/pkg/MASS/man/contr.sdif.html).

## Usage

``` r
forward_difference_code(n)
```

## Arguments

- n:

  Integer umber of factor levels to compute contrasts for.

## Value

A contrast matrix with dimensions n rows and (n-1) columns.

## Details

Example interpretation for a 4 level factor:

- Intercept = Grand mean (mean of the means of each level)

- grp1 = mean(grp1) - mean(grp2)

- grp2 = mean(grp2) - mean(grp3)

- grp3 = mean(grp3) - mean(grp4)

## Examples

``` r
mydf <- data.frame(
  grp = gl(4,5),
  resp = c(seq(1, 5), seq(5, 9), seq(10, 14), seq(15, 19))
)

mydf <- set_contrasts(mydf, grp ~ forward_difference_code)

lm(resp ~ grp, data = mydf)
#> 
#> Call:
#> lm(formula = resp ~ grp, data = mydf)
#> 
#> Coefficients:
#> (Intercept)       grp1-2       grp2-3       grp3-4  
#>        9.75        -4.00        -5.00        -5.00  
#> 
```
