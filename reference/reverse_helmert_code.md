# Use reverse helmert coding

Reverse helmert coding is the same concept as helmert coding, but the
order of the groupings is reversed. See also
[helmert_code](https://tsostarics.github.io/contrastable/reference/helmert_code.md).

## Usage

``` r
reverse_helmert_code(n)
```

## Arguments

- n:

  Integer umber of factor levels to compute contrasts for.

## Value

A contrast matrix with dimensions n rows and (n-1) columns.

## Details

Reverse helmert coding compares each level to the total mean of all
levels that come after it. Differs from forward difference coding, which
only compares pairs of levels (not a level to a cumulative mean of
levels).

Example interpretation for a 4 level factor:

- Intercept = Grand mean (mean of the means of each level)

- grp1 = mean(grp4, grp3, grp2) - grp(1)

- grp2 = mean(grp4, grp3) - mean(grp2)

- grp3 = mean(grp3) - mean(grp4)

## Examples

``` r
mydf <- data.frame(
  grp = gl(4,5),
  resp = c(seq(1, 5), seq(5, 9), seq(10, 14), seq(15, 19))
)

mydf <- set_contrasts(mydf, grp ~ reverse_helmert_code)
lm(resp ~ grp, data = mydf)
#> 
#> Call:
#> lm(formula = resp ~ grp, data = mydf)
#> 
#> Coefficients:
#> (Intercept)        grp>1        grp>2        grp>3  
#>        9.75        -9.00        -7.50        -5.00  
#> 
```
