# Use scaled sum coding

Contrast coding scheme with a centered intercept and comparisons from a
baseline reference level.

## Usage

``` r
scaled_sum_code(n)
```

## Arguments

- n:

  Integer umber of factor levels to compute contrasts for.

## Value

A contrast matrix with dimensions n rows and (n-1) columns.

## Details

The name for this contrast scheme varies widely in different fields and
across experimental psychology papers. It has been called simple, sum,
contrast, sum-to-zero, and deviation coding (among other names). This
package uses scaled sum coding to explicitly differentiate it from sum
coding, which has an implementation in base R with `contr.sum`.

For n levels of factors, generate a matrix with n-1 comparisons where:

- Reference level = -1/n

- Comparison level = (n-1)/n

- All others = -1/n

Example interpretation for a 4 level factor:

- Intercept = Grand mean (mean of the means of each level)

- grp2 = mean(grp2) - mean(grp1)

- grp3 = mean(grp3) - mean(grp1)

- grp4 = mean(grp4) - mean(grp1)

Note: grp coefficient estimates are the same as with contr.treatment,
but the intercept is changed to the grand mean instead of the mean of
grp1.

It's also important to note that this coding scheme is NOT the same as
`contr.sum/2` when the number of levels is greater than 2. When n=2,
estimates with `contr.sum` can be interpreted as "half the distance
between levels" but when k\>2, `contr.sum` is to be interpreted as "the
distance between this level and the GRAND MEAN". You may be tempted to
use `contr.sum(n)/2`, but this tests the hypothesis that 3/2 times the
mean of a level is equal to half the sum of the means of the other
levels, i.e., \\1.5\mu_1 - .5\mu_2 - .5\mu_3 - .5\mu_4 = 0\\, which is
not likely to be what you're looking for.

## Examples

``` r
# Compare these two, note that contr.sum(4)/2 is not the same
scaled_sum_code(4)
#>       2     3     4
#> 1 -0.25 -0.25 -0.25
#> 2  0.75 -0.25 -0.25
#> 3 -0.25  0.75 -0.25
#> 4 -0.25 -0.25  0.75
contr.sum(4)
#>   [,1] [,2] [,3]
#> 1    1    0    0
#> 2    0    1    0
#> 3    0    0    1
#> 4   -1   -1   -1

# Here they happen to be equivalent (modulo reference level)
scaled_sum_code(2)
#>      2
#> 1 -0.5
#> 2  0.5
contr.sum(2) / 2
#>   [,1]
#> 1  0.5
#> 2 -0.5

mydf <- data.frame(
  grp = gl(4,5),
  resp = c(seq(1, 5), seq(5, 9), seq(10, 14), seq(15, 19))
)

mydf <- set_contrasts(mydf, grp ~ scaled_sum_code)

lm(resp ~ grp, data = mydf)
#> 
#> Call:
#> lm(formula = resp ~ grp, data = mydf)
#> 
#> Coefficients:
#> (Intercept)         grp2         grp3         grp4  
#>        9.75         4.00         9.00        14.00  
#> 
```
