# Use treatment coding

Wrapper around
[`stats::contr.treatment()`](https://rdrr.io/r/stats/contrast.html).
Returns a contrast matrix where comparisons give differences between
each comparison level and a baseline reference level, while the
intercept equals the first level of the factor. See
[`scaled_sum_code()`](https://tsostarics.github.io/contrastable/reference/scaled_sum_code.md)
for a function that centers the intercept on the grand mean while
retaining pairwise comparisons from a reference level.

## Usage

``` r
treatment_code(n)
```

## Arguments

- n:

  Integer umber of factor levels to compute contrasts for.

## Value

A contrast matrix with dimensions n rows and (n-1) columns.

## Details

For n levels of factors, generate a matrix with n-1 comparisons where:

- Reference level = 0

- Comparison level = 1

- All others = 0

Note that this function sets the first level (alphabetically) as the
reference level while
[`stats::contr.SAS()`](https://rdrr.io/r/stats/contrast.html) sets the
LAST level as the reference level. However, in functions like
[`set_contrasts()`](https://tsostarics.github.io/contrastable/reference/set_contrasts.md),
and
[`enlist_contrasts()`](https://tsostarics.github.io/contrastable/reference/enlist_contrasts.md),
the reference level is automatically set to be the first level
alphabetically.

## Examples

``` r
mydf <- data.frame(
  grp = gl(4,5),
  resp = c(seq(1, 5), seq(5, 9), seq(10, 14), seq(15, 19))
)

mydf <- set_contrasts(mydf, grp ~ treatment_code)

lm(resp ~ grp, data = mydf)
#> 
#> Call:
#> lm(formula = resp ~ grp, data = mydf)
#> 
#> Coefficients:
#> (Intercept)         grp2         grp3         grp4  
#>           3            4            9           14  
#> 
```
