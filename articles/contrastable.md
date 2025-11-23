# contrastable

## Get started

This vignette provides a quick guide to start using this package for
your analyses. Additional examples with more written detail are
available in
[`vignette("contrasts")`](https://tsostarics.github.io/contrastable/articles/contrasts.md).

``` r
  # install.packages("contrastable")
library(contrastable)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

### Setting contrasts to data frame columns

There are three main functions which I’ll discuss in order:

- `set_contrasts`: set contrasts directly to factor columns
- `enlist_contrasts`: get a list of contrast matrices
- `glimpse_contrasts`: get a summary table of contrast information

All three use a shared two-sided formula syntax, for example:

``` r
enlist_contrasts(my_dataframe, 
                 varname ~ contrast_scheme + reference * intercept - dropped | labels)
```

- `varname`: The variable name of the column whose contrasts you want to
  set.
- `contrast_scheme`: (most often) a function that creates contrast
  matrices, can also be a variable assigned a matrix (eg
  `my_mat <- matrix(...)`, `my_mat` can be used) or a `hypr` object.
- `reference`: Use the `+` operator to set the reference level. This is
  usually the baseline to use for pairwise comparisons. If the levels of
  `varname` are `c("High", "Mid", "Low")`, you might set this to Low
  with `+ "Low"`
- `intercept`: Use the `*` operator to set the intercept, overwriting
  whatever the default is for the given contrast scheme. For example,
  the intercept (and reference level) for `treatment_code` is usually
  the first level alphabetically, but could be changed. For example,
  `* "Mid"`
- `dropped`: Use the `-` operator to remove some comparisons from the
  contrast matrix. Cannot be used with
  [`set_contrasts()`](https://tsostarics.github.io/contrastable/reference/set_contrasts.md).
  Sometimes used with polynomial contrasts.
- `labels`: Use the `|` operator to set the comparison labels,
  overwriting the defaults for the contrast scheme. For example, if
  doing pairwise comparisons for `varname` using `treatment_code` for
  levels `c("High", "Mid", "Low")` with `High` as the default reference
  level, the default coefficient names will be `varnameMid` and
  `varnameLow`. We can use `| c("Mid-High", "Low-High")` to change these
  in the output to `varnameMid-High` and `varnameLow-High`.

The operators can be used in any order, but `contrast_scheme` always has
to be the first thing after the `~`.

#### set_contrasts

Use this to set contrasts directly to a column, coercing it to a factor
as necessary. Often used as the last step in a wrangling pipeline. The
result should be assigned to a variable. We can set
`print_contrasts = TRUE` to print the contrasts that have been set.
Below we set the contrasts for a binary `gear_type` variable to use
scaled sum coding with `odd` as the reference level while setting the
comparison label to be something informative, which is reflected in the
model summary.

``` r
model_data <-
  mtcars |>
  dplyr::mutate(gear_type = ifelse(gear %% 2 == 0, "even", "odd")) |>
  set_contrasts(gear_type ~ scaled_sum_code + "odd" | c("Odd-Even"),
                print_contrasts = TRUE)
#> Converting to factors: gear_type
#> $gear_type
#>      Odd-Even
#> even  1/2    
#> odd  -1/2

summary(lm(mpg ~ gear_type, data = model_data))
#> 
#> Call:
#> lm(formula = mpg ~ gear_type, data = model_data)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -7.025 -3.127 -1.679  2.398 12.975 
#> 
#> Coefficients:
#>                   Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)        20.9792     0.9111  23.026  < 2e-16 ***
#> gear_typeOdd-Even   7.1083     1.8222   3.901 0.000501 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 4.99 on 30 degrees of freedom
#> Multiple R-squared:  0.3365, Adjusted R-squared:  0.3144 
#> F-statistic: 15.22 on 1 and 30 DF,  p-value: 0.0005009
```

We can set multiple columns at once by listing multiple columns on the
left hand side, separated by `+`

``` r
model_data <-
  mtcars |>
  dplyr::mutate(gear_type = ifelse(gear %% 2 == 0, "even", "odd")) |>
  set_contrasts(gear_type ~ scaled_sum_code + "odd" | c("Odd-Even"),
                print_contrasts = TRUE,
                carb + cyl ~ helmert_code)
#> Converting to factors: gear_type carb cyl
#> $gear_type
#>      Odd-Even
#> even  1/2    
#> odd  -1/2    
#> 
#> $carb
#>   <2   <3   <4   <6   <8  
#> 1 -1/2 -1/3 -1/4 -1/5 -1/6
#> 2  1/2 -1/3 -1/4 -1/5 -1/6
#> 3    0  2/3 -1/4 -1/5 -1/6
#> 4    0    0  3/4 -1/5 -1/6
#> 6    0    0    0  4/5 -1/6
#> 8    0    0    0    0  5/6
#> 
#> $cyl
#>   <6   <8  
#> 4 -1/2 -1/3
#> 6  1/2 -1/3
#> 8    0  2/3

summary(lm(mpg ~ gear_type + carb + cyl, data = model_data))
#> 
#> Call:
#> lm(formula = mpg ~ gear_type + carb + cyl, data = model_data)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -5.5190 -1.6626  0.0115  1.8229  6.3961 
#> 
#> Coefficients:
#>                   Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)       20.76667    0.98358  21.113   <2e-16 ***
#> gear_typeOdd-Even  1.91122    1.94985   0.980   0.3372    
#> carb<2            -0.58488    1.79537  -0.326   0.7475    
#> carb<3            -0.56951    2.54671  -0.224   0.8250    
#> carb<4            -3.14772    1.67694  -1.877   0.0733 .  
#> carb<6             0.07335    3.89232   0.019   0.9851    
#> carb<8            -0.90741    3.61691  -0.251   0.8041    
#> cyl<6             -4.69683    2.14496  -2.190   0.0390 *  
#> cyl<8             -6.08232    2.23460  -2.722   0.0122 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 3.361 on 23 degrees of freedom
#> Multiple R-squared:  0.7693, Adjusted R-squared:  0.689 
#> F-statistic: 9.586 on 8 and 23 DF,  p-value: 9.454e-06
```

We can also use `tidyselect` functionality to target multiple columns.
Note that when doing so, you cannot specify duplicated column names.

``` r
model_data <-
  mtcars |>
  dplyr::mutate(gear_type = ifelse(gear %% 2 == 0, "even", "odd")) |>
  set_contrasts(gear_type ~ scaled_sum_code + "odd" | c("Odd-Even"),
                vs:carb ~ helmert_code,
                print_contrasts = TRUE)
#> Converting to factors: gear_type vs am gear carb
#> $gear_type
#>      Odd-Even
#> even  1/2    
#> odd  -1/2    
#> 
#> $vs
#>   <1  
#> 0 -1/2
#> 1  1/2
#> 
#> $am
#>   <1  
#> 0 -1/2
#> 1  1/2
#> 
#> $gear
#>   <4   <5  
#> 3 -1/2 -1/3
#> 4  1/2 -1/3
#> 5    0  2/3
#> 
#> $carb
#>   <2   <3   <4   <6   <8  
#> 1 -1/2 -1/3 -1/4 -1/5 -1/6
#> 2  1/2 -1/3 -1/4 -1/5 -1/6
#> 3    0  2/3 -1/4 -1/5 -1/6
#> 4    0    0  3/4 -1/5 -1/6
#> 6    0    0    0  4/5 -1/6
#> 8    0    0    0    0  5/6
```

### enlist_contrasts

Used to get a named list of contrast matrices. Useful to pass to the
`contrasts` argument of a modeling function if available.

``` r
model_contrasts <-
  mtcars |>
  dplyr::mutate(gear_type = ifelse(gear %% 2 == 0, "even", "odd")) |>
  enlist_contrasts(gear_type ~ scaled_sum_code + "odd" | c("Odd-Even"))
#> Converting to factors: gear_type

model_contrasts
#> $gear_type
#>      Odd-Even
#> even      0.5
#> odd      -0.5
```

``` r
model_contrasts <-
  mtcars |>
  dplyr::mutate(gear_type = ifelse(gear %% 2 == 0, "even", "odd")) |>
  enlist_contrasts(gear_type ~ scaled_sum_code + "odd" | c("Odd-Even"),
                   carb + cyl ~ sum_code)
#> Converting to factors: gear_type carb cyl

model_contrasts
#> $gear_type
#>      Odd-Even
#> even      0.5
#> odd      -0.5
#> 
#> $carb
#>    2  3  4  6  8
#> 1 -1 -1 -1 -1 -1
#> 2  1  0  0  0  0
#> 3  0  1  0  0  0
#> 4  0  0  1  0  0
#> 6  0  0  0  1  0
#> 8  0  0  0  0  1
#> 
#> $cyl
#>    6  8
#> 4 -1 -1
#> 6  1  0
#> 8  0  1
```

We can also use matrix objects when setting contrasts:

``` r
carb_contrasts <- scaled_sum_code(6)

enlist_contrasts(mtcars,
                 cyl ~ sum_code,
                 carb ~ carb_contrasts)
#> Converting to factors: cyl carb
#> $cyl
#>    6  8
#> 4 -1 -1
#> 6  1  0
#> 8  0  1
#> 
#> $carb
#>            2          3          4          6          8
#> 1 -0.1666667 -0.1666667 -0.1666667 -0.1666667 -0.1666667
#> 2  0.8333333 -0.1666667 -0.1666667 -0.1666667 -0.1666667
#> 3 -0.1666667  0.8333333 -0.1666667 -0.1666667 -0.1666667
#> 4 -0.1666667 -0.1666667  0.8333333 -0.1666667 -0.1666667
#> 6 -0.1666667 -0.1666667 -0.1666667  0.8333333 -0.1666667
#> 8 -0.1666667 -0.1666667 -0.1666667 -0.1666667  0.8333333
```

Note here that the reference level is always the first level in the
factor, which is typically alphanumeric order. For example, `contr.sum`
usually sets the last level as the reference, but we can see that when
using this package’s functions it’s always the first level (for sum
coding, this is the row with all `-1`).

``` r
contr.sum(3) # third row = reference level
#>   [,1] [,2]
#> 1    1    0
#> 2    0    1
#> 3   -1   -1

enlist_contrasts(mtcars, cyl ~ contr.sum) # == sum_code
#> Converting to factors: cyl
#> $cyl
#>    6  8
#> 4 -1 -1
#> 6  1  0
#> 8  0  1
```

This behavior can be suppressed by wrapping the contrast scheme with
[`I()`](https://rdrr.io/r/base/AsIs.html), but will issue a warning:

``` r
enlist_contrasts(mtcars, cyl ~ I(contr.sum)) # == sum_code
#> Converting to factors: cyl
#> Warning in .postprocess_matrix(new_contrasts, code_by, reference_level, : No
#> comparison labels set and as_is=TRUE, contrast labels will be column indices.
#> $cyl
#>    1  2
#> 1  1  0
#> 2  0  1
#> 3 -1 -1
```

### glimpse_contrasts

Used to summarize information about the contrast schemes used. Note that
this is usually used as a 2-step process, as it needs information about
the contrast specifications and it expects that the same contrasts are
set to the dataframe provided. For example, if I try to glimpse the
contrasts for `mtcars` directly, I’ll be warned that the dataframe
columns aren’t actually set to what I specified in the formulas, along
with a code snippet of how to fix this.

``` r
mtcars2 <- 
  dplyr::mutate(mtcars, gear_type = ifelse(gear %% 2 == 0, "even", "odd"))

glimpse_contrasts(mtcars2,
                  gear_type ~ scaled_sum_code + "odd" | c("Odd-Even"),
                  carb + cyl ~ sum_code)
#> Warning: These vars in `mtcars2` are not factors:
#>  - gear_type
#>  - carb
#>  - cyl
#> To fix, be sure to run:
#> mtcars2 <- set_contrasts(mtcars2, 
#>                          gear_type ~ scaled_sum_code + "odd" | c("Odd-Even"),
#>                          carb + cyl ~ sum_code)
#>      factor n  level_names          scheme reference  intercept
#> 1 gear_type 2    even, odd scaled_sum_code       odd grand mean
#> 2      carb 6 1, 2, 3,....        sum_code      <NA> grand mean
#> 3       cyl 3      4, 6, 8        sum_code      <NA> grand mean
```

I can copy-paste this directly and try again:

``` r
mtcars2 <- set_contrasts(mtcars2, 
                         gear_type ~ scaled_sum_code + "odd" | c("Odd-Even"),
                         carb + cyl ~ sum_code) 
#> Converting to factors: gear_type carb cyl

glimpse_contrasts(mtcars2,
                  gear_type ~ scaled_sum_code + "odd" | c("Odd-Even"),
                  carb + cyl ~ sum_code)
#>      factor n  level_names          scheme reference  intercept
#> 1 gear_type 2    even, odd scaled_sum_code       odd grand mean
#> 2      carb 6 1, 2, 3,....        sum_code      <NA> grand mean
#> 3       cyl 3      4, 6, 8        sum_code      <NA> grand mean
```

The observation here is that if I don’t use
[`set_contrasts()`](https://tsostarics.github.io/contrastable/reference/set_contrasts.md)
on my dataset used in my statistical model, the results won’t match the
information in the table from
[`glimpse_contrasts()`](https://tsostarics.github.io/contrastable/reference/glimpse_contrasts.md).
However, this also requires changing the formulas in 2 places if I need
to make any changes. We can make 1 list of contrast formulas that we
pass around to different functions like so:

``` r
my_contrasts <- list(gear_type ~ scaled_sum_code + "odd" | c("Odd-Even"),
                     carb + cyl ~ sum_code)

mtcars2 <- 
  mtcars |> 
  dplyr::mutate(gear_type = ifelse(gear %% 2 == 0, "even", "odd")) |> 
  set_contrasts(my_contrasts)
#> Converting to factors: gear_type carb cyl

glimpse_contrasts(mtcars2, my_contrasts)
#>      factor n  level_names          scheme reference  intercept
#> 1 gear_type 2    even, odd scaled_sum_code       odd grand mean
#> 2      carb 6 1, 2, 3,....        sum_code      <NA> grand mean
#> 3       cyl 3      4, 6, 8        sum_code      <NA> grand mean
```

#### decompose_contrasts

Use this function to extract the contrasts of one column into separate
columns– one for each comparison. This function is particularly helpful
for pedagogical uses to show students how contrasts are represented from
the model’s perspective. Below we see that we’ve added 3 new columns
from decomposing the `gear_type` and `cyl` columns into their respective
comparisons.

``` r
mtcars2 |> 
  decompose_contrasts(~gear_type + cyl) |> 
  head()
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb gear_type
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4      even
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4      even
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1      even
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1       odd
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2       odd
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1       odd
#>                   gear_typeOdd-Even cyl6 cyl8
#> Mazda RX4                       0.5    1    0
#> Mazda RX4 Wag                   0.5    1    0
#> Datsun 710                      0.5   -1   -1
#> Hornet 4 Drive                 -0.5    1    0
#> Hornet Sportabout              -0.5    0    1
#> Valiant                        -0.5    1    0
```

### Contrast functions

Below is a listing of the different contrast coding functions provided
by this package. You would use these in the `contrast_scheme` part of
the formulas. The intercept is described for the default case, but can
be changed as described above using the `*` operator.

- [`treatment_code()`](https://tsostarics.github.io/contrastable/reference/treatment_code.md):
  Pairwise comparisons from a reference level, intercept equals mean of
  the reference level.
- [`scaled_sum_code()`](https://tsostarics.github.io/contrastable/reference/scaled_sum_code.md):
  Pairwise comparisons from a reference level, intercept equals the
  grand mean
- [`sum_code()`](https://tsostarics.github.io/contrastable/reference/sum_code.md):
  Pairwise comparisons from the grand mean for all levels except the
  reference level, intercept equals the grand mean.
- `backwards_difference_code()`: Subtract adjacent levels. For levels A,
  B, C, D (in that order), returns the differences B-A, C-B, and D-C.
  Intercept equals the grand mean.
- `forwards_difference_code()`: Subtract adjacent levels. For levels A,
  B, C, D (in that order), returns the differences A-B, B-C, and C-D.
  Intercept equals the grand mean.
- [`helmert_code()`](https://tsostarics.github.io/contrastable/reference/helmert_code.md):
  Nested comparisons starting from the first level. Intercept equals the
  grand mean.
- [`reverse_helmert_code()`](https://tsostarics.github.io/contrastable/reference/reverse_helmert_code.md):
  Nested comparisons starting from the last level. Intercept equals the
  grand mean.
- [`cumulative_split_code()`](https://tsostarics.github.io/contrastable/reference/cumulative_split_code.md):
  Cumulative grouping of levels. For levels A, B, C, D (in that order),
  returns A-(B+C+D), (A+B)-(C+D), (A+B+C)-D. Intercept equals the grand
  mean.
- [`polynomial_code()`](https://tsostarics.github.io/contrastable/reference/orth_polynomial_code.md):
  Orthogonal polynomial coding, intercept equals the grand mean.
- [`raw_polynomial_code()`](https://tsostarics.github.io/contrastable/reference/raw_polynomial_code.md):
  Raw polynomial coding, intercept equals the grand mean.

You can use any function that returns contrast matrices. Below are some
functions from the `stats` and `MASS` packages that can be used.

- [`stats::contr.treatment()`](https://rdrr.io/r/stats/contrast.html):
  Equivalent to treatment coding
  ([`treatment_code()`](https://tsostarics.github.io/contrastable/reference/treatment_code.md))
- [`stats::contr.SAS()`](https://rdrr.io/r/stats/contrast.html):
  Equivalent to treatment coding, but uses the last level as the
  reference level by default. Note that this difference is neutralized
  due to this package always setting the first level as the reference
  level.
- [`stats::contr.poly`](https://rdrr.io/r/stats/contrast.html):
  Equivalent to polynomial coding
  ([`polynomial_code()`](https://tsostarics.github.io/contrastable/reference/orth_polynomial_code.md))
- [`MASS::contr.sdif`](https://rdrr.io/pkg/MASS/man/contr.sdif.html):
  Equivalent to backwards difference coding
  (`backwards_difference_code()`)
- [`stats::contr.sum`](https://rdrr.io/r/stats/contrast.html):
  Equivalent to sum coding
  ([`sum_code()`](https://tsostarics.github.io/contrastable/reference/sum_code.md))
- [`stats::contr.helmert`](https://rdrr.io/r/stats/contrast.html):
  Provides nested comparisons like helmert coding
  ([`helmert_code()`](https://tsostarics.github.io/contrastable/reference/helmert_code.md)),
  but the matrix is not scaled. This means that the effect estimates are
  off by a scaling factor dependent on the number of levels. This is
  reflected in the default comparison labels this package provides. See
  below.

``` r
enlist_contrasts(mtcars, carb ~ contr.helmert)
#> Converting to factors: carb
#> $carb
#>   (<2)/2 (<3)/3 (<4)/4 (<6)/5 (<8)/6
#> 1     -1     -1     -1     -1     -1
#> 2      1     -1     -1     -1     -1
#> 3      0      2     -1     -1     -1
#> 4      0      0      3     -1     -1
#> 6      0      0      0      4     -1
#> 8      0      0      0      0      5
```

``` r
enlist_contrasts(mtcars, carb ~ helmert_code())
```

    #> Converting to factors: carb
    #> $carb
    #>   <2   <3   <4   <6   <8  
    #> 1 -1/2 -1/3 -1/4 -1/5 -1/6
    #> 2  1/2 -1/3 -1/4 -1/5 -1/6
    #> 3    0  2/3 -1/4 -1/5 -1/6
    #> 4    0    0  3/4 -1/5 -1/6
    #> 6    0    0    0  4/5 -1/6
    #> 8    0    0    0    0  5/6

See
[`vignette("contrasts")`](https://tsostarics.github.io/contrastable/articles/contrasts.md)
for more information.
