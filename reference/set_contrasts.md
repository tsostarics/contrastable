# Set contrasts to factor columns in dataframe

Uses the same syntax as
[`enlist_contrasts()`](https://tsostarics.github.io/contrastable/reference/enlist_contrasts.md),
but returns the dataframe with the new contrasts applied. Use this when
your model function doesnt have a contrasts argument and you want to
avoid writing `contrasts<-` multiple times. See
[`enlist_contrasts()`](https://tsostarics.github.io/contrastable/reference/enlist_contrasts.md)
for details about the package-specific syntax.

## Usage

``` r
set_contrasts(
  model_data,
  ...,
  verbose = getOption("contrastable.verbose"),
  .droplevels = getOption("contrastable.droplevels"),
  print_contrasts = FALSE
)
```

## Arguments

- model_data:

  Data frame you intend on passing to your model

- ...:

  A series of 2 sided formulas with factor name on the left hand side
  and desired contrast scheme on the right hand side. The reference
  level can be set with `+`, the intercept can be overwritten with `*`,
  comparison labels can be set using `|`, and trends for polynomial
  coding can be removed using `-`.

- verbose:

  Logical, defaults to TRUE, whether messages should be printed. See
  `Options("contrastable.verbose")`

- .droplevels:

  Logical, defaults to TRUE, whether missing levels from existing factor
  columns should be dropped before applying new contrasts. See
  `Options("contrastable.droplevels")`

- print_contrasts:

  Logical, default FALSE, whether to print the contrasts set for each
  factor. Fractions are displayed using
  [`MASS::fractions()`](https://rdrr.io/pkg/MASS/man/fractions.html)

## Value

The `model_data` dataframe, but with updated contrasts.

## Details

[`enlist_contrasts()`](https://tsostarics.github.io/contrastable/reference/enlist_contrasts.md),
`set_contrasts()`, and
[`glimpse_contrasts()`](https://tsostarics.github.io/contrastable/reference/glimpse_contrasts.md)
use special syntax to set contrasts for multiple factors. The syntax
consists of two-sided formulas with the desired factor column on the
left hand side and the contrast specification on the right hand side.
For example, `varname ~ scaled_sum_code`. Many contrasts support
additional kinds of contrast manipulations using overloaded operators:

- `+ X`: Set the reference level to the level named X. Only supported
  for schemes that have a singular reference level such as
  [`sum_code()`](https://tsostarics.github.io/contrastable/reference/sum_code.md),
  [`scaled_sum_code()`](https://tsostarics.github.io/contrastable/reference/scaled_sum_code.md),
  [`treatment_code()`](https://tsostarics.github.io/contrastable/reference/treatment_code.md),
  [`stats::contr.treatment()`](https://rdrr.io/r/stats/contrast.html),
  [`stats::contr.sum()`](https://rdrr.io/r/stats/contrast.html),
  [`stats::contr.SAS()`](https://rdrr.io/r/stats/contrast.html). Ignored
  for schemes like
  [`helmert_code()`](https://tsostarics.github.io/contrastable/reference/helmert_code.md).

- `* X`: Overwrite the intercept to the mean of the level named X

- `- A:B`: For polynomial coding schemes only, drop comparisons A
  through B.

- `| c(...)`: Change the comparison labels for the contrast matrix to
  the character vector `c(...)` of length `n-1`. These labels will
  appear in the output/summary of a statistical model. Note that for
  `brms::brm`, instances of `-` (a minus sign) are replaced with `M`.

You can also specify multiple variables on the left hand side of a
formula using tidyselect helpers. See examples for more information.

Typically model functions like lm will have a contrasts argument where
you can set the contrasts at model run time, rather than having to
manually change the contrasts on the underlying factor columns in your
data. This function will return such a named list of contrast matrices
to pass to these functions. Note that this function should not be used
within a modeling function call, e.g.,
`lm(y~x, data = model_data, contrasts = enlist_contrasts(model_data, x~sum_code))`.
Often, this will call `enlist_contrasts` twice, rather than just once.

For some model fitting functions, like `brms::brm`, there is no
contrasts argument. For such cases, use `set_contrasts()` to set
contrasts directly to the factors in a dataframe.

One good way to use
[`enlist_contrasts()`](https://tsostarics.github.io/contrastable/reference/enlist_contrasts.md)
is in conjunction with
[`MASS::fractions()`](https://rdrr.io/pkg/MASS/man/fractions.html) to
create a list of matrices that can be printed to explicitly show the
entire contrast matrices you're using for your models. This can be
especially helpful for supplementary materials in an academic paper.

Sometimes when using orthogonal polynomial contrasts from
[`stats::contr.poly()`](https://rdrr.io/r/stats/contrast.html) people
will drop higher level polynomials for parsimony. Note however that
these do capture some amount of variation, so even though they're
orthogonal contrasts the lower level polynomials will have their
estimates changed. Moreover, you cannot reduce a contrast matrix to a
matrix smaller than size n\*n-1 in the dataframe you pass to a model
fitting function itself, as R will try to fill in the gaps with
something else. If you want to drop contrasts you'll need to use
something like `enlist_contrasts(df, x ~ contr.poly - 3:5)` and pass
this to the `contrasts` argument in the model fitting function.

## See also

[`enlist_contrasts()`](https://tsostarics.github.io/contrastable/reference/enlist_contrasts.md)
[`glimpse_contrasts()`](https://tsostarics.github.io/contrastable/reference/glimpse_contrasts.md)

## Examples

``` r
head(
   set_contrasts(mtcars, carb + cyl ~ helmert_code, print_contrasts = TRUE)
)
#> Converting to factors: carb cyl
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
#> 
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```
