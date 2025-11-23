# Get quick summary of contrasts in dataframe

Uses the same syntax as
[`enlist_contrasts()`](https://tsostarics.github.io/contrastable/reference/enlist_contrasts.md)
and
[`set_contrasts()`](https://tsostarics.github.io/contrastable/reference/set_contrasts.md).
Returns a summary table of the contrasts you've set. If you set
`return.list=TRUE` then you can access a list of contrasts in the second
element of the resulting list. The glimpse dataframe is the first
element. `FALSE` will return just the glimpse data frame.

## Usage

``` r
glimpse_contrasts(
  model_data,
  ...,
  return_list = FALSE,
  show_all_factors = TRUE,
  add_namespace = FALSE,
  show_one_level_factors = FALSE,
  minimal = TRUE,
  verbose = getOption("contrastable.verbose"),
  .droplevels = getOption("contrastable.droplevels")
)
```

## Arguments

- model_data:

  Data to be passed to a model fitting function

- ...:

  Series of formulas

- return_list:

  Logical, defaults to FALSE, whether the output of enlist_contrasts
  should be returned

- show_all_factors:

  Logical, defaults to TRUE, whether the factors not explicitly set with
  formulas should be included

- add_namespace:

  Logical, defaults to FALSE, whether to append the namespace of the
  contrast scheme to the scheme name

- show_one_level_factors:

  Logical, should factors with only one level be included in the output?
  Default is FALSE to omit

- minimal:

  Logical, default TRUE, whether to omit the orthogonal, centered,
  dropped_trends, and explicitly_set columns from the output table

- verbose:

  Logical, defaults to TRUE, whether messages should be printed

- .droplevels:

  Logical, defaults to TRUE, whether missing levels from existing factor
  columns should be dropped before applying new contrasts. See
  `Options("contrastable.droplevels")`

## Value

A dataframe if return.list is FALSE, a list with a dataframe and list of
named contrasts if TRUE.

## Details

Generally, `glimpse_contrasts` will give warnings about mismatches
between the specified contrasts and what's actually set on the factors
in a dataframe. The warnings will typically tell you how to resolve
these mismatches. See the `contrasts` and `warnings` vignettes for more
information.

## See also

[`enlist_contrasts()`](https://tsostarics.github.io/contrastable/reference/enlist_contrasts.md)
[`set_contrasts()`](https://tsostarics.github.io/contrastable/reference/set_contrasts.md)

## Examples

``` r
my_contrasts <- list(cyl ~ sum_code, carb ~ helmert_code)
my_data <- set_contrasts(mtcars, my_contrasts, verbose = FALSE)
my_data$gear <- factor(my_data$gear) # Make gear a factor manually

# View information about contrasts; gear will use default for unordered
glimpse_contrasts(my_data, my_contrasts)
#>   factor n  level_names          scheme reference  intercept
#> 1    cyl 3      4, 6, 8        sum_code      <NA> grand mean
#> 2   carb 6 1, 2, 3,....    helmert_code      <NA> grand mean
#> 3   gear 3      3, 4, 5 contr.treatment         3    mean(3)
```
