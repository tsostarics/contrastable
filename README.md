
<!-- README.md is generated from README.Rmd. Please edit that file -->

# contrastable

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/tsostarics/contrastable/branch/main/graph/badge.svg?token=PW2NOWO8NE)](https://codecov.io/gh/tsostarics/contrastable?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/tsostarics/contrastable/workflows/R-CMD-check/badge.svg)](https://github.com/tsostarics/contrastable/actions)
<!-- badges: end -->

This package provides utilities to set different common contrast coding
schemes for use with regression models.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tsostarics/contrastable", build_vignettes = TRUE)
```

# Example

Here is a simple example showing how to set particular factors to a
specific contrast scheme.

``` r
library(contrastable)
my_data <- mtcars
my_data$gear <- ordered(my_data$gear) # Set as ordered factor in dataframe
```

We can use `glimpse_contrasts` to get information about the factors and
diagnostics about the scheme we have set.

``` r
# Specify the contrast schemes we want, factor conversion done automatically
# Set reference level with + and intercept with *
contrast_schemes <- list(cyl  ~ scaled_sum_code + 6,
                         carb ~ helmert_code,
                         vs   ~ sum_code + 1)

# Get information about our contrasts, even those we didn't explicitly set
glimpse_contrasts(my_data, 
                  contrast_schemes, 
                  clean.schemes = TRUE,
                  all.factors = TRUE) |> 
  knitr::kable()
#> Converting to factors: cyl carb vs
#> Expect contr.treatment or contr.poly for unset factors: gear
```

| factor | n_levels | level_names      | scheme          | reference | intercept  | orthogonal | centered | dropped_trends | explicitly_set |
|:-------|---------:|:-----------------|:----------------|:----------|:-----------|:-----------|:---------|:---------------|:---------------|
| cyl    |        3 | 4, 6, 8          | scaled_sum      | 6         | grand mean | FALSE      | TRUE     | NA             | TRUE           |
| carb   |        6 | 1, 2, 3, 4, 6, 8 | helmert         | 8         | grand mean | TRUE       | TRUE     | NA             | TRUE           |
| vs     |        2 | 0, 1             | sum             | 1         | grand mean | FALSE      | TRUE     | NA             | TRUE           |
| gear   |        3 | 3, 4, 5          | orth_polynomial | NA        | grand mean | TRUE       | TRUE     | NA             | FALSE          |

`enlist_contrasts` can be used to generate a named list of contrasts
that can be used in the `contrasts` argument of various modeling
functions.

``` r
# Get a list of the contrasts we've explicitly set
enlist_contrasts(mtcars, contrast_schemes)
#> Converting to factors: cyl carb vs
#> $cyl
#>            4          8
#> 4  0.6666667 -0.3333333
#> 6 -0.3333333 -0.3333333
#> 8 -0.3333333  0.6666667
#> 
#> $carb
#>           >1   >2    >3         >4   >6
#> 1  0.8333333  0.0  0.00  0.0000000  0.0
#> 2 -0.1666667  0.8  0.00  0.0000000  0.0
#> 3 -0.1666667 -0.2  0.75  0.0000000  0.0
#> 4 -0.1666667 -0.2 -0.25  0.6666667  0.0
#> 6 -0.1666667 -0.2 -0.25 -0.3333333  0.5
#> 8 -0.1666667 -0.2 -0.25 -0.3333333 -0.5
#> 
#> $vs
#>    0
#> 0  1
#> 1 -1
```

`set_contrasts` can be used to set the contrasts onto the dataframe
itself, which is needed when a modeling function lacks a `contrasts`
argument.

``` r
# Set contrasts to dataframe itself
my_data <- set_contrasts(my_data, contrast_schemes)
#> Converting to factors: cyl carb vs
#> Expect contr.treatment or contr.poly for unset factors: gear
MASS::fractions(contrasts(my_data$carb))
#>   >1   >2   >3   >4   >6  
#> 1  5/6    0    0    0    0
#> 2 -1/6  4/5    0    0    0
#> 3 -1/6 -1/5  3/4    0    0
#> 4 -1/6 -1/5 -1/4  2/3    0
#> 6 -1/6 -1/5 -1/4 -1/3  1/2
#> 8 -1/6 -1/5 -1/4 -1/3 -1/2
```

Functions will give various messages and warnings such as:

-   When non-factor columns are coerced to factors
-   When ordered factors are set to non-polynomial contrasts
-   When a factor with only one level is detected
-   When a factor is not explicitly set
-   and more

Detailed usage is available in the `contrasts` vignette with
`vignette('contrasts', 'contrastable')`.
