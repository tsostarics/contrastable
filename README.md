
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

## Citation

To cite contrastable in publications, please use

Sostarics, T. (2024). contrastable: Contrast Coding Utilities in R. R
package version 0.1.0.

A BibTeX entry for LaTeX users is

      @Manual{,
        author = {Thomas Sostarics},
        title = {{contrastable}: Contrast Coding Utilities in {R}},
        year = {2024},
        url = {https://github.com/tsostarics/contrastable},
      }

In-text citations should reference the package and ideally which
contrast functions were used to avoid ambiguity.

BAD: “We sum code our variables (Sostarics 2024)”

GOOD: “We use the `sum_code()` function from the contrastable package
(Sostarics 2024) for our variables.”

Other examples:

- Contrasts were set using the `scaled_sum_code()` function from the
  contrastable package (Sostarics 2024).
- We use the contrastable package (Sostarics 2024) for contrast coding
  our categorical variables; details of the contrasts for each model are
  provided in the appendix. (be sure to do the latter!)
- Below are the contrast matrices returned by the `helmert_code()` and
  `scaled_sum_code()` functions from the contrastable R package
  (Sostarics 2024)
- We use the `set_contrasts()` and `sum_code()` functions from the
  contrastable package (Sostarics 2024) to sum code (+1/-1) our
  variables

I also recommend writing out, potentially in a footnote, what the
comparisons are.

GOOD: “We use the contrastable package’s `sum_code()` function (+1/-1,
Sostarics 2024).”

BETTER: “We use the contrastable package’s `sum_code()` function (+1/-1,
Sostarics 2024). This contrast scheme encodes differences between each
comparison level and the grand mean.”

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
                  add_namespace = TRUE,
                  all.factors = TRUE) |> 
  knitr::kable()
```

| factor |   n | level_names      | scheme                        | reference | intercept  | orthogonal | centered | dropped_trends | explicitly_set |
|:-------|----:|:-----------------|:------------------------------|:----------|:-----------|:-----------|:---------|:---------------|:---------------|
| cyl    |   3 | 4, 6, 8          | contrastable::scaled_sum_code | 6         | grand mean | FALSE      | TRUE     | NA             | TRUE           |
| carb   |   6 | 1, 2, 3, 4, 6, 8 | contrastable::helmert_code    | NA        | grand mean | TRUE       | TRUE     | NA             | TRUE           |
| vs     |   2 | 0, 1             | contrastable::sum_code        | 1         | grand mean | NA         | TRUE     | NA             | TRUE           |
| gear   |   3 | 3, 4, 5          | stats::contr.poly             | NA        | grand mean | TRUE       | TRUE     | NA             | FALSE          |

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
#>     >1         >2    >3   >4         >6
#> 1 -0.5 -0.3333333 -0.25 -0.2 -0.1666667
#> 2  0.5 -0.3333333 -0.25 -0.2 -0.1666667
#> 3  0.0  0.6666667 -0.25 -0.2 -0.1666667
#> 4  0.0  0.0000000  0.75 -0.2 -0.1666667
#> 6  0.0  0.0000000  0.00  0.8 -0.1666667
#> 8  0.0  0.0000000  0.00  0.0  0.8333333
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
#> 1 -1/2 -1/3 -1/4 -1/5 -1/6
#> 2  1/2 -1/3 -1/4 -1/5 -1/6
#> 3    0  2/3 -1/4 -1/5 -1/6
#> 4    0    0  3/4 -1/5 -1/6
#> 6    0    0    0  4/5 -1/6
#> 8    0    0    0    0  5/6
```

Functions will give various messages and warnings such as:

- When non-factor columns are coerced to factors
- When ordered factors are set to non-polynomial contrasts
- When a factor with only one level is detected
- When a factor is not explicitly set
- and more

Detailed usage is available in the `contrasts` vignette with
`vignette('contrasts', 'contrastable')`.
