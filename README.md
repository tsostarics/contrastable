
<!-- README.md is generated from README.Rmd. Please edit that file -->

# contrastable

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/tsostarics/contrastable/branch/main/graph/badge.svg?token=PW2NOWO8NE)](https://codecov.io/gh/tsostarics/contrastable?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/tsostarics/contrastable/workflows/R-CMD-check/badge.svg)](https://github.com/tsostarics/contrastable/actions)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11869428.svg)](https://doi.org/10.5281/zenodo.11869428)

<!-- badges: end -->

This package provides utilities to set different common contrast coding
schemes for use with regression models. Detailed usage is available in
the `contrasts` vignette with `vignette('contrasts', 'contrastable')`.

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
package version 0.2.1.

A BibTeX entry for LaTeX users is

    @Manual{,
    author = {Thomas Sostarics},
    title = {{contrastable}: Contrast Coding Utilities in {R}},
    year = {2024},
    note = {R package version 0.2.1},
    url = {https://github.com/tsostarics/contrastable},
    doi = {10.5281/zenodo.11869427},
    }

In-text citations should reference the package and ideally which
contrast functions were used to avoid ambiguity.

Bad: “We sum code our variables (Sostarics 2024)”

Good: “We use the `sum_code()` function from the contrastable package
(Sostarics 2024) for our variables.”

Also good: “We sum code our variable (reference level -1, comparisons
+1). All contrasts are set using the contrastable package (Sostarics
2024).” (point here is to remove ambiguity of “sum code” using the
specific numbers)

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

Good: “We use the contrastable package’s `sum_code()` function (+1/-1,
Sostarics 2024).”

Better: “We use the contrastable package’s `sum_code()` function (+1/-1,
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
contrast_schemes <- list(
  cyl ~ scaled_sum_code + 6,
  carb ~ helmert_code,
  vs ~ treatment_code + 1
)

# Get information about our contrasts, even those we didn't explicitly set
# (gear is ordered, and so uses contr.poly by default)
glimpse_contrasts(my_data,
                  contrast_schemes,
                  add_namespace = TRUE,
                  show_all_factors = TRUE
) |>
  knitr::kable()
```

| factor |   n | level_names      | scheme                        | reference | intercept  |
|:-------|----:|:-----------------|:------------------------------|:----------|:-----------|
| cyl    |   3 | 4, 6, 8          | contrastable::scaled_sum_code | 6         | grand mean |
| carb   |   6 | 1, 2, 3, 4, 6, 8 | contrastable::helmert_code    | NA        | grand mean |
| vs     |   2 | 0, 1             | contrastable::treatment_code  | 1         | mean(1)    |
| gear   |   3 | 3, 4, 5          | stats::contr.poly             | NA        | grand mean |

`enlist_contrasts` can be used to generate a named list of contrasts
that can be used in the `contrasts` argument of various modeling
functions.

``` r
# Get a list of the contrasts we've explicitly set
enlist_contrasts(mtcars, contrast_schemes)
```

    #> $cyl
    #>   4    8   
    #> 4  2/3 -1/3
    #> 6 -1/3 -1/3
    #> 8 -1/3  2/3
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
    #> $vs
    #>   0
    #> 0 1
    #> 1 0

`set_contrasts` can be used to set the contrasts onto the dataframe
itself, which is needed when a modeling function lacks a `contrasts`
argument.

``` r
# Set contrasts to dataframe itself
my_data <- set_contrasts(my_data, contrast_schemes)
#> Converting to factors: cyl carb vs
#> Expect contr.treatment or contr.poly for unset factors: gear
MASS::fractions(contrasts(my_data$carb))
#>   <2   <3   <4   <6   <8  
#> 1 -1/2 -1/3 -1/4 -1/5 -1/6
#> 2  1/2 -1/3 -1/4 -1/5 -1/6
#> 3    0  2/3 -1/4 -1/5 -1/6
#> 4    0    0  3/4 -1/5 -1/6
#> 6    0    0    0  4/5 -1/6
#> 8    0    0    0    0  5/6
```

You can also set multiple contrasts at once using `{tidyselect}`
functionality.

``` r
my_data2 <- 
  data.frame(a = gl(2,10),
             b = gl(5,2, ordered = TRUE),
             c = gl(5,2),
             d = 1:10,
             e = 11:20)

enlist_contrasts(my_data2,
                 where(is.ordered) ~ polynomial_code,
                 where(is.unordered) ~ helmert_code,
                 d + e ~ sum_code)
#> $b
#>              .L         .Q            .C         ^4
#> 1 -6.324555e-01  0.5345225 -3.162278e-01  0.1195229
#> 2 -3.162278e-01 -0.2672612  6.324555e-01 -0.4780914
#> 3 -3.510833e-17 -0.5345225  1.755417e-16  0.7171372
#> 4  3.162278e-01 -0.2672612 -6.324555e-01 -0.4780914
#> 5  6.324555e-01  0.5345225  3.162278e-01  0.1195229
#> 
#> $a
#>      2
#> 1 -0.5
#> 2  0.5
#> 
#> $c
#>     <2         <3    <4   <5
#> 1 -0.5 -0.3333333 -0.25 -0.2
#> 2  0.5 -0.3333333 -0.25 -0.2
#> 3  0.0  0.6666667 -0.25 -0.2
#> 4  0.0  0.0000000  0.75 -0.2
#> 5  0.0  0.0000000  0.00  0.8
#> 
#> $d
#>     2  3  4  5  6  7  8  9 10
#> 1  -1 -1 -1 -1 -1 -1 -1 -1 -1
#> 2   1  0  0  0  0  0  0  0  0
#> 3   0  1  0  0  0  0  0  0  0
#> 4   0  0  1  0  0  0  0  0  0
#> 5   0  0  0  1  0  0  0  0  0
#> 6   0  0  0  0  1  0  0  0  0
#> 7   0  0  0  0  0  1  0  0  0
#> 8   0  0  0  0  0  0  1  0  0
#> 9   0  0  0  0  0  0  0  1  0
#> 10  0  0  0  0  0  0  0  0  1
#> 
#> $e
#>    12 13 14 15 16 17 18 19 20
#> 11 -1 -1 -1 -1 -1 -1 -1 -1 -1
#> 12  1  0  0  0  0  0  0  0  0
#> 13  0  1  0  0  0  0  0  0  0
#> 14  0  0  1  0  0  0  0  0  0
#> 15  0  0  0  1  0  0  0  0  0
#> 16  0  0  0  0  1  0  0  0  0
#> 17  0  0  0  0  0  1  0  0  0
#> 18  0  0  0  0  0  0  1  0  0
#> 19  0  0  0  0  0  0  0  1  0
#> 20  0  0  0  0  0  0  0  0  1
```

The functions in this package aim to be helpful when potential mistakes
are made and transparent when things happen behind the scenes (e.g.,
automatic factor coercion). You can check out descriptions of various
messages and warnings in the `warnings` vignette with
`vignette('warnings', 'contrastable')`.
