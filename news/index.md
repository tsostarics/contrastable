# Changelog

## contrastable 1.1.0

### New features

- New
  [`print_contrasts()`](https://tsostarics.github.io/contrastable/reference/print_contrasts.md)
  prints contrasts for the factor columns in a given dataframe
- New option `contrastable.droplevels` controls whether missing levels
  are automatically dropped

### Minor improvements and fixes

- Example for
  [`cumulative_split_code()`](https://tsostarics.github.io/contrastable/reference/cumulative_split_code.md)
  now shows correct values
- [`set_contrasts()`](https://tsostarics.github.io/contrastable/reference/set_contrasts.md)
  and
  [`enlist_contrasts()`](https://tsostarics.github.io/contrastable/reference/enlist_contrasts.md)
  will now drop missing levels before applying contrasts to factor
  variables by default

### Documentation

- Added get started vignette (vignette name: “contrastable”)
- Created pkgdown website, now linked in DESCRIPTION
- Citation and DOI updated following CRAN acceptance
- Docs for various functions tweaked
- Adjusted warning when no variables with more than 1 level are provided
- Depends R\>=4.1 added to DESCRIPTION

## contrastable 1.0.2

CRAN release: 2024-10-01

Accepted on CRAN: <https://CRAN.R-project.org/package=contrastable>.

## contrastable 1.0.1

## contrastable 1.0.0

- Initial CRAN submission.
