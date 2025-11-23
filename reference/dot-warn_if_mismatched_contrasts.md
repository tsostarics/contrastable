# Diagnose glimpse issues and send warnings

[`glimpse_contrasts()`](https://tsostarics.github.io/contrastable/reference/glimpse_contrasts.md)
does not modify the dataframe passed to it, which can result in
mismatches between the data the user will use and the glimpse
information presented. This runs many diagnostics to inform the use of
such mismatches and provides suggestions on how to fix the issue.

## Usage

``` r
.warn_if_mismatched_contrasts(
  model_data,
  contrast_list,
  model_data_name,
  dots_names,
  formulas
)
```

## Arguments

- model_data:

  Data user passed to
  [`glimpse_contrasts()`](https://tsostarics.github.io/contrastable/reference/glimpse_contrasts.md)

- contrast_list:

  List of contrasts created by
  [`enlist_contrasts()`](https://tsostarics.github.io/contrastable/reference/enlist_contrasts.md)

- model_data_name:

  Name of the dataframe passed to the user, will be truncated if it's a
  long expression that has (likely) been piped

- dots_names:

  Usually "...", in this case, the `...` filled in by the user (ie
  contrast formulas) will need to be expanded in the suggested fixes

- formulas:

  Formulas passed by the user

## Value

Nothing, issues warnings to the user.
