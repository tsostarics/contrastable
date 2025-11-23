# Get reference levels from a (possibly set) list of contrasts

Given a list of contrast matrices, if the contrast matrices were
explicitly generated via formulas and they have already had their
parameters parsed, use the parameters to look up the reference level for
each contrast matrix. If the parameters have not been computed (usually
because the contrasts are using the defaults) then look up the reference
level manually.

## Usage

``` r
.get_reference_levels(contrast_list, list_params = NULL, formulas = NULL)
```

## Arguments

- contrast_list:

  List of contrasts, does not need to be named

- list_params:

  Optional list of parameters, see
  [`.make_parameters()`](https://tsostarics.github.io/contrastable/reference/dot-make_parameters.md),
  if NULL, then the reference level is determined from the contrast
  matrix directly.

- formulas:

  Optional list of formulas, needed if `params` are passed. Used to get
  the correct environment for evaluating expressions in `params`. If
  NULL, then the reference level is determiend from the contrast matrix
  directly.

## Value

Character vector of reference levels. If a contrast matrix is not
specified for row names, the character value will denote the integer
index of the row for the reference level (usually 1).
