# Pass arguments to contrast code

Processes a formula and any arguments for the contrast matrix and sets
up the use_contrasts call

## Usage

``` r
.process_contrasts(model_data, raw_formula, omit_drop)
```

## Arguments

- model_data:

  Data frame with factor column

- raw_formula:

  Raw formula

- omit_drop:

  Logical, set to TRUE when set_contrasts is used by appending an
  attribute to the formula list, otherwise is FALSE (as in
  enlist_contrasts)

## Value

A contrast matrix
