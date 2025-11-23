# Split contrast function using parens

If something like `set_contrasts(df, var ~ sum_code())` then what's
extracted from the formula isn't the symbol for the function `sum_code`
but a language object. This can be converted to a list to extract the
function symbol and then any arguments provided in the parens. The
latter needs to be evaluated in the original environment, for example if
`set_contrasts(df, var ~ sum_code(scores=c(.1,.5,.6))` is called, the
scores list would not evaluate the `c(...)` call, yielding an error that
the scores argument isn't the right length (because it would be length
1, not 3).

## Usage

``` r
.split_if_language(params, var_envir)
```

## Arguments

- params:

  Parameters extracted from formula parsing

- var_envir:

  Environment to evaluate expressions in

## Value

Parameter list with `code_by` set to the correct symbol & an additional
list entry for other arguments, which will be empty if no arguments are
provided.
