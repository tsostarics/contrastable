# Check if node is a contrast-manipulation operator

Some symbols are reserved for the special syntax implemented by the
package. This function checks if a given node (i.e., a call) is one or
more of `+ - * |`

## Usage

``` r
.is_reserved_operator(node, check_sym = NULL)
```

## Arguments

- node:

  List element

- check_sym:

  If NULL (default), check all reserved operators. Otherwise, a string
  that will be converted to a symbol.

## Value

`TRUE` if `node` is identical to a reserved operator, otherwise `FALSE`
