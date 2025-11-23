# Reset comparison labels of matrix to defaults

Given a contrast matrix and a coding function used to generate it, check
whether we have default labels implemented. If so, use them if the
matrix doesn't have unique ones. If no function is provided, just use
what the matrix has or use numeric indices

## Usage

``` r
.reset_comparison_labels(contr_mat, coding_fx = NULL)
```

## Arguments

- contr_mat:

  Contrast matrix

- coding_fx:

  Function name as a string

## Value

Matrix with column names changed if necessary
