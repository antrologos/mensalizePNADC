# Validate PNADC Input Data

Checks that input data has required columns for the specified
processing.

## Usage

``` r
validate_pnadc(data, check_weights = FALSE, stop_on_error = TRUE)
```

## Arguments

- data:

  A data.frame or data.table with PNADC microdata

- check_weights:

  Logical. If TRUE, also check for weight-related variables.

- stop_on_error:

  Logical. If TRUE, stops with an error. If FALSE, returns a validation
  report list.

## Value

If stop_on_error is TRUE, returns invisibly if valid or stops with
error. If FALSE, returns a list with validation results.

## Examples

``` r
if (FALSE) { # \dontrun{
validate_pnadc(my_data)
validate_pnadc(my_data, check_weights = TRUE)
} # }
```
