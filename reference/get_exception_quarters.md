# Get Exception Quarters

Returns the list of quarters where IBGE used non-standard technical
break timing. In these quarters, the first reference week of a month
only needs 3 days (instead of the usual 4) within that month.

## Usage

``` r
get_exception_quarters()
```

## Value

Character vector of exception quarters in "YYYYtQ" format

## Details

The exception quarters are documented in IBGE's methodology notes:

- 2016t3: September 25 - October 1 technical break

- 2016t4: December 25-31 technical break

- 2017t2: June 25 - July 1 technical break

- 2022t3: September 25 - October 1 technical break

- 2023t2: June 25 - July 1 technical break

## Examples

``` r
get_exception_quarters()
#> [1] "2016t3" "2016t4" "2017t2" "2022t3" "2023t2"
# [1] "2016t3" "2016t4" "2017t2" "2022t3" "2023t2"
```
