# Identify Reference Month in PNADC Data

PNADC is a quarterly survey, but each interview actually refers to a
specific week within the quarter. This function identifies which month
that week belongs to, enabling monthly (instead of quarterly) time
series analysis.

The algorithm uses:

- IBGE's reference week timing rules (first Saturday with sufficient
  days in month)

- Respondent birthdates to constrain possible interview dates

- UPA-panel level aggregation (everyone in same sampling unit
  interviewed together)

## Usage

``` r
identify_reference_month(data, exception_quarters = NULL)
```

## Arguments

- data:

  A data.frame or data.table with PNADC microdata. Required columns:

  - `Ano`: Survey year

  - `Trimestre`: Quarter (1-4)

  - `UPA`: Primary Sampling Unit

  - `V1014`: Panel identifier

  - `V2008`: Birth day (1-31)

  - `V20081`: Birth month (1-12)

  - `V20082`: Birth year

  - `V2009`: Age

  Optional but recommended for complete crosswalk:

  - `V1008`: Household sequence within UPA

  - `V2003`: Person sequence within household

- exception_quarters:

  Character vector of quarters with relaxed timing rules, in format
  "YYYYtQ" (e.g., "2016t3"). If NULL (default), uses the built-in list
  of known exceptions: 2016t3, 2016t4, 2017t2, 2022t3, 2023t2.

## Value

A data.table with the original key columns plus:

- `ref_month`: Reference month as Date (first day of month, e.g.,
  "2023-01-01")

- `ref_month_in_quarter`: Position in quarter (1, 2, 3) or NA if
  indeterminate

- `ref_month_yyyymm`: Integer YYYYMM format (e.g., 202301)

## Details

Determines which month within each quarter corresponds to each survey
observation based on IBGE's "Parada Tecnica" (technical break) rules.

The determination rate (proportion of observations with identified
reference month) is typically 85-90%. Observations that cannot be
determined are marked with NA.

The function processes data in the following steps:

1.  Calculate first valid interview Saturday for each month in quarter

2.  For each person, calculate possible interview date range based on
    birthday constraints

3.  Convert date ranges to month-in-quarter positions

4.  Aggregate to UPA-panel level (all persons must agree)

5.  Handle exception quarters with relaxed timing rules

## See also

[`get_exception_quarters`](https://antrologos.github.io/mensalizePNADC/reference/get_exception_quarters.md)
for the list of exception quarters

## Examples

``` r
if (FALSE) { # \dontrun{
# Identify reference months
result <- identify_reference_month(pnadc_data)

# Check determination rate
result[, .(
  total = .N,
  determined = sum(!is.na(ref_month_in_quarter)),
  rate = mean(!is.na(ref_month_in_quarter))
), by = .(Ano, Trimestre)]
} # }
```
