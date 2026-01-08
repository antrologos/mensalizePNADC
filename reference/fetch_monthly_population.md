# Fetch Monthly Population from SIDRA

Downloads population estimates from IBGE SIDRA API (table 6022) and
transforms from moving-quarter to exact monthly values.

## Usage

``` r
fetch_monthly_population(
  start_yyyymm = NULL,
  end_yyyymm = NULL,
  verbose = TRUE
)
```

## Arguments

- start_yyyymm:

  Integer. First month to include (YYYYMM format). If NULL, returns all
  available months.

- end_yyyymm:

  Integer. Last month to include (YYYYMM format). If NULL, returns all
  available months.

- verbose:

  Logical. Print progress messages? Default TRUE.

## Value

A data.table with columns:

- `ref_month_yyyymm`: Integer in YYYYMM format

- `m_populacao`: Monthly population in thousands

## Details

SIDRA table 6022 provides moving-quarter population estimates. Each
value represents the 3-month average centered on the middle month. For
example, the value for code 201203 (quarter ending March 2012)
represents the population for February 2012.

This function:

1.  Fetches raw moving-quarter data from SIDRA

2.  Transforms to exact monthly values by aligning with middle months

3.  Extrapolates boundary months (first and last) using quadratic
    regression

The extrapolation follows the methodology in Hecksher (2024), using
quadratic regression on population differences to estimate the first
month (Jan 2012) and the most recent month.

## Dependencies

This function requires the `sidrar` package for API access. Install
with: `install.packages("sidrar")`

## See also

[`mensalizePNADC`](https://antrologos.github.io/mensalizePNADC/reference/mensalizePNADC.md)
which uses this function when `compute_weights = TRUE`

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch all available months
pop <- fetch_monthly_population()

# Fetch specific date range
pop <- fetch_monthly_population(201301, 201912)
} # }
```
