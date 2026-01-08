# Mensalize PNADC Quarterly Survey Data

This function processes stacked quarterly PNADC microdata to:

1.  Identify which month within each quarter each observation refers to

2.  Optionally compute monthly survey weights (if
    `compute_weights = TRUE`)

The output is a crosswalk table that can be joined with original
(unstacked) PNADC data files to add monthly time information.

## Usage

``` r
mensalizePNADC(
  data,
  compute_weights = FALSE,
  output = c("crosswalk", "microdata", "aggregates"),
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame or data.table with stacked quarterly PNADC microdata.

  For reference month identification only (`compute_weights = FALSE`),
  minimum required columns are:

  - `Ano`, `Trimestre`: Year and quarter

  - `UPA`, `V1014`: Primary sampling unit and panel

  - `V2008`, `V20081`, `V20082`: Birth day, month, year

  - `V2009`: Age

  For monthly weight computation (`compute_weights = TRUE`), additional
  columns are required. See Details.

- compute_weights:

  Logical. If TRUE, compute monthly survey weights in addition to
  identifying reference months. Default is FALSE. Requires the `sidrar`
  package to fetch population data from IBGE SIDRA API.

- output:

  Character. What to return:

  - `"crosswalk"` (default): Minimal crosswalk for joining

  - `"microdata"`: Full microdata with all computed columns

  - `"aggregates"`: Monthly aggregated indicators

- verbose:

  Logical. Print progress messages? Default TRUE.

## Value

Depends on `output` parameter:

If `output = "crosswalk"` (default): A data.table with join keys and new
time variables:

- Join keys: `Ano`, `Trimestre`, `UPA`, `V1008`, `V1014`, `V2003`

- `ref_month`: Reference month as Date

- `ref_month_in_quarter`: Position in quarter (1, 2, 3) or NA

- `ref_month_yyyymm`: Integer YYYYMM format

- `weight_monthly`: Monthly weight (if `compute_weights = TRUE`)

If `output = "microdata"`: Full input data with all computed columns
added.

If `output = "aggregates"`: Monthly aggregated indicators.

## Details

Main function to convert quarterly PNADC survey data to monthly time
series. Returns a crosswalk data.frame for joining with original data,
adding reference month information and optionally monthly survey
weights.

### Reference Month Identification

The algorithm determines which month each survey response refers to
based on:

- IBGE's "Parada Tecnica" rules for reference week timing

- Respondent birthdates (constrains possible interview dates)

- UPA-panel grouping (everyone interviewed together)

Typically 85-90% of observations can be assigned a definite reference
month.

### Monthly Weight Computation

When `compute_weights = TRUE`, the function:

1.  Fetches monthly population from IBGE SIDRA API (table 6022)

2.  Redistributes quarterly weights to months using hierarchical rake
    weighting

3.  Smooths monthly aggregates to remove quarterly artifacts

The resulting `weight_monthly` is the **final** weight for
general-purpose monthly analysis. No Bayesian adjustment is applied.

For theme-specific calibration to match IBGE SIDRA series (e.g.,
unemployment rate, employment levels), use
[`calibrate_to_sidra`](https://antrologos.github.io/mensalizePNADC/reference/calibrate_to_sidra.md)
separately.

Additional required columns for weight computation:

- Survey design: `V1028`, `V1008`, `V2003`, `UF`, `posest`, `posest_sxi`

## Dependencies

When `compute_weights = TRUE`, the `sidrar` package is required to fetch
population data. Install with: `install.packages("sidrar")`

## See also

[`identify_reference_month`](https://antrologos.github.io/mensalizePNADC/reference/identify_reference_month.md)
for just reference month identification
[`calibrate_monthly_weights`](https://antrologos.github.io/mensalizePNADC/reference/calibrate_monthly_weights.md)
for weight calibration details
[`calibrate_to_sidra`](https://antrologos.github.io/mensalizePNADC/reference/calibrate_to_sidra.md)
for theme-specific Bayesian calibration
[`compute_labor_indicators`](https://antrologos.github.io/mensalizePNADC/reference/compute_labor_indicators.md)
for computing indicators from results

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage: identify reference months only
library(mensalizePNADC)

# Load stacked quarterly data (minimum columns)
pnadc <- data.table::fread("pnadc_stacked.csv",
  select = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
             "V2008", "V20081", "V20082", "V2009"))

# Get crosswalk
crosswalk <- mensalizePNADC(pnadc)

# Join with original quarterly file
original <- haven::read_dta("PNADC_2023T1.dta")
monthly <- dplyr::left_join(original, crosswalk,
  by = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003"))


# With monthly weights for general analysis
# (requires sidrar package and internet connection)
pnadc_full <- haven::read_dta("PNADCtrimestralempilhada.dta")

result <- mensalizePNADC(pnadc_full, compute_weights = TRUE)

# Use weight_monthly for any monthly aggregate
result[, .(pop = sum(weight_monthly)), by = ref_month_yyyymm]
} # }
```
