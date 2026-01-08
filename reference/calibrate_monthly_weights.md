# Calibrate Monthly Weights

The original PNADC survey weights (`V1028`) are designed for quarterly
estimates. This function creates monthly weights by:

1.  Grouping observations by nested demographic/geographic cells

2.  Iteratively adjusting weights so monthly totals match quarterly
    totals within each cell

3.  Calibrating final weights against external monthly population totals

## Usage

``` r
calibrate_monthly_weights(data, monthly_totals, n_cells = 4L)
```

## Arguments

- data:

  A data.frame or data.table with PNADC microdata including reference
  month information (output from
  [`identify_reference_month`](https://antrologos.github.io/mensalizePNADC/reference/identify_reference_month.md)).
  Required columns include:

  - Reference month columns: `ref_month_yyyymm`, `ref_month_in_quarter`

  - Survey design: `V1028`, `UF`, `posest`, `posest_sxi`

  - Demographics: `V2009` (age)

  - Time: `Ano`, `Trimestre`

- monthly_totals:

  A data.frame with monthly population totals. Required columns:

  - `ref_month_yyyymm` or `anomesexato`: YYYYMM integer

  - `m_populacao`: Monthly population in thousands

- n_cells:

  Integer. Number of hierarchical cell levels to use (1-4). Default is 4
  (full hierarchy). Lower values are faster but less precise.

## Value

A data.table with the input data plus:

- `weight_calibrated`: Calibrated monthly weight

- `celula1` through `celula4`: Cell identifiers (if computed)

## Details

Redistributes quarterly survey weights to monthly weights using
hierarchical calibration across demographic and geographic cells.

The hierarchical calibration cells are:

- celula1:

  Age groups: 0-13, 14-29, 30-59, 60+

- celula2:

  Post-stratum group + age group

- celula3:

  State (UF) + celula2

- celula4:

  Post-stratum (posest) + celula2

At each level, weights are adjusted so that:
`sum(weight_new) by (cell, month) / sum(weight_old) by (cell, month)`
equals
`sum(V1028) by (cell, quarter) / sum(weight_old) by (cell, month)`

This preserves the quarterly totals while redistributing to months.

## See also

[`identify_reference_month`](https://antrologos.github.io/mensalizePNADC/reference/identify_reference_month.md),
[`adjust_weights_bayesian`](https://antrologos.github.io/mensalizePNADC/reference/calibrate_to_sidra.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# First identify reference months
crosswalk <- identify_reference_month(pnadc_data)
merged <- merge(pnadc_data, crosswalk, by = join_keys)

# Then calibrate weights
result <- calibrate_monthly_weights(merged, monthly_pop)
} # }
```
