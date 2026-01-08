# Calibrate Weights to IBGE SIDRA Series (Optional)

This is an **optional** function for users who need their estimates to
exactly match IBGE's official monthly series. The core
[`mensalizePNADC`](https://antrologos.github.io/mensalizePNADC/reference/mensalizePNADC.md)
function already produces valid monthly weights; this function provides
additional calibration for specific themes.

**Important**: Different themes require different Bayesian calibrations.
For example, weights calibrated to match unemployment rates will differ
from weights calibrated to match employment levels.

## Usage

``` r
calibrate_to_sidra(
  data,
  theme = c("unemployment", "employment", "labor_force", "income", "custom"),
  sidra_series = NULL,
  verbose = TRUE
)

adjust_weights_bayesian(data, monthly_targets, employment_detail = TRUE)
```

## Arguments

- data:

  A data.table output from
  [`mensalizePNADC`](https://antrologos.github.io/mensalizePNADC/reference/mensalizePNADC.md)
  with `compute_weights = TRUE`. Must contain `weight_monthly` and
  employment status variables (`VD4001`, `VD4002`, etc.).

- theme:

  Character. Which IBGE series to match:

  "unemployment"

  :   Taxa de desocupacao - unemployment rate

  "employment"

  :   Populacao ocupada - employment levels

  "labor_force"

  :   PEA/PNEA - labor force participation

  "income"

  :   Rendimento habitual - income aggregates

  "custom"

  :   User provides target series via `sidra_series`

- sidra_series:

  Optional data.frame with monthly target series. Required if
  `theme = "custom"`. Must contain:

  - `ref_month_yyyymm`: Month in YYYYMM format

  - `m_*`: Target values for each indicator (in thousands)

- verbose:

  Logical. Print progress messages? Default TRUE.

## Value

A data.table with the input data plus:

- `weight_sidra`: Bayesian-adjusted weight matching SIDRA series

The original `weight_monthly` is preserved for comparison.

## Details

Applies theme-specific Bayesian calibration so aggregated estimates
match IBGE's official SIDRA published series.

### Why Theme-Specific Calibration?

IBGE's published SIDRA series are internally consistent but use slightly
different adjustments for different indicators. This means:

- Weights optimized for unemployment rate may not sum exactly to the
  published employment level

- Weights optimized for employment may not produce the exact published
  unemployment rate

Choose the theme that matches your primary analysis focus.

### Algorithm

Uses Bayes' theorem to adjust individual weights: \$\$P(status \|
category) = P(status) \times P(category \| status) / P(category)\$\$

Where:

- status = employment situation (employed, unemployed, etc.)

- category = demographic/geographic cell

- P(status) is derived from SIDRA targets

## See also

[`mensalizePNADC`](https://antrologos.github.io/mensalizePNADC/reference/mensalizePNADC.md)
for core mensalization (required first) `adjust_weights_bayesian` for
the underlying Bayesian algorithm

## Examples

``` r
if (FALSE) { # \dontrun{
# First, get base weights
crosswalk <- mensalizePNADC(pnadc_full, compute_weights = TRUE,
                             monthly_totals = monthly_pop)

# Then, calibrate for unemployment analysis
unemployment_data <- calibrate_to_sidra(crosswalk, theme = "unemployment")

# Calculate unemployment rate (will match IBGE SIDRA exactly)
unemployment_data[, .(
  rate = sum(weight_sidra * (VD4002 == 2)) /
         sum(weight_sidra * (VD4001 == 1)) * 100
), by = ref_month_yyyymm]
} # }
```
