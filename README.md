# PNADCperiods

<!-- badges: start -->
[![R-CMD-check](https://github.com/antrologos/mensalizePNADC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/antrologos/mensalizePNADC/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/antrologos/mensalizePNADC/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/antrologos/mensalizePNADC/actions/workflows/pkgdown.yaml)
[![codecov](https://codecov.io/gh/antrologos/mensalizePNADC/branch/master/graph/badge.svg)](https://codecov.io/gh/antrologos/mensalizePNADC)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

Convert Brazil's quarterly PNADC survey data into sub-quarterly time series (monthly, fortnightly, weekly).

## Overview

The `PNADCperiods` package identifies reference periods (months, fortnights, weeks) in Brazil's quarterly PNADC (Pesquisa Nacional por Amostra de Domicilios Continua) microdata and optionally computes calibrated survey weights.

**Why sub-quarterly analysis?** PNADC quarterly statistics are actually moving averages of three months. This package recovers the specific period each observation refers to, enabling true monthly (or finer) labor market analysis.

- **~97% monthly determination rate** when using stacked multi-quarter data
- **~2-5% fortnightly determination rate** (within-quarter constraints only)
- **~1-2% weekly determination rate** (within-quarter constraints only)
- **Validated methodology** based on Hecksher (2024)
- **Fast**: ~1 minute for 28M rows

## Key Functions

| Function | Description |
|----------|-------------|
| `pnadc_identify_periods()` | Build crosswalk: identify reference months, fortnights, and weeks |
| `pnadc_apply_periods()` | Apply crosswalk to data + optional weight calibration |
| `identify_reference_month()` | Reference month identification only |
| `identify_reference_fortnight()` | Reference fortnight identification only |
| `identify_reference_week()` | Reference week identification only |
| `calibrate_monthly_weights()` | Hierarchical rake weighting for monthly weights |
| `fetch_monthly_population()` | Fetch population totals from IBGE SIDRA API |
| `validate_pnadc()` | Validate input data has required columns |

## Installation

```r
# Install from GitHub
devtools::install_github("antrologos/mensalizePNADC")
```

## Quick Example

```r
library(PNADCperiods)
library(data.table)

# Load stacked PNADC data (2+ years recommended for best determination rate)
pnadc <- fread("pnadc_stacked.csv")

# Step 1: Build crosswalk (identify reference periods)
crosswalk <- pnadc_identify_periods(pnadc)

# Step 2: Apply to data (with optional weight calibration)
result <- pnadc_apply_periods(pnadc, crosswalk,
                               weight_var = "V1028",
                               anchor = "quarter")

# Use in analysis
result[, .(n = .N), by = ref_month_yyyymm]
```

**Key parameters for `pnadc_apply_periods()`:**
- `weight_var`: Which weight column to use (`"V1028"` for quarterly, `"V1032"` for annual)
- `anchor`: Calibration anchor (`"quarter"` or `"year"`)
- `calibrate = TRUE`: Compute calibrated weights (default: TRUE)
- `calibration_unit`: Granularity for calibration (`"month"`, `"fortnight"`, or `"week"`)

**Calibration strategy:**
- All time periods (month, fortnight, week) calibrate to the **full Brazilian population** from SIDRA
- Hierarchical raking is automatically simplified for finer granularities:
  - Monthly: 4 cell levels (full demographic/geographic hierarchy)
  - Fortnight: 2 cell levels (age + region)
  - Weekly: 1 cell level (age groups only)

**Crosswalk output columns:**
- `ref_month`, `ref_month_in_quarter`, `ref_month_yyyymm`, `determined_month`
- `ref_fortnight`, `ref_fortnight_in_quarter`, `ref_fortnight_yyyyff`, `determined_fortnight`
- `ref_week`, `ref_week_in_quarter`, `ref_week_yyyyww`, `determined_week`

**Required input columns:**
- For identification: `Ano`, `Trimestre`, `UPA`, `V1008`, `V1014`, `V2008`, `V20081`, `V20082`, `V2009`
- For weights: add `V1028` (or `V1032`), `UF`, `posest`, `posest_sxi`

For complete examples, see the [Get Started guide](https://antrologos.github.io/mensalizePNADC/articles/getting-started.html).

## Documentation

| Guide | Description |
|-------|-------------|
| [Get Started](https://antrologos.github.io/mensalizePNADC/articles/getting-started.html) | Installation and first steps |
| [Download Data](https://antrologos.github.io/mensalizePNADC/articles/download-and-prepare.html) | Prepare PNADC microdata |
| [Applied Examples](https://antrologos.github.io/mensalizePNADC/articles/applied-examples.html) | COVID, recession, minimum wage validation |
| [How It Works](https://antrologos.github.io/mensalizePNADC/articles/how-it-works.html) | Algorithm details |
| [Reference](https://antrologos.github.io/mensalizePNADC/reference/index.html) | Function documentation |

## Authors

- **Marcos Hecksher** - Original methodology
- **Rogerio Barbosa** - R package maintainer

## References

- [IBGE PNADC Documentation](https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html)
- Hecksher, M. (2024). Mensalizacao da PNADC. Working paper.

## License

MIT
