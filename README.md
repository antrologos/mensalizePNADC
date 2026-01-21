# PNADCperiods

<!-- badges: start -->
[![R-CMD-check](https://github.com/antrologos/PNADCperiods/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/antrologos/PNADCperiods/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/antrologos/PNADCperiods/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/antrologos/PNADCperiods/actions/workflows/pkgdown.yaml)
[![codecov](https://codecov.io/gh/antrologos/PNADCperiods/branch/master/graph/badge.svg)](https://codecov.io/gh/antrologos/PNADCperiods)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Convert Brazil's quarterly PNADC survey data into sub-quarterly time series (monthly, fortnightly, weekly) with proper survey weight calibration.

## Overview

The `PNADCperiods` package identifies reference periods (months, fortnights, weeks) in Brazil's quarterly PNADC (Pesquisa Nacional por Amostra de Domicilios Continua) microdata and computes calibrated survey weights for sub-quarterly analysis.

**Why sub-quarterly analysis?** PNADC quarterly statistics are actually moving averages of three months. This package recovers the specific period each observation refers to, enabling true monthly (or finer) labor market analysis.

### Key Features

- **Nested identification hierarchy**: Weeks require fortnights, fortnights require months (enforced by construction)
- **~97% monthly determination rate** when using stacked multi-quarter data
- **~6% strict fortnight rate** (within-quarter constraints only)
- **~1.5% strict week rate** (within-quarter constraints only)
- **Experimental strategies** boost fortnight to ~27-28% and week to ~2% via probabilistic + UPA aggregation
- **IBGE-based calendar**: All periods use IBGE's official Sunday-Saturday week boundaries
- **Hierarchical weight calibration** adapted per time granularity
- **Fast**: Processes ~450,000 rows/sec (~1 minute for 28M rows)

## Installation

```r
# Requires R >= 4.0.0

# Install from GitHub
devtools::install_github("antrologos/PNADCperiods")

# For SIDRA API integration (optional, for weight calibration):
install.packages("sidrar")
```

## Quick Start

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

# Analyze by month
result[determined_month == TRUE, .(
  employed = sum(weight_monthly * (ocupado == 1), na.rm = TRUE)
), by = ref_month_yyyymm]
```

## Key Functions

| Function | Description |
|----------|-------------|
| `pnadc_identify_periods()` | Build crosswalk: identify reference months, fortnights, and weeks (nested) |
| `pnadc_apply_periods()` | Apply crosswalk to data + hierarchical weight calibration |
| `identify_reference_month()` | Standalone month identification |
| `identify_reference_fortnight()` | Standalone fortnight identification |
| `identify_reference_week()` | Standalone week identification |
| `pnadc_experimental_periods()` | Experimental probabilistic/UPA aggregation strategies |
| `combine_period_crosswalks()` | Merge strict and experimental crosswalks |
| `fetch_monthly_population()` | Fetch population totals from IBGE SIDRA API |
| `validate_pnadc()` | Validate input data has required columns |

## Nested Identification Hierarchy

The algorithm enforces strict nesting **by construction**:

```
determined_week => determined_fortnight => determined_month
```

- **Phase 1**: Identify MONTHS using cross-quarter aggregation at UPA-V1014 level
- **Phase 2**: Identify FORTNIGHTS only within determined months (household level, within-quarter)
- **Phase 3**: Identify WEEKS only within determined fortnights (household level, within-quarter)

This means fortnights can **only** be identified for observations with determined months, and weeks can **only** be identified for observations with determined fortnights.

## Period Definitions

### IBGE Calendar

All periods use IBGE's official calendar where weeks run Sunday-Saturday:

| Period | Definition |
|--------|------------|
| **Month** | 4-5 IBGE weeks; first week has >= 4 days in the calendar month |
| **Fortnight** | First half (days 1-15) or second half (days 16+) of a month |
| **Week** | Sunday-Saturday; belongs to the month where its Saturday falls |

### Output Columns

The crosswalk contains IBGE-based period boundaries:

```r
# Month columns
ref_month_start      # Sunday of first IBGE week of the month
ref_month_end        # Saturday of last IBGE week of the month
ref_month_in_quarter # Position in quarter (1, 2, 3)
ref_month_yyyymm     # Integer YYYYMM format (e.g., 202301)
ref_month_weeks      # Number of IBGE weeks in month (4 or 5)
determined_month     # TRUE if month was determined

# Fortnight columns
ref_fortnight_start       # Sunday of first IBGE week of the fortnight
ref_fortnight_end         # Saturday of last IBGE week of the fortnight
ref_fortnight_in_quarter  # Position in quarter (1-6)
ref_fortnight_yyyyff      # Integer YYYYFF format (01-24 per year)
determined_fortnight      # TRUE if fortnight was determined

# Week columns
ref_week_start       # Sunday of the IBGE week
ref_week_end         # Saturday of the IBGE week
ref_week_in_quarter  # Position in quarter (1-14)
ref_week_yyyyww      # Integer IBGE YYYYWW format
determined_week      # TRUE if week was determined
```

## Weight Calibration

The `pnadc_apply_periods()` function performs hierarchical rake weighting with automatic adaptation per time granularity:

| Calibration Unit | Cell Levels | Smoothing | Notes |
|------------------|-------------|-----------|-------|
| `"month"` | 4 (full hierarchy) | 3-period rolling mean | Age, region, state, post-stratum |
| `"fortnight"` | 2 (simplified) | 7-period rolling mean | Age + region only |
| `"week"` | 1 (minimal) | None | Age groups only |

All time periods calibrate to the **full Brazilian population** from SIDRA (not divided).

```r
# Monthly calibration (default)
result <- pnadc_apply_periods(pnadc, crosswalk,
                               weight_var = "V1028",
                               anchor = "quarter")

# Fortnight calibration
result <- pnadc_apply_periods(pnadc, crosswalk,
                               weight_var = "V1028",
                               anchor = "quarter",
                               calibration_unit = "fortnight")

# Weekly calibration
result <- pnadc_apply_periods(pnadc, crosswalk,
                               weight_var = "V1028",
                               anchor = "quarter",
                               calibration_unit = "week")
```

## Required Input Columns

| For | Required Columns |
|-----|-----------------|
| **Period identification** | `Ano`, `Trimestre`, `UPA`, `V1008`, `V1014`, `V2008`, `V20081`, `V20082`, `V2009` |
| **Weight calibration** | Add: `V1028` (or `V1032`), `UF`, `posest`, `posest_sxi` |

Use `validate_pnadc()` to check your data before processing.

## Experimental Strategies

The `pnadc_experimental_periods()` function provides experimental strategies to boost fortnight and week determination rates:

| Strategy | Fortnight % | Week % | Description |
|----------|-------------|--------|-------------|
| Strict only | ~6% | ~1.5% | Baseline (deterministic) |
| `"probabilistic"` | ~21% | ~0.6% | Assigns based on date interval midpoint |
| `"upa_aggregation"` | ~22% | ~0.6% | Extends via UPA consensus |
| `"both"` | ~27-28% | ~2% | Sequential: probabilistic then UPA |

**Key finding**: UPA homogeneity rate = 100% for both fortnights and weeks within quarters.

```r
# Build strict crosswalk
crosswalk <- pnadc_identify_periods(pnadc)

# Apply experimental strategies
crosswalk_exp <- pnadc_experimental_periods(
  crosswalk,
  pnadc,
  strategy = "both",
  confidence_threshold = 0.9
)

# Check results
crosswalk_exp[, .(
  strict = sum(!is.na(ref_month_in_quarter) & !probabilistic_assignment),
  experimental = sum(probabilistic_assignment, na.rm = TRUE),
  total = sum(determined_month)
)]

# Use directly with calibration
result <- pnadc_apply_periods(pnadc, crosswalk_exp,
                               weight_var = "V1028",
                               anchor = "quarter")
```

## Test Coverage

The package has comprehensive tests ensuring:

- **Nesting enforcement**: Fortnight requires month, week requires fortnight (always)
- **Value consistency**: Fortnights fall within their month's bounds, weeks within fortnight bounds
- **IBGE boundaries**: ref_month_start is always Sunday, ref_month_end is always Saturday
- **Input validation**: Detects missing columns, invalid years/quarters, invalid birth dates
- **Calibration logic**: Proper weight adjustment and population target matching
- **Edge cases**: NA handling, empty data, year boundaries, single-quarter data

Run tests with:
```r
devtools::test("PNADCperiods")
```

## Documentation

| Guide | Description |
|-------|-------------|
| [Get Started](https://antrologos.github.io/PNADCperiods/articles/getting-started.html) | Installation and first steps |
| [Download Data](https://antrologos.github.io/PNADCperiods/articles/download-and-prepare.html) | Prepare PNADC microdata |
| [Applied Examples](https://antrologos.github.io/PNADCperiods/articles/applied-examples.html) | COVID, recession, minimum wage validation |
| [How It Works](https://antrologos.github.io/PNADCperiods/articles/how-it-works.html) | Algorithm details |
| [Annual Poverty Analysis](https://antrologos.github.io/PNADCperiods/articles/annual-poverty-analysis.html) | Using annual visit data |
| [Complex Survey Design](https://antrologos.github.io/PNADCperiods/articles/complex-survey-design.html) | Survey design considerations |
| [Benchmark Report](https://antrologos.github.io/PNADCperiods/articles/determination-rates-benchmark.html) | Determination rates on complete PNADC data |
| [Reference](https://antrologos.github.io/PNADCperiods/reference/index.html) | Function documentation |

## Tips for Best Results

1. **Stack multiple quarters**: Use 2+ years of data for best monthly determination rates (~97%)
2. **Use `validate_pnadc()`**: Check your data has all required columns before processing
3. **Choose the right anchor**: Use `anchor = "quarter"` with V1028 weights, `anchor = "year"` with V1032 weights
4. **Check determination rates**: The crosswalk has a `determination_rates` attribute with rates for each period type
5. **Consider experimental strategies**: If you need higher fortnight/week rates and can accept probabilistic assignments

## Citation

If you use this package in your research, please cite:

```r
citation("PNADCperiods")
```

> Hecksher, M. & Barbosa, R. (2024). PNADCperiods: Identify Reference Periods in Brazil's PNADC Survey Data. R package. https://github.com/antrologos/PNADCperiods

## Getting Help

- **Bug reports**: [GitHub Issues](https://github.com/antrologos/PNADCperiods/issues)
- **Questions**: Open a GitHub issue with the "question" label

## Authors

- **Marcos Hecksher** - Original methodology ([ORCID](https://orcid.org/0000-0003-2992-1252))
- **Rogerio Barbosa** - R package maintainer ([ORCID](https://orcid.org/0000-0002-6796-4547))

## References

- [IBGE PNADC Documentation](https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html)
- Hecksher, M. (2024). Mensalizacao da PNADC. Working paper.

## License

MIT
