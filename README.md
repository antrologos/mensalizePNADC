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
- **97.0% monthly determination rate** with full data stacking (2012-2025, 55 quarters)
- **Progressive improvement**: 70% (1 quarter) → 94% (8 quarters) → 97% (55 quarters)
- **9.2% strict fortnight rate** (within-quarter constraints only)
- **3.3% strict week rate** (within-quarter constraints only)
- **Experimental strategies** boost rates: month→97.3%, fortnight→13.5%, week→7.5% (probabilistic conf=0.8)
- **IBGE-based calendar**: All periods use IBGE's official Sunday-Saturday week boundaries
- **Adaptive weight calibration**: Hierarchical raking with period-specific smoothing (4/2/1 cell levels)
- **SIDRA integration**: Automatic population target fetching from IBGE API
- **Zero negative weights**: All calibrations in comprehensive tests produced 0 negative weights
- **Fast**: Processes ~28M rows in ~30 seconds for crosswalk generation

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

# Load stacked PNADC data (all available years recommended for best determination)
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
| `pnadc_experimental_periods()` | Experimental probabilistic/UPA aggregation strategies |
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

## Determination Rates: Impact of Data Stacking

Month determination rates improve dramatically with more stacked data due to PNADC's rotating panel design. Each household (UPA + V1014) is interviewed for 5 consecutive quarters, always in the **same relative month position**. This means birthday constraints from **any** quarter can determine the month for **all** quarters.

**Benchmark progression (strict identification only):**

| Data Coverage | Quarters | Observations | Month % | Fortnight % | Week % |
|---------------|----------|--------------|---------|-------------|--------|
| Single quarter | 1 | ~516K | **~70%** | ~7% | ~2% |
| Two years | 8 | 3.76M | **~94%** | ~9% | ~3% |
| Full history (2012-2025) | 55 | 28.4M | **97.0%** | 9.2% | 3.3% |

**Why doesn't fortnight/week improve?** These periods use within-quarter constraints only (household aggregation). Each quarter's interview timing is independent at the sub-monthly level, so cross-quarter aggregation doesn't help.

**Practical implication**: Stack all available years for best monthly determination. For 2019-2020 analysis, include 2012-2018 data when building the crosswalk - it dramatically improves month identification.

## Period Definitions

### IBGE Calendar

All periods use IBGE's official calendar where weeks run Sunday-Saturday:

| Period | Definition |
|--------|------------|
| **Month** | 4 IBGE reference weeks (28 days); first week has >= 4 days in the calendar month |
| **Fortnight** | 2 IBGE weeks; first fortnight = weeks 1-2, second fortnight = weeks 3-4 of each month |
| **Week** | Sunday-Saturday; belongs to the month where its Saturday falls |

### Output Columns

The crosswalk contains period identifiers (keyed by UPA, V1014):

```r
# Join keys (for merging with PNADC data)
Ano, Trimestre, UPA, V1008, V1014

# Month columns
ref_month_in_quarter # Position in quarter (1, 2, 3, or NA)
ref_month_in_year    # Position in year (1-12, or NA)
ref_month_yyyymm     # Integer YYYYMM format (e.g., 202301)
determined_month     # TRUE if month was determined

# Fortnight columns
ref_fortnight_in_month    # Position in month (1, 2, or NA)
ref_fortnight_in_quarter  # Position in quarter (1-6, or NA)
ref_fortnight_yyyyff      # Integer YYYYFF format (01-24 per year)
determined_fortnight      # TRUE if fortnight was determined

# Week columns
ref_week_in_month    # Position in month (1-4, or NA)
ref_week_in_quarter  # Position in quarter (1-12, or NA)
ref_week_yyyyww      # Integer IBGE YYYYWW format
determined_week      # TRUE if week was determined
```

**Note:** Use `pnadc_apply_periods()` to merge these periods back to your full data and compute calibrated weights.

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

| Strategy | Description | Nesting |
|----------|-------------|---------|
| `"probabilistic"` | Assigns based on date interval midpoint when range spans 2 periods | Respects month→fortnight→week |
| `"upa_aggregation"` | Extends via UPA/UPA-V1014 consensus when proportion threshold met | Respects month→fortnight→week |
| `"both"` | Sequential: probabilistic first, then UPA aggregation | Union of both strategies |

All strategies enforce proper nesting: experimental fortnights require identified months (strict or experimental), and experimental weeks require identified fortnights.

### Benchmark Results (2019-2020 data, 8 quarters)

| Strategy | Threshold | Month % | Fortnight % | Week % |
|----------|-----------|---------|-------------|--------|
| Strict (baseline) | - | 94.3% | 8.9% | 3.3% |
| Probabilistic | 0.8 | **97.3%** | **13.5%** | **7.5%** |
| Probabilistic | 0.9 | 97.3% | 13.5% | 3.6% |
| UPA aggregation | 0.9 | 94.3% | 8.9% | 3.3% |
| Both | 0.9 | 97.3% | 13.5% | 3.6% |

**Key finding**: Probabilistic strategy provides the main improvement (+3pp month, +4.6pp fortnight). UPA aggregation adds minimal benefit.

**Note:** These benchmarks use 8-quarter data. With full data stacking (55 quarters), the strict baseline improves to 97.0% month / 9.2% fortnight / 3.3% week, matching the probabilistic strategy's month rate without experimental methods.

```r
# Build strict crosswalk (with store_date_bounds for experimental use)
crosswalk <- pnadc_identify_periods(pnadc, store_date_bounds = TRUE)

# Apply experimental strategies (output is directly compatible with pnadc_apply_periods)
crosswalk_exp <- pnadc_experimental_periods(
  crosswalk,
  strategy = "both",
  confidence_threshold = 0.9,
  upa_proportion_threshold = 0.5
)

# Check improvement vs strict
crosswalk_exp[, .(
  month_rate = mean(determined_month),
  fortnight_rate = mean(determined_fortnight),
  week_rate = mean(determined_week)
)]

# Use directly with calibration (experimental output is fully compatible)
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

1. **Stack ALL available quarters**: Full history (2012-2025) achieves 97% monthly determination. Using 8 quarters yields ~94%, single quarter only ~70%
2. **Include extra years for your analysis period**: Even if analyzing 2019-2020, include 2012-2018 in the crosswalk for better month determination
3. **Use `validate_pnadc()`**: Check your data has all required columns before processing
4. **Choose the right anchor**: Use `anchor = "quarter"` with V1028 weights, `anchor = "year"` with V1032 weights
5. **Annual data achieves 98% match**: Annual visit data with year anchor has highest determination rate
6. **For fortnightly analysis**: Use experimental probabilistic strategy (conf=0.9) to boost from 9.2% to 13.5%
7. **For weekly analysis**: Use conf=0.8 threshold to boost from 3.3% to 7.5% (if willing to accept lower confidence)
8. **COVID-19 impact**: 2020-Q4 shows degraded rates (~83% month) due to collection disruptions

## Comprehensive Test Results

The package was comprehensively tested on 2019-2020 PNADC data (8 quarters, 3.76M observations):

| Metric | Result |
|--------|--------|
| Total test operations | 42 (16 crosswalks + 16 quarterly applies + 4 annual applies + 6 data ops) |
| Errors encountered | **0** |
| Negative weights produced | **0** (across all calibration configurations) |
| Processing time (full suite) | ~6.5 minutes |

### Calibration Quality (2019-2020 test subset)

| Anchor | Unit | Match Rate | Weight CV | Notes |
|--------|------|------------|-----------|-------|
| Quarter | Month (smooth) | 94.3% | 1.19 | With 8-quarter crosswalk |
| Quarter | Fortnight (smooth) | 8.9% | 1.21 | Sparse but stable |
| Quarter | Week | 3.3% | 2.83 | High CV due to sparsity |
| Year | Month (smooth) | **98.0%** | **1.05** | Best overall configuration |

**Note:** These rates are from an 8-quarter crosswalk. Using a full-history crosswalk (2012-2025, 55 quarters) improves quarterly month determination to 97.0%.

See `test_results/summaries/` for detailed benchmark data.

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
