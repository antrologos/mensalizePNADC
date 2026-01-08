# mensalizePNADC

Convert Brazil’s quarterly PNADC survey data into monthly time series.

📖 **[Full
Documentation](https://antrologos.github.io/mensalizePNADC/)** —
Detailed algorithm explanation with examples and diagrams

## Overview

The `mensalizePNADC` package identifies reference months in Brazil’s
quarterly PNADC (Pesquisa Nacional por Amostra de Domicilios Continua)
survey data and optionally computes monthly survey weights. This enables
monthly (instead of quarterly) labor market analysis using PNADC
microdata.

**Key Features:**

- **High accuracy**: 95.83% determination rate on 28.4 million
  observations (2012-2025)
- **Fast processing**: 6.3 minutes for 28.4 million rows (basic mode),
  8.1 minutes with weight calibration
- **Minimal dependencies**: Requires `data.table` and `checkmate`;
  `sidrar` needed only for weight calibration
- **Flexible output**: Returns a crosswalk for easy joins with original
  data

## Installation

``` r
# Install from GitHub
devtools::install_github("antrologos/mensalizePNADC")
```

## Quick Start

``` r
library(mensalizePNADC)
library(data.table)

# Load stacked quarterly PNADC data (minimum required columns)
pnadc <- fread("pnadc_stacked.csv",
  select = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
             "V2008", "V20081", "V20082", "V2009"))

# Get reference month crosswalk
crosswalk <- mensalizePNADC(pnadc)
# Step 1/1: Identifying reference months...
#   Determination rate: 95.8%

# Join with original data
result <- merge(original_data, crosswalk,
  by = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003"))

# Now use ref_month_yyyymm for monthly analysis
result[, .(n = .N), by = ref_month_yyyymm]
```

## Important: Use Stacked Data for Best Results

The mensalization algorithm achieves **95.83% determination rate** when
processing **stacked multi-quarter data**. If you process quarters
individually, you’ll only get ~65-75% determination.

| Processing Mode              | Determination Rate |
|------------------------------|--------------------|
| Per-quarter (single quarter) | 65-75%             |
| Stacked (multi-quarter)      | **95.83%**         |

This is by design: PNADC uses a rotating panel where households are
interviewed in the same relative month position each quarter (always
month 1, always month 2, or always month 3). The algorithm combines
birthday constraints from all quarters to narrow down which month the
household belongs to.

**Recommended**: Stack at least 2 years of quarterly data before calling
[`mensalizePNADC()`](https://antrologos.github.io/mensalizePNADC/reference/mensalizePNADC.md).

------------------------------------------------------------------------

## How Reference Month Identification Works

The PNADC is collected quarterly, but each interview actually
corresponds to a specific **reference week** within the quarter. This
package identifies which of the three months in the quarter each
observation belongs to.

### Step-by-Step Algorithm

#### Step 1: Determine Valid Interview Saturdays

IBGE’s “Parada Tecnica” (technical break) rules define when interviews
can occur. Each reference week ends on a Saturday, and that Saturday
must have at least **4 days** within the reference month.

For each month in the quarter, the algorithm finds the **first valid
Saturday**:

    Example for Q1 2023:
    - January 2023: First Saturday is Jan 7 (has 7 days in Jan) -> Valid
    - February 2023: First Saturday is Feb 4 (has 4 days in Feb) -> Valid
    - March 2023: First Saturday is Mar 4 (has 4 days in Mar) -> Valid

The possible interview date range for each quarter spans from the first
Saturday of month 1 to (first Saturday of month 3 + 21 days).

#### Step 2: Apply Birthday Constraints

Each respondent’s reported age and birthdate constrains when the
interview could have occurred:

- If `Age = Year - BirthYear` (visit was **after** birthday): interview
  must have been on or after the first Saturday following the birthday
- If `Age = Year - BirthYear - 1` (visit was **before** birthday):
  interview must have been before the birthday

For each person, this narrows the possible interview date window.

#### Step 3: Convert Date Ranges to Month Positions

The narrowed date range is converted to month-in-quarter positions (1,
2, or 3):

- If minimum possible date falls in month 2, `month_min_pos = 2`
- If maximum possible date falls in month 1, `month_max_pos = 1`
- Special handling for dates near month boundaries (within first 3 days)

#### Step 4: Aggregate to UPA-Panel Level

All individuals in the same **UPA + Panel (V1014)** are interviewed
together in the same reference month. The algorithm takes the
**intersection** of all individual constraints within each UPA-Panel
group:

    upa_month_min = max(individual month_min positions)
    upa_month_max = min(individual month_max positions)

If `upa_month_min == upa_month_max`, the reference month is uniquely
determined.

#### Step 5: Cross-Quarter Aggregation (Key Innovation)

PNADC is a **rotating panel**: the same UPA-V1014 is interviewed in the
**same relative month position** (always month 1, always month 2, or
always month 3) across all their quarterly visits (up to 5 consecutive
quarters).

The algorithm aggregates constraints across ALL quarters for each
UPA-V1014, not just within a single quarter. This dramatically improves
determination rates:

- **Per-quarter**: Only uses birthday constraints from that quarter
  (~70% determination)
- **Cross-quarter**: Combines constraints from all visits (~96%
  determination)

#### Step 6: Handle Exception Quarters

Some quarters have non-standard timing rules (technical breaks that
cross month boundaries). For quarters 2016t3, 2016t4, 2017t2, 2022t3,
and 2023t2, a relaxed rule (minimum 3 days instead of 4) is applied if
the standard rule fails to determine the month.

------------------------------------------------------------------------

## How Monthly Weight Calibration Works

When `compute_weights = TRUE`, the package computes monthly survey
weights that sum to IBGE’s official monthly population estimates.

### Step 1: Fetch Monthly Population from SIDRA API

The function automatically downloads population estimates from IBGE’s
SIDRA API (table 6022).

**The Challenge**: SIDRA provides **moving-quarter** population
estimates, not exact monthly values. Each SIDRA value represents the
3-month average centered on the middle month:

| SIDRA Code | 3-Month Window   | Represents Population For |
|------------|------------------|---------------------------|
| 201203     | Jan+Feb+Mar 2012 | **February 2012**         |
| 201204     | Feb+Mar+Apr 2012 | **March 2012**            |
| 201205     | Mar+Apr+May 2012 | **April 2012**            |

**Transformation**: The function shifts values to align with their
center month: - `m_populacao[Feb 2012] = SIDRA value for code 201203`

**Boundary Extrapolation**: The first month (Jan 2012) and latest month
have no centered moving quarter. These are extrapolated using quadratic
regression on population differences from the nearest 25 observations.

### Step 2: Hierarchical Rake Weighting

The original quarterly weights (V1028) are redistributed to months using
**iterative proportional fitting** (rake weighting) across nested
calibration cells:

| Cell Level | Definition                          | Purpose                      |
|------------|-------------------------------------|------------------------------|
| `celula1`  | Age groups: 0-13, 14-29, 30-59, 60+ | Demographic balance          |
| `celula2`  | Post-stratum group + age            | Regional-demographic balance |
| `celula3`  | State (UF) + celula2                | State-level balance          |
| `celula4`  | Post-stratum (posest) + celula2     | Fine geographic balance      |

At each level, weights are adjusted so that:

    sum(weight_new) by (cell, month) / sum(weight_old) by (cell, month)
      = sum(V1028) by (cell, quarter) / sum(weight_old) by (cell, month)

This preserves the quarterly totals while redistributing to months
proportionally.

### Step 3: Final Calibration to Population Totals

After hierarchical reweighting, a final scaling ensures monthly weighted
totals match SIDRA population:

    weight_final = weight_calibrated * (SIDRA_population / sum(weight_calibrated))

The output `weight_monthly` is calibrated to match IBGE’s official
monthly population estimates.

------------------------------------------------------------------------

## Output Variables

| Variable               | Type    | Description                                     |
|------------------------|---------|-------------------------------------------------|
| `ref_month`            | Date    | Reference month (first day, e.g., “2023-01-01”) |
| `ref_month_in_quarter` | Integer | Position in quarter: 1, 2, 3, or NA             |
| `ref_month_yyyymm`     | Integer | YYYYMM format (e.g., 202301)                    |
| `weight_monthly`       | Numeric | Monthly weight (if `compute_weights = TRUE`)    |

## Required Input Variables

**Minimum (reference month identification only):**

| Variable    | Description                           |
|-------------|---------------------------------------|
| `Ano`       | Survey year                           |
| `Trimestre` | Quarter (1-4)                         |
| `UPA`       | Primary Sampling Unit                 |
| `V1014`     | Panel identifier                      |
| `V1008`     | Household identifier                  |
| `V2003`     | Person identifier                     |
| `V2008`     | Birth day (1-31, or 99 for unknown)   |
| `V20081`    | Birth month (1-12, or 99 for unknown) |
| `V20082`    | Birth year (or 9999 for unknown)      |
| `V2009`     | Age                                   |

**Additional (for `compute_weights = TRUE`):**

| Variable     | Description                      |
|--------------|----------------------------------|
| `V1028`      | Original quarterly survey weight |
| `UF`         | State code                       |
| `posest`     | Post-stratification cell         |
| `posest_sxi` | Post-stratification group        |

## Functions

| Function                                                                                                            | Description                                       |
|---------------------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| [`mensalizePNADC()`](https://antrologos.github.io/mensalizePNADC/reference/mensalizePNADC.md)                       | Main function: identify months + optional weights |
| [`identify_reference_month()`](https://antrologos.github.io/mensalizePNADC/reference/identify_reference_month.md)   | Just reference month identification               |
| [`calibrate_monthly_weights()`](https://antrologos.github.io/mensalizePNADC/reference/calibrate_monthly_weights.md) | Rake weighting for monthly weights                |
| [`fetch_monthly_population()`](https://antrologos.github.io/mensalizePNADC/reference/fetch_monthly_population.md)   | Fetch population from SIDRA API                   |
| [`validate_pnadc()`](https://antrologos.github.io/mensalizePNADC/reference/validate_pnadc.md)                       | Input data validation                             |
| [`get_exception_quarters()`](https://antrologos.github.io/mensalizePNADC/reference/get_exception_quarters.md)       | List quarters with non-standard timing            |

------------------------------------------------------------------------

## Performance Results

Tested on 55 quarters (2012Q1 - 2025Q3) with 28,395,273 total
observations:

### Processing Time

| Mode                             | Loading  | Processing | Total           |
|----------------------------------|----------|------------|-----------------|
| Basic (`compute_weights=FALSE`)  | 17.4 sec | 361.2 sec  | **6.3 minutes** |
| Weights (`compute_weights=TRUE`) | 15.8 sec | 468.6 sec  | **8.1 minutes** |

### Determination Rates by Period

| Period    | Quarters    | Determination Rate | Notes                              |
|-----------|-------------|--------------------|------------------------------------|
| 2012      | Q1-Q4       | 91.8% - 98.9%      | First year, panel rotation effects |
| 2013-2019 | 28 quarters | 96.1% - 99.0%      | Best results, stable sampling      |
| 2020-2021 | 8 quarters  | 93.0% - 97.5%      | Pandemic sample changes            |
| 2022-2024 | 12 quarters | 91.3% - 95.6%      | Post-pandemic normalization        |
| 2025      | Q1-Q3       | 88.9% - 95.9%      | Most recent data                   |

### Overall Statistics

- **Total observations**: 28,395,273
- **Determined**: 27,211,667 (95.83%)
- **Not determined**: 1,183,606 (4.17%)

------------------------------------------------------------------------

## Monthly Weights

For monthly aggregate estimates, compute monthly-appropriate survey
weights:

``` r
result <- mensalizePNADC(pnadc_full,
  compute_weights = TRUE)

# Use weight_monthly for estimates
result[, .(pop = sum(weight_monthly)), by = ref_month_yyyymm]
```

The `weight_monthly` output is calibrated to match IBGE’s official
monthly population estimates from SIDRA table 6022. Average monthly
population: ~206 million (matching Brazil’s population).

------------------------------------------------------------------------

## Topic-Specific Calibration (Coming Soon)

A future version will include
[`calibrate_to_sidra()`](https://antrologos.github.io/mensalizePNADC/reference/calibrate_to_sidra.md)
for **theme-specific Bayesian calibration** to exactly match IBGE’s
published SIDRA series for specific indicators.

### How It Will Work

The base `weight_monthly` from
[`mensalizePNADC()`](https://antrologos.github.io/mensalizePNADC/reference/mensalizePNADC.md)
provides general-purpose monthly weights calibrated to population
totals. However, different labor market indicators (unemployment rate,
employment level, income) may require slightly different weight
adjustments to exactly match IBGE’s official published series.

The
[`calibrate_to_sidra()`](https://antrologos.github.io/mensalizePNADC/reference/calibrate_to_sidra.md)
function will:

1.  **Fetch target series** from SIDRA (e.g., unemployment rate,
    employment levels)
2.  **Apply Bayesian adjustment** to weights so that aggregated
    estimates match the official series
3.  **Return theme-specific weights** (e.g.,
    `weight_sidra_unemployment`)

**Planned themes:**

| Theme          | SIDRA Tables        | What’s Calibrated                      |
|----------------|---------------------|----------------------------------------|
| `unemployment` | Taxa de desocupacao | Unemployment rate matches IBGE exactly |
| `employment`   | Populacao ocupada   | Employment levels match IBGE exactly   |
| `labor_force`  | PEA, PNEA           | Labor force participation matches IBGE |
| `income`       | Rendimento habitual | Income aggregates match IBGE           |
| `custom`       | User-provided       | User supplies their own target series  |

**Why separate from main function?** Different indicators require
different calibrations. The base `weight_monthly` is self-consistent and
general-purpose, while theme-specific weights are optimized for matching
specific published series.

------------------------------------------------------------------------

## Authors

- **Marcos Hecksher** - Original methodology and Stata implementation
- **Rogerio Barbosa** - R package author

## Documentation

- 📖 **[Online
  Documentation](https://antrologos.github.io/mensalizePNADC/)** — Full
  pkgdown website with function reference and vignettes
- 📖 **[Getting Started
  Vignette](https://antrologos.github.io/mensalizePNADC/articles/getting-started.html)**
  — Detailed algorithm explanation with diagrams and examples
- After installing, run
  [`vignette("getting-started", package = "mensalizePNADC")`](https://antrologos.github.io/mensalizePNADC/articles/getting-started.md)
  for the rendered version

## References

- [IBGE PNADC
  Documentation](https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html)
- Hecksher, M. (2024). Mensalizacao da PNADC. Working paper.

## License

MIT
