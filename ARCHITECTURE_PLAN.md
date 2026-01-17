# PNADCperiods - Architecture Documentation

This document describes the implemented architecture of the PNADCperiods package (v2.0.0).

## Package Overview

**Package name:** `PNADCperiods`
**Purpose:** Identify reference periods (months, fortnights, weeks) in Brazil's PNADC survey data and optionally calibrate weights for sub-quarterly analysis.

## API Design

### Two Main Functions

| Function | Purpose |
|----------|---------|
| `pnadc_identify_periods()` | Build crosswalk with month/fortnight/week identification |
| `pnadc_apply_periods()` | Apply crosswalk to data + optional weight calibration |

### Supporting Functions (Exported)

| Function | Purpose |
|----------|---------|
| `identify_reference_month()` | Standalone month identification |
| `identify_reference_fortnight()` | Standalone fortnight identification |
| `identify_reference_week()` | Standalone week identification |
| `fetch_monthly_population()` | Fetch population from SIDRA API |
| `validate_pnadc()` | Input validation |
| `calibrate_monthly_weights()` | Legacy monthly calibration (use `pnadc_apply_periods()` instead) |

## Crosswalk Structure

The crosswalk is a `data.table` at household-quarter level:

| Column | Type | Description |
|--------|------|-------------|
| `Ano` | integer | Survey year |
| `Trimestre` | integer | Quarter (1-4) |
| `UPA` | integer | Primary sampling unit |
| `V1008` | integer | Household sequence |
| `V1014` | integer | Panel group (1-8) |
| `ref_month` | Date | Reference month (1st of month) |
| `ref_month_in_quarter` | integer | Position in quarter (1, 2, 3) or NA |
| `ref_month_yyyymm` | integer | YYYYMM format |
| `determined_month` | logical | TRUE if month was determined |
| `ref_fortnight` | Date | Reference fortnight (1st or 16th) |
| `ref_fortnight_in_quarter` | integer | Position in quarter (1-6) or NA |
| `ref_fortnight_yyyyff` | integer | YYYYFF format (1-24 per year) |
| `determined_fortnight` | logical | TRUE if fortnight was determined |
| `ref_week` | Date | Reference week (Monday) |
| `ref_week_in_quarter` | integer | Position in quarter (1-14) or NA |
| `ref_week_yyyyww` | integer | ISO YYYYWW format |
| `determined_week` | logical | TRUE if week was determined |

**Join keys:** `Ano`, `Trimestre`, `UPA`, `V1008`, `V1014`

## Determination Rates

| Period | Rate | Reason |
|--------|------|--------|
| Month | ~97% | Aggregates at UPA-V1014 level across ALL quarters (panel design) |
| Fortnight | ~2-5% | Cannot aggregate across quarters; within-quarter constraints only |
| Week | ~1-2% | Cannot aggregate across quarters; within-quarter constraints only |

**Key insight:** Only month identification benefits from stacking data across quarters. Fortnights and weeks are determined solely from birthday constraints within a single quarter.

## File Structure

```
R/
├── pnadc-identify-periods.R      # Main: pnadc_identify_periods()
├── pnadc-apply-periods.R         # Main: pnadc_apply_periods()
├── identify-reference-month.R    # Core month algorithm
├── identify-reference-fortnight.R # Core fortnight algorithm
├── identify-reference-week.R     # Core week algorithm
├── calibrate-weights.R           # Legacy calibration
├── fetch-sidra-population.R      # SIDRA API for population
├── smooth-aggregates.R           # Smoothing algorithm
├── utils-dates.R                 # Fast date utilities
├── utils-validation.R            # Input validation
└── PNADCperiods-package.R        # Package docs + globalVariables
```

## Algorithm Summary

### Month Identification (High Rate)

1. Calculate valid interview Saturdays using IBGE timing rules
2. Apply birthday constraints to narrow date range per person
3. Convert dates to month positions (1, 2, 3 in quarter)
4. **Aggregate at UPA-V1014 level ACROSS ALL QUARTERS** (key optimization)
5. Dynamic exception detection for edge cases
6. Determine: if min_position == max_position → determined

### Fortnight/Week Identification (Low Rate)

1. Same steps 1-3 as month
2. **Aggregate at household level WITHIN QUARTER ONLY**
3. No cross-quarter aggregation (positions not consistent across visits)
4. Determine: if min_position == max_position → determined

## Calibration Pipeline

When `pnadc_apply_periods(..., calibrate = TRUE)`:

1. Join data with crosswalk
2. Fetch population targets from SIDRA (or use provided targets)
3. Create hierarchical calibration cells (4 levels)
4. Iterative reweighting within anchor period (quarter or year)
5. Calibrate to external population totals
6. Smooth weights (optional)

**Anchor types:**
- `anchor = "quarter"`: Preserve quarterly totals, redistribute to sub-periods
- `anchor = "year"`: Preserve annual totals (for annual PNADC data)

## Dependencies

**Required:**
- `data.table` (>= 1.14.0) - Core data manipulation
- `checkmate` (>= 2.0.0) - Input validation

**Suggested:**
- `sidrar` - SIDRA API access for population data
- `testthat` - Testing
- `knitr`, `rmarkdown` - Vignettes
- `pkgdown` - Website

## Workflow Examples

### Standard Monthly Analysis
```r
crosswalk <- pnadc_identify_periods(pnadc_stacked)
result <- pnadc_apply_periods(pnadc_2023, crosswalk,
                              weight_var = "V1028",
                              anchor = "quarter")
```

### Annual Data
```r
result <- pnadc_apply_periods(pnadc_annual, crosswalk,
                              weight_var = "V1032",
                              anchor = "year")
```

### Crosswalk Only (No Calibration)
```r
result <- pnadc_apply_periods(pnadc_2023, crosswalk,
                              calibrate = FALSE)
```
