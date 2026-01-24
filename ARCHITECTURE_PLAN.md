# PNADCperiods - Architecture Documentation

This document describes the implemented architecture of the PNADCperiods package.

## Package Overview

**Package name:** `PNADCperiods`
**Purpose:** Identify reference periods (months, fortnights, weeks) in Brazil's PNADC survey data and calibrate weights for sub-quarterly analysis.

## API Design

### Core Functions

| Function | Purpose |
|----------|---------|
| `pnadc_identify_periods()` | Build crosswalk with nested month/fortnight/week identification |
| `pnadc_apply_periods()` | Apply crosswalk to data + adaptive weight calibration |

### Experimental Extension

| Function | Purpose |
|----------|---------|
| `pnadc_experimental_periods()` | Probabilistic + UPA aggregation strategies for higher determination rates |

### Supporting Functions

| Function | Purpose |
|----------|---------|
| `fetch_monthly_population()` | Fetch population from SIDRA API (table 6022) |
| `validate_pnadc()` | Input validation with detailed error reporting |

## Nested Identification Strategy

The package uses a **nested identification hierarchy**:

```
Month → Fortnight → Week
```

**Key principle:** A finer period can only be determined if its parent period is determined.

| Phase | Period | Requires | Aggregation Level |
|-------|--------|----------|-------------------|
| 1 | Month | - | UPA-V1014 across ALL quarters |
| 2 | Fortnight | Month determined | Household within quarter |
| 3 | Week | Fortnight determined | Household within quarter |

This nesting ensures logical consistency: you cannot know which week without knowing which fortnight, and you cannot know which fortnight without knowing which month.

## Crosswalk Structure

The crosswalk is a `data.table` at household-quarter level:

| Column | Type | Description |
|--------|------|-------------|
| `Ano` | integer | Survey year |
| `Trimestre` | integer | Quarter (1-4) |
| `UPA` | integer | Primary sampling unit |
| `V1008` | integer | Household sequence |
| `V1014` | integer | Panel group (1-8) |
| `ref_month_start` | Date | Sunday of first IBGE week of the month |
| `ref_month_end` | Date | Saturday of last IBGE week of the month |
| `ref_month_in_quarter` | integer | Position in quarter (1, 2, 3) or NA |
| `ref_month_yyyymm` | integer | YYYYMM format (e.g., 202301) |
| `ref_month_weeks` | integer | Number of IBGE weeks in month (always 4) |
| `determined_month` | logical | TRUE if month was determined |
| `ref_fortnight_start` | Date | Sunday of first IBGE week of the fortnight |
| `ref_fortnight_end` | Date | Saturday of last IBGE week of the fortnight |
| `ref_fortnight_in_quarter` | integer | Position in quarter (1-6) or NA |
| `ref_fortnight_yyyyff` | integer | YYYYFF format (1-24 per year) |
| `determined_fortnight` | logical | TRUE if fortnight was determined |
| `ref_week_start` | Date | Sunday of the IBGE week |
| `ref_week_end` | Date | Saturday of the IBGE week |
| `ref_week_in_quarter` | integer | Position in quarter (1-12) or NA |
| `ref_week_yyyyww` | integer | IBGE YYYYWW format |
| `determined_week` | logical | TRUE if week was determined |

**Join keys:** `Ano`, `Trimestre`, `UPA`, `V1008`, `V1014`

## Determination Rates

| Period | Rate | Reason |
|--------|------|--------|
| Month | ~97% | Aggregates at UPA-V1014 level across ALL quarters (panel design) |
| Fortnight | ~7% | Nested under month; household-level within quarter only |
| Week | ~1.5% | Nested under fortnight; household-level within quarter only |

**Key insight:** Only month identification benefits from stacking data across quarters. Fortnights and weeks are determined solely from birthday constraints within a single quarter, AND require their parent period to be determined first.

## File Structure

```
R/
├── pnadc-identify-periods.R         # pnadc_identify_periods() - nested 4-phase algorithm
├── pnadc-apply-periods.R            # pnadc_apply_periods() - adaptive calibration + smoothing
├── experimental-period-identification.R  # pnadc_experimental_periods()
├── identify-reference-month.R       # Core month algorithm with dynamic exceptions (internal)
├── identify-reference-fortnight.R   # Core fortnight algorithm (internal)
├── identify-reference-week.R        # Core week algorithm + technical stop handling (internal)
├── fetch-sidra-population.R         # fetch_monthly_population() - SIDRA API
├── utils-dates.R                    # Fast IBGE date utilities (lookup tables, week/fortnight functions)
├── utils-validation.R               # validate_pnadc() + internal helpers
└── PNADCperiods-package.R           # Package docs + globalVariables
```

## Algorithm Details

### Phase 1: Month Identification (High Rate)

1. Calculate valid interview Saturdays using IBGE "Parada Técnica" rules
2. Apply birthday constraints to narrow date range per person
3. Convert dates to month positions (1, 2, 3 in quarter)
4. **Aggregate at UPA-V1014 level ACROSS ALL QUARTERS** (key optimization)
5. Dynamic exception detection for edge cases (relaxed Saturday rules)
6. Determine: if `min_position == max_position` → determined

### Phase 2: Fortnight Identification (Nested, Low Rate)

**Precondition:** Only processes observations with `determined_month = TRUE`

1. Constrain fortnight search to within the identified month:
   - Month 1 → fortnights 1-2
   - Month 2 → fortnights 3-4
   - Month 3 → fortnights 5-6
2. Apply birthday constraints within constrained range
3. **Aggregate at household level `(Ano, Trimestre, UPA, V1008)` WITHIN QUARTER**
4. Dynamic exception detection
5. Determine: if `min_position == max_position` → determined

### Phase 3: Week Identification (Nested, Low Rate)

**Precondition:** Only processes observations with `determined_fortnight = TRUE`

1. Constrain week search to within the identified fortnight
2. Apply birthday constraints within constrained range
3. **Aggregate at household level `(Ano, Trimestre, UPA, V1008)` WITHIN QUARTER**
4. Dynamic exception detection
5. **Technical Stop Handling:**
   - Rule 3.1: Quarter boundary stops → assign to week 12 (last week of quarter)
   - Rule 3.2: Within-quarter stops → use household consensus when available
   - Rule 3.3: Fallback → first valid week after technical stop
6. Determine: if `min_position == max_position` → determined

### Phase 4: Build Crosswalk

Combines results from all phases into the final crosswalk data.table with:
- IBGE period boundaries (start/end dates as Sunday-Saturday)
- Position codes (in_quarter)
- YYYYMM/YYYYFF/YYYYWW integer codes
- Determination flags

## Experimental Strategies

The `pnadc_experimental_periods()` function provides three strategies for improving determination rates beyond the strict algorithm:

| Strategy | Description |
|----------|-------------|
| `probabilistic` | For 2-period ranges, assigns based on which period contains most of the date interval. Works at UPA-V1014 level for months, household level for fortnights/weeks. |
| `upa_aggregation` | Extends strictly identified periods to other observations in same UPA/UPA-V1014 when a consensus exists. |
| `both` | Sequentially applies probabilistic first, then UPA aggregation. Guarantees identification rate >= max of individual strategies. |

**All strategies enforce proper nesting:** experimental fortnights require identified months (strict or experimental), experimental weeks require identified fortnights (strict or experimental).

### Strategy Details

- **Probabilistic:** Calculates confidence as the proportion of the date interval falling within the assigned period. Only assigns when confidence >= threshold (default 0.9). Implements same dynamic exception detection as strict algorithm.
- **UPA Aggregation:** Uses UPA-V1014 level for months (panel design), UPA level for fortnights/weeks (all households in same UPA interviewed in same period within quarter). Requires proportion >= threshold (default 0.5).
- **Both:** Strategies operate independently - probabilistic captures narrow ranges, UPA aggregation extends based on strict consensus. Result is union of both.

### Output Columns

When `include_derived = TRUE` (default), output is directly compatible with `pnadc_apply_periods()`:

| Column | Description |
|--------|-------------|
| `ref_month_exp`, `ref_fortnight_exp`, `ref_week_exp` | Experimental positions (1-3, 1-6, 1-12) |
| `ref_month_exp_confidence`, etc. | Confidence scores (0-1) |
| `probabilistic_assignment` | TRUE if any period assigned experimentally |
| `determined_month`, `determined_fortnight`, `determined_week` | Combined strict + experimental flags |
| All derived columns from strict crosswalk | IBGE boundaries, YYYYMM/FF/WW codes |

## Calibration Pipeline

When `pnadc_apply_periods(..., calibrate = TRUE)`:

1. Join data with crosswalk
2. Fetch population targets from SIDRA (or use provided targets)
3. Create hierarchical calibration cells (auto-selected based on period)
4. Iterative reweighting within anchor period (quarter or year)
5. Calibrate to external population totals (FULL Brazilian population)
6. Apply smoothing (period-specific)

### Period-Specific Calibration Settings

| Period | Cell Levels | Smoothing | Min Cell Size |
|--------|-------------|-----------|---------------|
| Month | 4 (full hierarchy) | 3-period rolling mean | 10 |
| Fortnight | 2 (age + region) | 7-period rolling mean | 10 |
| Week | 1 (age only) | None | 10 |

Cell levels are auto-selected based on `calibration_unit` parameter. Levels with cells smaller than `min_cell_size` are skipped.

### Calibration Cell Hierarchy

1. **celula1:** Age groups (0-13, 14-29, 30-59, 60+)
2. **celula2:** Post-stratum group (posest_sxi) + age
3. **celula3:** State (UF) + celula2
4. **celula4:** Post-stratum (posest) + celula2

### Anchor Types

- `anchor = "quarter"`: Preserve quarterly totals, redistribute to sub-periods (use with V1028)
- `anchor = "year"`: Preserve annual totals (for annual PNADC data with V1032 weights)

### Output Weight Columns

- `weight_monthly`: Calibrated monthly weight (when `calibration_unit = "month"`)
- `weight_fortnight`: Calibrated fortnight weight (when `calibration_unit = "fortnight"`)
- `weight_weekly`: Calibrated weekly weight (when `calibration_unit = "week"`)

## Performance Optimizations

The package is optimized for large datasets (~450,000 rows/sec):

1. **Lookup tables:** Pre-computed date tables for 20x faster date creation vs `ISOdate()`
2. **Integer arithmetic:** ISO week calculations use pure integer math (300x faster than `data.table::isoweek()`)
3. **data.table threading:** Enabled via `setDTthreads(0)` at package load
4. **Vectorized operations:** All calculations use vectorized data.table syntax
5. **Efficient joins:** data.table binary joins instead of `merge()`

## Dependencies

**Required:**
- `data.table` (>= 1.14.0) - Core data manipulation
- `checkmate` (>= 2.0.0) - Input validation
- `sidrar` (>= 0.2.9) - SIDRA API access for population data

**Suggested:**
- `dplyr` - Alternative data manipulation
- `haven` - Read SAS/SPSS/Stata files
- `testthat` (>= 3.0.0) - Testing
- `knitr`, `rmarkdown` - Vignettes
- `pkgdown` - Website
- `ggplot2`, `scales` - Visualization

## Workflow Examples

### Standard Monthly Analysis
```r
# Build crosswalk from stacked multi-year data (maximizes determination rate)
crosswalk <- pnadc_identify_periods(pnadc_stacked)

# Apply to specific quarter with calibration
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

### Experimental Period Assignment
```r
# Build standard crosswalk
crosswalk <- pnadc_identify_periods(pnadc_data)

# Apply experimental strategies for additional assignments
crosswalk_exp <- pnadc_experimental_periods(
  crosswalk,
  pnadc_data,
  strategy = "both",  # probabilistic + UPA aggregation
  confidence_threshold = 0.9,
  upa_proportion_threshold = 0.5,
  include_derived = TRUE  # Output compatible with pnadc_apply_periods()
)

# Use directly with calibration
result <- pnadc_apply_periods(pnadc_data, crosswalk_exp,
                              weight_var = "V1028",
                              anchor = "quarter")

# Filter to strict-only if needed
strict_only <- crosswalk_exp[probabilistic_assignment == FALSE | is.na(probabilistic_assignment)]
```

## Required PNADC Variables

### For Period Identification
- `Ano` - Survey year
- `Trimestre` - Quarter (1-4)
- `UPA` - Primary sampling unit
- `V1008` - Household sequence number
- `V1014` - Panel group (1-8)
- `V2008` - Birth day (1-31, 99=unknown)
- `V20081` - Birth month (1-12, 99=unknown)
- `V20082` - Birth year
- `V2009` - Age

### For Weight Calibration (additional)
- `V1028` - Quarterly weight (for quarterly data)
- `V1032` - Annual weight (for annual data)
- `UF` - State code
- `posest` - Post-stratum
- `posest_sxi` - Post-stratum group
