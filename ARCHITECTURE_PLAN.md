# PNADC Temporal Crosswalk - Architecture Reorganization Plan

## Design Decisions (Confirmed)

1. **Crosswalk contains BOTH month and week** - computed together
2. **No backwards compatibility** - old functions will be removed entirely
3. **Main function returns only crosswalk** - pure identification
4. **Calibration happens inside `apply_crosswalk()`** - by default, with option to disable
5. **Package name needs to change** - "mensalize" is too narrow

---

## Package Renaming Options

Since the crosswalk now contains both months AND weeks, "mensalizePNADC" no longer fits. Options:

| Option | Pros | Cons |
|--------|------|------|
| `pnadcTemporal` | Clear, neutral | Generic |
| `crosswalkPNADC` | Describes what it does | "Crosswalk" is jargon |
| `timePNADC` | Simple, clear | Maybe too simple |
| `pnadcTime` | Follows tidyverse style | - |
| `temporalizePNADC` | Keeps verb structure | Invented word |

**Recommendation:** `pnadcTemporal` or keep `mensalizePNADC` (monthly is still the primary use case, and the name is established)

---

## New Architecture: Two Functions Only

The entire user-facing API reduces to **two functions**:

### Function 1: `build_crosswalk()`

```r
build_crosswalk(

  pnadc_stacked,
  verbose = FALSE
)
```

**Purpose:** Pure identification algorithm. Returns universal crosswalk with BOTH month and week.

**Returns:**
```r
# Crosswalk data.table with columns:
# - UPA, V1014 (join keys)
# - ref_month (Date)
# - ref_month_in_quarter (1, 2, 3, or NA)
# - ref_month_yyyymm (integer)
# - ref_week (Date - Monday of week)
# - ref_week_in_quarter (1-14, or NA)
# - ref_week_yyyyww (integer, ISO 8601)
# - determined (logical - TRUE if month/week was determined)
```

**Notes:**
- Requires stacked multi-quarter data for high determination rate
- No weights, no external data, no side effects
- Pure function: same input → same output

### Function 2: `apply_crosswalk()`

```r
apply_crosswalk(
  data,
  crosswalk,
  weight_var = "V1028",           # "V1028" for quarterly, "V1032" for annual
  calibrate = TRUE,               # Calibrate weights by default
  calibration_unit = "month",     # "month" or "week"
  anchor = "quarter",             # "quarter" or "year" (for annual data)
  target_totals = NULL,           # NULL = auto-fetch from SIDRA
  smooth = TRUE,                  # Smooth calibrated weights
  verbose = FALSE
)
```

**Purpose:** Apply crosswalk to ANY PNADC dataset and optionally calibrate weights.

**Returns:** The input `data` with added columns:
- `ref_month`, `ref_month_in_quarter`, `ref_month_yyyymm`
- `ref_week`, `ref_week_in_quarter`, `ref_week_yyyyww`
- `weight_monthly` (if `calibrate = TRUE` and `calibration_unit = "month"`)
- `weight_weekly` (if `calibrate = TRUE` and `calibration_unit = "week"`)

**Internal pipeline (when `calibrate = TRUE`):**
1. Join data with crosswalk
2. Fetch population targets from SIDRA (if `target_totals = NULL`)
3. Create hierarchical calibration cells
4. Iterative reweighting (4 levels)
5. Calibrate to external totals
6. Smooth weights (if `smooth = TRUE`)

---

## Workflow Examples

### Workflow 1: Standard Monthly Analysis (Quarterly Data)
```r
library(pnadcTemporal)  # or mensalizePNADC

# Step 1: Build crosswalk from stacked data
crosswalk <- build_crosswalk(pnadc_stacked)

# Step 2: Apply to your dataset (calibration happens automatically)
result <- apply_crosswalk(pnadc_2023, crosswalk)

# Result has: ref_month, ref_week, weight_monthly
```

### Workflow 2: Annual Data
```r
# Same crosswalk works for annual data!
crosswalk <- build_crosswalk(pnadc_stacked)

# Apply with annual weight and yearly anchor
result <- apply_crosswalk(
  pnadc_annual,
  crosswalk,
  weight_var = "V1032",
  anchor = "year"
)
```

### Workflow 3: Weekly Analysis
```r
crosswalk <- build_crosswalk(pnadc_stacked)

result <- apply_crosswalk(
  pnadc_2023,
  crosswalk,
  calibration_unit = "week"
)
# Result has: ref_week, weight_weekly
```

### Workflow 4: Crosswalk Only (No Calibration)
```r
crosswalk <- build_crosswalk(pnadc_stacked)

result <- apply_crosswalk(
  pnadc_2023,
  crosswalk,
  calibrate = FALSE
)
# Result has: ref_month, ref_week (no weight columns)
```

### Workflow 5: Custom Population Targets
```r
crosswalk <- build_crosswalk(pnadc_stacked)

# User provides their own targets
my_targets <- data.table(
  ref_month_yyyymm = c(202301, 202302, ...),
  population = c(215000000, 215100000, ...)
)

result <- apply_crosswalk(
  pnadc_2023,
  crosswalk,
  target_totals = my_targets
)
```

---

## Internal Functions (Not Exported)

These exist but are not part of the public API:

### Identification Layer
- `identify_reference_month()` - Core monthly algorithm (8 steps)
- `identify_reference_week()` - Core weekly algorithm (derives from monthly)

### Calibration Layer
- `create_calibration_cells()` - Hierarchical cell creation
- `reweight_at_cell_level()` - Single level reweighting
- `calibrate_to_totals()` - Final calibration to external targets

### Population Layer
- `fetch_monthly_population()` - SIDRA API for monthly
- `derive_weekly_population()` - Distribute monthly to weeks

### Smoothing Layer
- `smooth_weights()` - Remove quarterly artifacts

### Utilities
- `validate_pnadc()` - Input validation
- Date utilities in `utils-dates.R`

---

## File Structure

```
R/
├── build-crosswalk.R          # Main entry: build_crosswalk()
├── apply-crosswalk.R          # Main entry: apply_crosswalk()
├── identify-reference-month.R # Internal: monthly algorithm
├── identify-reference-week.R  # Internal: weekly algorithm
├── calibrate-weights.R        # Internal: unified calibration
├── fetch-population.R         # Internal: SIDRA + weekly derivation
├── smooth-weights.R           # Internal: smoothing algorithm
├── utils-dates.R              # Internal: date utilities
├── utils-validation.R         # Internal: validation
└── zzz.R                       # Package startup
```

**Removed files:**
- `mensalizePNADC.R` - replaced by `build-crosswalk.R`
- `semanalizePNADC.R` - absorbed into `build-crosswalk.R`
- `mensalize-annual.R` - absorbed into `apply-crosswalk.R`
- `calibrate-weekly-weights.R` - merged into `calibrate-weights.R`
- `smooth-aggregates.R` - kept but maybe renamed

---

## NAMESPACE (Exports)

```r
# Only TWO exported functions
export(build_crosswalk)
export(apply_crosswalk)

# Optional: export utilities if useful standalone
export(fetch_monthly_population)
export(validate_pnadc)
```

---

## Crosswalk Structure Detail

The crosswalk is a `data.table` with these columns:

| Column | Type | Description |
|--------|------|-------------|
| `UPA` | integer | Primary sampling unit |
| `V1014` | integer | Panel group (1-8) |
| `ref_month` | Date | Reference month (1st of month) |
| `ref_month_in_quarter` | integer | Position in quarter (1, 2, 3) or NA |
| `ref_month_yyyymm` | integer | YYYYMM format (202301) |
| `ref_week` | Date | Reference week (Monday) |
| `ref_week_in_quarter` | integer | Position in quarter (1-14) or NA |
| `ref_week_yyyyww` | integer | ISO YYYYWW format (202301) |
| `determined` | logical | TRUE if reference period was determined |

**Join keys:** `UPA` + `V1014`

**Note:** The crosswalk is at the UPA-panel level, not individual level. One row per unique (UPA, V1014) combination across all quarters in the input data.

---

## Calibration Logic (Unified)

The unified calibration function handles all cases:

```r
calibrate_weights_internal <- function(
  data,
  unit = c("month", "week"),
  anchor = c("quarter", "year"),
  target_totals,
  smooth = TRUE
) {

  # Determine grouping variable based on anchor
  anchor_var <- if (anchor == "quarter") {
    c("Ano", "Trimestre")
  } else {
    "Ano"
  }

  # Determine target variable based on unit
  ref_var <- if (unit == "month") "ref_month_yyyymm" else "ref_week_yyyyww"

  # Step 1: Create hierarchical cells (SAME for all)
  data <- create_calibration_cells(data)

  # Step 2: Iterative reweighting at 4 levels
  # Key insight: only the anchor grouping changes
  for (cell_level in paste0("celula", 1:4)) {
    data <- reweight_at_cell_level(
      data,
      cell_var = cell_level,
      anchor_var = anchor_var,
      ref_var = ref_var
    )
  }

  # Step 3: Calibrate to external population totals
  data <- calibrate_to_totals(data, target_totals, ref_var)

  # Step 4: Smooth (optional)
  if (smooth) {
    data <- smooth_weights(data, unit)
  }

  return(data)
}
```

**Key insight:** The ONLY difference between quarterly/annual/weekly calibration is:
1. `anchor_var` - what period to preserve totals within
2. `ref_var` - what temporal unit to redistribute to

Everything else (cell creation, reweighting logic, smoothing) is identical.

---

## Documentation Plan

### Vignettes
1. **getting-started.Rmd** - Quick start with both functions
2. **working-with-annual-data.Rmd** - Annual data specifics
3. **weekly-analysis.Rmd** - Weekly temporal unit
4. **how-it-works.Rmd** - Algorithm deep-dive (update)
5. **applied-examples.Rmd** - Real-world examples (update)

### Function Documentation
- `build_crosswalk()` - Full roxygen2 with examples
- `apply_crosswalk()` - Full roxygen2 with examples for all use cases

### pkgdown
- Simplified reference (only 2 main functions)
- Clear workflow diagrams
- Migration guide from old API (for existing users)

---

## Implementation Phases

### Phase 1: Core Functions
1. Create `build_crosswalk()` that returns unified crosswalk (month + week)
2. Create `apply_crosswalk()` with calibration pipeline
3. Unify calibration logic into single internal function
4. Update `identify_reference_week()` to derive from monthly

### Phase 2: Cleanup
1. Delete old files: `mensalizePNADC.R`, `semanalizePNADC.R`, `mensalize-annual.R`
2. Delete `calibrate-weekly-weights.R` (merged)
3. Update NAMESPACE
4. Update DESCRIPTION (maybe rename package)

### Phase 3: Documentation
1. Rewrite all vignettes
2. Update roxygen2 for new functions
3. Update CLAUDE.md
4. Update README

### Phase 4: Testing
1. Write tests for `build_crosswalk()`
2. Write tests for `apply_crosswalk()` (all variants)
3. Integration tests

### Phase 5: Release
1. Bump to version 2.0.0 (or 1.0.0 if renaming package)
2. Write NEWS.md
3. Rebuild pkgdown site
4. Push to GitHub

---

## Open Questions

1. **Package name:** Keep `mensalizePNADC` or rename to `pnadcTemporal`?

2. **Smoothing for aggregates:** Keep `smooth_monthly_aggregates()` as separate export, or fold into the pipeline?

3. **Validation export:** Keep `validate_pnadc()` exported for users to check their data?

4. **Population fetch export:** Keep `fetch_monthly_population()` exported for users who want raw SIDRA data?
