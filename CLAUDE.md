# CLAUDE.md

## Project Overview

R package (`PNADCperiods`) that converts Brazil's quarterly PNADC survey data into sub-quarterly time series (monthly, fortnightly, weekly) with optional weight calibration.

**Authors:** Marcos Hecksher (methodology) | Rogerio Barbosa (R package)
**Repository:** https://github.com/antrologos/PNADCperiods

## Quick Reference

```r
library(PNADCperiods)
crosswalk <- pnadc_identify_periods(pnadc_stacked)
result <- pnadc_apply_periods(pnadc_2023, crosswalk, weight_var = "V1028", anchor = "quarter")
```

### Key Functions

| Function | Purpose |
|----------|---------|
| `pnadc_identify_periods()` | Build crosswalk (month/fortnight/week) |
| `pnadc_apply_periods()` | Apply crosswalk + calibrate weights |
| `identify_reference_month()` | Month identification (standalone) |
| `identify_reference_fortnight()` | Fortnight identification (standalone) |
| `identify_reference_week()` | Week identification (standalone) |
| `fetch_monthly_population()` | Fetch population from SIDRA API |
| `validate_pnadc()` | Input validation |
| `pnadc_experimental_periods()` | Experimental probabilistic period assignment |
| `combine_period_crosswalks()` | Merge strict and experimental crosswalks |

### Required Variables

- **Identification:** `Ano`, `Trimestre`, `UPA`, `V1008`, `V1014`, `V2008`, `V20081`, `V20082`, `V2009`
- **Calibration:** add `V1028`/`V1032`, `UF`, `posest`, `posest_sxi`

### Determination Rates

| Period | Rate | Notes |
|--------|------|-------|
| Month | ~97% | Aggregates across quarters at UPA-V1014 level (panel design) |
| Fortnight | ~6% | Within-quarter only; cannot aggregate across quarters |
| Week | ~1.5% | Within-quarter only; cannot aggregate across quarters |

**Always use stacked multi-quarter data for best month determination rate.**

## Development Workflow

```r
devtools::document("PNADCperiods")  # Update NAMESPACE and man/
devtools::check("PNADCperiods")     # R CMD check
devtools::test("PNADCperiods")      # Run tests
devtools::install("PNADCperiods", dependencies = FALSE)  # Install locally
pkgdown::build_site("PNADCperiods")  # Rebuild docs locally
```

### GitHub Pages Deployment

**CRITICAL:** The pkgdown site publishes from the `refactor/pnadc-periods` branch, NOT master/main.

To update the online documentation:
```bash
cd "D:\Dropbox\Artigos\mensalizacao_pnad\PNADCperiods"
pkgdown::build_site()  # Build locally first
git add .
git commit -m "Update documentation"
git push origin refactor/pnadc-periods  # Push to this branch - site rebuilds automatically
```

**Do NOT merge to master** - the GitHub Actions workflow triggers on push to `refactor/pnadc-periods`.

Site URL: https://antrologos.github.io/PNADCperiods/

## Source Files

| File | Purpose |
|------|---------|
| `pnadc-identify-periods.R` | Main crosswalk builder (month + fortnight + week) |
| `pnadc-apply-periods.R` | Apply crosswalk + unified calibration (includes internal smoothing) |
| `identify-reference-month.R` | Core month algorithm with dynamic exceptions |
| `identify-reference-fortnight.R` | Fortnight (quinzena) identification |
| `identify-reference-week.R` | Week identification |
| `calibrate-weights.R` | Legacy monthly calibration |
| `fetch-sidra-population.R` | Fetch population from SIDRA API |
| `experimental-period-identification.R` | Experimental probabilistic strategies |
| `utils-dates.R` | Fast date utilities (lookup tables, ISO week functions) |
| `utils-validation.R` | Input validation |

## Algorithm Summary

1. Calculate valid interview Saturdays (IBGE timing rules)
2. Apply birthday constraints to narrow date range
3. Convert dates to month/fortnight/week positions
4. Aggregate constraints at UPA-panel level **across all quarters** (months only)
5. Dynamic exception detection for edge cases
6. Final determination: if min == max → determined

**Critical:** Months aggregate by `.(UPA, V1014)` across all quarters. Fortnights/weeks aggregate by `.(Ano, Trimestre, UPA, V1008)` within quarter only.

### Weight Calibration

All time periods calibrate to **FULL Brazilian population** (not divided). Hierarchical raking is simplified for sparse data:

| Period | Cell Levels | Smoothing |
|--------|-------------|-----------|
| Month | 4 (full hierarchy) | 3-period rolling mean |
| Fortnight | 2 (age + region) | 7-period rolling mean |
| Week | 1 (age only) | None |

## Important Notes for Claude Code

### Shell Environment (CRITICAL)

**Claude Code uses Unix bash shell even on Windows.** The shell is `/usr/bin/bash`, NOT Windows CMD or PowerShell.

**Always use Unix commands, NOT Windows commands:**

| Task | WRONG (Windows) | CORRECT (Unix) |
|------|-----------------|----------------|
| Delete file | `del file.txt` | `rm file.txt` |
| List files | `dir` | `ls` |
| Copy file | `copy a.txt b.txt` | `cp a.txt b.txt` |
| Move file | `move a.txt b.txt` | `mv a.txt b.txt` |
| Create dir | `mkdir /p path` | `mkdir -p path` |
| Remove dir | `rmdir /s /q dir` | `rm -rf dir` |

Using Windows commands causes: `Exit code 127: command not found`

### Running R Scripts on Windows

**R location:** `C:\Program Files\R\R-4.5.0\bin\Rscript.exe`

To run R scripts from the command line:
```bash
"C:\\Program Files\\R\\R-4.5.0\\bin\\Rscript.exe" script.R
```

**NEVER use `-e` flag for complex R code** - causes segfaults.

```bash
# WRONG
Rscript -e "complex code..."

# CORRECT - write to file first
# 1. Write script to temp.R
# 2. Run: "C:\\Program Files\\R\\R-4.5.0\\bin\\Rscript.exe" temp.R
# 3. Clean up: rm temp.R
```

### Data Quality Rules

**NEVER use simulated or placeholder data** in vignettes, figures, or examples. All figures and analyses must use real PNADC data or real official statistics. If external validation data is needed (e.g., IBGE official poverty estimates), fetch it from official sources or clearly state that validation requires user-provided data.

**NEVER use "Expected output" sections** in vignettes or documentation. All code examples should either:
1. Show actual outputs pre-computed from real data (for eval=FALSE chunks), or
2. Be executable with real data (for eval=TRUE chunks)

If output cannot be shown (e.g., depends on user's data), omit the output section entirely rather than showing hypothetical results.

### Git Repository Location

**CRITICAL:** The git repository is inside the package directory, NOT the project root.

```
D:\Dropbox\Artigos\mensalizacao_pnad\          # Project root (NOT a git repo)
└── PNADCperiods\                            # Git repository is HERE
    ├── .git\
    ├── R\
    ├── tests\
    └── ...
```

**Always use the package directory for git commands:**
```bash
# WRONG - causes exit codes 128/129
cd "D:\Dropbox\Artigos\mensalizacao_pnad" && git status

# CORRECT
cd "D:\Dropbox\Artigos\mensalizacao_pnad\PNADCperiods" && git status
```

Common git errors when using wrong directory:
- Exit code 128: `fatal: not a git repository`
- Exit code 129: `warning: Not a git repository`

### Common Issues

1. **Low determination rate** → Stack 2+ years of data
2. **Missing columns** → Use `validate_pnadc()` to check
3. **SIDRA API errors** → Check internet; API has rate limits
4. **Git errors (exit 128/129)** → Use `PNADCperiods/` directory, not project root
5. **Dropbox file locking** → If `devtools::document()` fails with "Invalid argument" error when writing to vignettes, pause Dropbox sync. All vignettes have `purl = FALSE` to prevent .R file creation, but Dropbox can still interfere.

### Local Data Paths (Rogerio's machine)

```
D:/Dropbox/Bancos_Dados/PNADC/
├── Trimestral/Dados/           # Quarterly data (pnadc_YYYY-Nq.fst)
└── Anual/visitas/              # Annual visit data (pnadc_YYYY_visitaN.fst)
```

**File naming conventions:**
- Quarterly: `pnadc_2019-1q.fst`, `pnadc_2019-2q.fst`, etc.
- Annual: `pnadc_2019_visita1.fst`, `pnadc_2020_visita5.fst`, etc.

**Visit selection for annual data:**
- 2015-2019: Visit 1
- 2020-2021: Visit 5 (COVID period - visit 1 not available)
- 2022-2024: Visit 1

### Vignette Precompute Scripts

**CRITICAL:** Vignette precompute scripts and their outputs must NOT be inside the package folder.

```
D:\Dropbox\Artigos\mensalizacao_pnad\
├── code\                              # Precompute scripts go HERE
│   ├── precompute_getting_started.R
│   ├── precompute_download_prepare.R
│   ├── generate_how_it_works_figures.R
│   └── ...
├── output\vignette\                   # Precomputed outputs go HERE
│   ├── figures\                       # Generated figures
│   ├── tables\                        # Generated tables
│   ├── getting_started_crosswalk.rds  # Saved R objects
│   └── *.txt                          # Text outputs for vignettes
└── PNADCperiods\                      # Package folder (git repo)
    └── vignettes\                     # Only .Rmd files and static assets
```

**Convention:**
- Scripts: `code/precompute_<vignette_name>.R` or `code/generate_<vignette_name>_figures.R`
- Outputs: `output/vignette/<vignette_name>_*.rds|txt|png`
- Figures copied to package: `PNADCperiods/vignettes/figures/<vignette_name>/`
