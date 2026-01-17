## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----fortnight-week-identification--------------------------------------------
# # Build crosswalk with all period types
# crosswalk <- pnadc_identify_periods(pnadc_stacked, verbose = TRUE)
# 
# # Check determination rates
# crosswalk[, .(
#   n_obs = .N,
#   det_month = mean(determined_month, na.rm = TRUE),
#   det_fortnight = mean(determined_fortnight, na.rm = TRUE),
#   det_week = mean(determined_week, na.rm = TRUE)
# ), by = .(Ano, Trimestre)]
# 
# # For fortnight analysis, filter to determined observations
# fortnight_data <- crosswalk[determined_fortnight == TRUE]
# nrow(fortnight_data) / nrow(crosswalk)  # ~3%
# 
# # Apply crosswalk with fortnight calibration
# result <- pnadc_apply_periods(
#   pnadc_data, crosswalk,
#   weight_var = "V1028",
#   anchor = "quarter",
#   calibration_unit = "fortnight"
# )

## ----monthly-calibration------------------------------------------------------
# # Apply crosswalk and calibrate monthly weights
# result <- pnadc_apply_periods(pnadc_stacked, crosswalk,
#                                weight_var = "V1028",
#                                anchor = "quarter",
#                                calibration_unit = "month",  # default
#                                calibrate = TRUE)
# 
# # Use weight_monthly for monthly estimates
# monthly_pop <- result[, .(
#   population = sum(weight_monthly, na.rm = TRUE)
# ), by = ref_month_yyyymm]

## ----fortnight-weekly---------------------------------------------------------
# # Fortnight calibration
# result <- pnadc_apply_periods(pnadc_data, crosswalk,
#                                weight_var = "V1028",
#                                anchor = "quarter",
#                                calibration_unit = "fortnight")
# 
# # Weekly calibration
# result <- pnadc_apply_periods(pnadc_data, crosswalk,
#                                weight_var = "V1028",
#                                anchor = "quarter",
#                                calibration_unit = "week")

## ----keep-all-example---------------------------------------------------------
# # Build crosswalk and apply (returns all rows by default)
# crosswalk <- pnadc_identify_periods(pnadc_full)
# result <- pnadc_apply_periods(pnadc_full, crosswalk,
#                                weight_var = "V1028",
#                                anchor = "quarter")
# 
# nrow(result) == nrow(pnadc_full)  # TRUE - all rows returned
# sum(is.na(result$weight_monthly))  # ~3% of rows have NA weights
# 
# # Use na.rm = TRUE when aggregating
# monthly_pop <- result[, .(pop = sum(weight_monthly, na.rm = TRUE)), by = ref_month_yyyymm]
# 
# # Filter to determined rows for analysis
# result_determined <- result[!is.na(ref_month_in_quarter)]
# nrow(result_determined) < nrow(pnadc_full)  # TRUE (~97% of rows)

## ----smoothing-options--------------------------------------------------------
# # With smoothing (default) - recommended for most analyses
# result <- pnadc_apply_periods(
#   pnadc_data, crosswalk,
#   weight_var = "V1028",
#   anchor = "quarter",
#   smooth = TRUE  # default
# )
# 
# # Without smoothing - for shock analysis
# result_unsmoothed <- pnadc_apply_periods(
#   pnadc_data, crosswalk,
#   weight_var = "V1028",
#   anchor = "quarter",
#   smooth = FALSE
# )

## ----quarterly-apply----------------------------------------------------------
# # Build crosswalk from stacked quarterly data
# crosswalk <- pnadc_identify_periods(pnadc_stacked)
# 
# # Apply to quarterly data with monthly weights
# result_monthly <- pnadc_apply_periods(
#   pnadc_quarterly, crosswalk,
#   weight_var = "V1028",
#   anchor = "quarter",
#   calibration_unit = "month",
#   calibrate = TRUE
# )
# 
# # Apply with fortnight weights (for short-term analysis)
# result_fortnight <- pnadc_apply_periods(
#   pnadc_quarterly, crosswalk,
#   weight_var = "V1028",
#   anchor = "quarter",
#   calibration_unit = "fortnight",
#   calibrate = TRUE
# )
# 
# # Apply with weekly weights (for high-frequency monitoring)
# result_weekly <- pnadc_apply_periods(
#   pnadc_quarterly, crosswalk,
#   weight_var = "V1028",
#   anchor = "quarter",
#   calibration_unit = "week",
#   calibrate = TRUE
# )

## ----annual-apply-------------------------------------------------------------
# # Step 1: Build crosswalk from quarterly data
# quarterly_data <- read_fst("pnadc_quarterly_stacked.fst", as.data.table = TRUE)
# crosswalk <- pnadc_identify_periods(quarterly_data)
# 
# # Step 2: Load annual data with income variables
# annual_data <- read_fst("pnadc_2023_visita1.fst", as.data.table = TRUE)
# 
# # Step 3: Apply crosswalk with annual calibration
# result <- pnadc_apply_periods(
#   annual_data, crosswalk,
#   weight_var = "V1032",
#   anchor = "year",
#   calibration_unit = "month",
#   calibrate = TRUE
# )
# 
# # Use for monthly income/poverty analysis
# monthly_income <- result[!is.na(ref_month_in_quarter), .(
#   mean_income = weighted.mean(VD5008, weight_monthly, na.rm = TRUE)
# ), by = ref_month_yyyymm]

## ----modular-identification---------------------------------------------------
# # Identify reference months only
# months <- identify_reference_month(pnadc_stacked)
# 
# # Identify fortnights only
# fortnights <- identify_reference_fortnight(pnadc_stacked)
# 
# # Identify weeks only
# weeks <- identify_reference_week(pnadc_stacked)
# 
# # Check determination rates by quarter
# months[, .(
#   total = .N,
#   determined = sum(!is.na(ref_month_in_quarter)),
#   rate = round(mean(!is.na(ref_month_in_quarter)) * 100, 1)
# ), by = .(Ano, Trimestre)]

## ----modular-utilities--------------------------------------------------------
# # Validate input data before processing
# validation <- validate_pnadc(pnadc_stacked, stop_on_error = FALSE)
# 
# # Fetch population data from SIDRA-IBGE manually (for inspection)
# pop_data <- fetch_monthly_population(verbose = TRUE)
# head(pop_data)
# 
# # Note: pnadc_apply_periods() fetches population data automatically
# # when calibrate = TRUE. Manual fetching is only needed for inspection
# # or custom calibration workflows.

## ----check-rates--------------------------------------------------------------
# crosswalk[, .(rate = mean(!is.na(ref_month_in_quarter))), by = .(Ano, Trimestre)]

## ----complete-workflow--------------------------------------------------------
# library(PNADCperiods)
# library(data.table)
# 
# # =============================================================================
# # STEP 1: Load and stack quarterly PNADC data
# # =============================================================================
# 
# # Load quarterly data (recommend 2+ years for good determination rates)
# # Real data should be loaded from your files, e.g.:
# # files <- list.files("path/to/data", pattern = "pnadc_.*\\.fst$", full.names = TRUE)
# # pnadc_stacked <- rbindlist(lapply(files, fst::read_fst, as.data.table = TRUE))
# 
# # Required columns for identification:
# required_id <- c("Ano", "Trimestre", "UPA", "V1008", "V1014",
#                  "V2003", "V2008", "V20081", "V20082", "V2009")
# 
# # Additional columns for calibration:
# required_weights <- c("V1028", "UF", "posest", "posest_sxi")
# 
# # Additional columns for analysis (labor force variables):
# analysis_vars <- c("VD4001", "VD4002")  # Labor force status, occupation status
# 
# # =============================================================================
# # STEP 2: Validate input data
# # =============================================================================
# 
# validation <- validate_pnadc(pnadc_stacked, stop_on_error = FALSE)
# print(validation)
# 
# # Check data coverage
# pnadc_stacked[, .N, by = .(Ano, Trimestre)][order(Ano, Trimestre)]
# 
# # =============================================================================
# # STEP 3: Build crosswalk (identify reference periods)
# # =============================================================================
# 
# # This identifies months, fortnights, and weeks for all observations
# crosswalk <- pnadc_identify_periods(pnadc_stacked, verbose = TRUE)

## ----complete-workflow-step3b, eval=FALSE-------------------------------------
# # Check determination rates by year
# crosswalk[, .(
#   n = .N,
#   det_month = round(mean(determined_month, na.rm = TRUE) * 100, 1),
#   det_fortnight = round(mean(determined_fortnight, na.rm = TRUE) * 100, 1),
#   det_week = round(mean(determined_week, na.rm = TRUE) * 100, 1)
# ), by = Ano]

## ----complete-workflow-step4, eval=FALSE--------------------------------------
# # =============================================================================
# # STEP 4: Apply crosswalk and calibrate monthly weights
# # =============================================================================
# 
# result <- pnadc_apply_periods(
#   pnadc_stacked,
#   crosswalk,
#   weight_var = "V1028",
#   anchor = "quarter",
#   calibration_unit = "month",
#   calibrate = TRUE,
#   keep_all = TRUE,  # Keep indeterminate observations (with NA weights)
#   verbose = TRUE
# )
# 
# # Verify population totals match SIDRA
# result[!is.na(weight_monthly), .(
#   pop_millions = sum(weight_monthly) / 1e6
# ), by = ref_month_yyyymm][order(ref_month_yyyymm)]
# 
# # =============================================================================
# # STEP 5: Compute monthly estimates
# # =============================================================================
# 
# # Calculate monthly unemployment rate
# monthly_labor <- result[!is.na(ref_month_in_quarter), .(
#   labor_force = sum(weight_monthly * (VD4001 == 1), na.rm = TRUE),
#   employed = sum(weight_monthly * (VD4002 == 1), na.rm = TRUE),
#   n_obs = .N
# ), by = .(ref_month_yyyymm, ref_month)]
# 
# monthly_labor[, unemployment_rate := (labor_force - employed) / labor_force * 100]
# 
# # Check sample sizes
# summary(monthly_labor$n_obs)  # Should be ~65,000+ per month
# 
# # =============================================================================
# # STEP 6: Visualize results
# # =============================================================================
# 
# # Basic plot (requires ggplot2)
# # library(ggplot2)
# # ggplot(monthly_labor, aes(x = ref_month, y = unemployment_rate)) +
# #   geom_line() +
# #   geom_point(size = 1) +
# #   labs(title = "Monthly Unemployment Rate - Brazil",
# #        subtitle = "From PNADC quarterly data using PNADCperiods",
# #        x = NULL, y = "Unemployment Rate (%)") +
# #   theme_minimal()

