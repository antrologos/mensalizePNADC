## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----monthly-weights----------------------------------------------------------
# # Load full data with all required variables
# pnadc_full <- fread("pnadc_stacked_full.csv")
# 
# # Step 1: Build crosswalk
# crosswalk <- pnadc_identify_periods(pnadc_full, verbose = TRUE)
# 
# # Step 2: Apply crosswalk and calibrate weights
# result <- pnadc_apply_periods(pnadc_full, crosswalk,
#                                weight_var = "V1028",
#                                anchor = "quarter",
#                                calibrate = TRUE,
#                                verbose = TRUE)
# 
# # Use weight_monthly for monthly estimates
# monthly_pop <- result[, .(
#   population = sum(weight_monthly, na.rm = TRUE)
# ), by = ref_month_yyyymm]

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

## ----annual-workflow----------------------------------------------------------
# # Step 1: Create crosswalk from quarterly data
# # (birthday variables are in quarterly data, not annual)
# quarterly_data <- fread("pnadc_quarterly_stacked.csv")
# crosswalk <- pnadc_identify_periods(quarterly_data)
# 
# # Step 2: Load annual data with income variables
# annual_data <- fread("pnadc_annual_visit1_stacked.csv")
# 
# # Step 3: Apply crosswalk and calibrate annual weights
# result <- pnadc_apply_periods(
#   annual_data, crosswalk,
#   weight_var = "V1032",
#   anchor = "year",
#   calibrate = TRUE,
#   verbose = TRUE
# )
# 
# # Step 4: Use weight_monthly for monthly income/poverty analysis
# monthly_income <- result[, .(
#   mean_income = weighted.mean(vd5008, weight_monthly, na.rm = TRUE)
# ), by = ref_month_yyyymm]

## ----modular------------------------------------------------------------------
# # Step 1: Just identify reference months
# months <- identify_reference_month(pnadc)
# 
# # Step 2: Check determination rate by quarter
# months[, .(
#   total = .N,
#   determined = sum(!is.na(ref_month_in_quarter)),
#   rate = round(mean(!is.na(ref_month_in_quarter)) * 100, 1)
# ), by = .(Ano, Trimestre)]
# 
# # Step 3: Validate input data
# validation <- validate_pnadc(pnadc, stop_on_error = FALSE)
# 
# # Step 4: Fetch population data from SIDRA-IBGE manually
# pop_data <- fetch_monthly_population(verbose = TRUE)
# 
# # Step 5: Calibrate weights separately
# calibrated <- calibrate_monthly_weights(merged_data, pop_data)

## ----check-rates--------------------------------------------------------------
# crosswalk[, .(rate = mean(!is.na(ref_month_in_quarter))), by = .(Ano, Trimestre)]

