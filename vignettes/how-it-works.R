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
# # Run mensalization with weight computation
# result <- mensalizePNADC(pnadc_full, compute_weights = TRUE, verbose = TRUE)
# 
# # Use weight_monthly for monthly estimates
# monthly_pop <- result[, .(
#   population = sum(weight_monthly, na.rm = TRUE)
# ), by = ref_month_yyyymm]

## ----keep-all-example---------------------------------------------------------
# # Default: returns all rows (indeterminate have weight_monthly = NA)
# result <- mensalizePNADC(pnadc_full, compute_weights = TRUE)
# nrow(result) == nrow(pnadc_full)  # TRUE - all rows returned
# sum(is.na(result$weight_monthly))  # ~3% of rows have NA weights
# 
# # Use na.rm = TRUE when aggregating
# monthly_pop <- result[, .(pop = sum(weight_monthly, na.rm = TRUE)), by = ref_month_yyyymm]
# 
# # Alternative: returns only determined rows
# result_determined <- mensalizePNADC(pnadc_full, compute_weights = TRUE, keep_all = FALSE)
# nrow(result_determined) < nrow(pnadc_full)  # TRUE (~97% of rows)

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

