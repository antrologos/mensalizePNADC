## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----installation-------------------------------------------------------------
# # Install from GitHub
# devtools::install_github("antrologos/mensalizePNADC")

## ----load-data----------------------------------------------------------------
# library(mensalizePNADC)
# library(data.table)
# 
# # Load your stacked quarterly PNADC data
# pnadc <- fread("pnadc_stacked.csv",
#   select = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
#              "V2008", "V20081", "V20082", "V2009"))
# 
# # Check data dimensions
# cat("Rows:", nrow(pnadc), "\n")
# cat("Quarters:", uniqueN(pnadc[, .(Ano, Trimestre)]), "\n")

## ----get-crosswalk------------------------------------------------------------
# crosswalk <- mensalizePNADC(pnadc)
# # Processing PNADC data...
# # |==============================| 100%
# #   Determination rate: 97.0%
# 
# # View the crosswalk (a data.table)
# head(crosswalk)
# #    Ano Trimestre    UPA V1008 V1014 V2003   ref_month ref_month_in_quarter ref_month_yyyymm
# # 1: 2012         1 110000     1     1     1 2012-01-01                    1           201201
# # 2: 2012         1 110000     1     1     2 2012-01-01                    1           201201
# 
# # Check the determination rate attribute
# attr(crosswalk, "determination_rate")
# # [1] 0.97

## ----join-data----------------------------------------------------------------
# # Load an original quarterly file
# library(haven)
# original <- read_dta("PNADC_2023T1.dta")
# 
# # Join to add monthly information
# monthly_data <- merge(original, crosswalk,
#   by = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003"),
#   all.x = TRUE)
# 
# # Now you have:
# # - ref_month: Reference month as Date (e.g., 2023-01-01)
# # - ref_month_in_quarter: Position in quarter (1, 2, 3, or NA)
# # - ref_month_yyyymm: Integer YYYYMM format (e.g., 202301)

## ----use-monthly--------------------------------------------------------------
# # Filter to a specific month
# jan_2023 <- monthly_data[ref_month_yyyymm == 202301]
# 
# # Group by month
# by_month <- monthly_data[, .(
#   n_obs = .N,
#   mean_age = mean(V2009, na.rm = TRUE)
# ), by = ref_month_yyyymm]
# 
# # Check determination rate by year
# monthly_data[, .(
#   total = .N,
#   determined = sum(!is.na(ref_month_in_quarter)),
#   rate = round(mean(!is.na(ref_month_in_quarter)) * 100, 1)
# ), by = Ano]

## ----monthly-weights----------------------------------------------------------
# # Load full data with all required variables
# pnadc_full <- fread("pnadc_stacked_full.csv")
# 
# # Run mensalization with weight computation
# # (automatically fetches population from SIDRA API)
# result <- mensalizePNADC(pnadc_full,
#   compute_weights = TRUE,
#   verbose = TRUE)
# # Step 1/4: Identifying reference months...
# #   Determination rate: 97.0%
# # Step 2/4: Fetching monthly population from SIDRA...
# # Step 3/4: Calibrating monthly weights (rake weighting)...
# # Step 4/4: Smoothing monthly aggregates...
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
# sum(is.na(result_determined$weight_monthly))  # 0 - no NA weights

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
# # Step 4: Fetch population data manually
# pop_data <- fetch_monthly_population(verbose = TRUE)
# 
# # Step 5: Calibrate weights separately
# calibrated <- calibrate_monthly_weights(merged_data, pop_data)

## ----check-rates--------------------------------------------------------------
# crosswalk[, .(rate = mean(!is.na(ref_month_in_quarter))), by = .(Ano, Trimestre)]

