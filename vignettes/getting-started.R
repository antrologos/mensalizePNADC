## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----install------------------------------------------------------------------
# # Install from GitHub
# devtools::install_github("antrologos/mensalizePNADC")

## -----------------------------------------------------------------------------
# # Load the package
# library(PNADCperiods)
# library(data.table)

## ----load-data----------------------------------------------------------------
# # Load your stacked quarterly PNADC data
# pnadc <- fread("pnadc_stacked.csv")
# 
# # Check data dimensions
# cat("Rows:", nrow(pnadc), "\n")
# cat("Quarters:", uniqueN(pnadc[, .(Ano, Trimestre)]), "\n")

## ----build-crosswalk----------------------------------------------------------
# # Identify reference periods (month, fortnight, week)
# crosswalk <- pnadc_identify_periods(pnadc)

## ----view-crosswalk-----------------------------------------------------------
# # View the crosswalk
# head(crosswalk)
# 
# # Check determination rates
# attr(crosswalk, "determination_rates")
# # $month
# # [1] 0.97
# # $fortnight
# # [1] 0.885
# # $week
# # [1] 0.623

## ----apply-crosswalk----------------------------------------------------------
# # Apply crosswalk to a specific quarterly dataset
# # For quarterly data, use anchor = "quarter" and weight_var = "V1028"
# result <- pnadc_apply_periods(
#   pnadc_2023q1,
#   crosswalk,
#   weight_var = "V1028",
#   anchor = "quarter",
#   calibrate = TRUE,
#   calibration_unit = "month"
# )

## ----build-once---------------------------------------------------------------
# # Stack multiple years of quarterly data
# pnadc_stacked <- rbindlist(list(pnadc_2020, pnadc_2021, pnadc_2022, pnadc_2023))
# 
# # Build crosswalk (only needs the identification columns)
# crosswalk <- pnadc_identify_periods(pnadc_stacked)
# 
# # Save for reuse
# saveRDS(crosswalk, "crosswalk_2020_2023.rds")

## ----apply-many---------------------------------------------------------------
# # Load saved crosswalk
# crosswalk <- readRDS("crosswalk_2020_2023.rds")
# 
# # Apply to quarterly data
# monthly_q1 <- pnadc_apply_periods(pnadc_2023q1, crosswalk,
#                                    weight_var = "V1028",
#                                    anchor = "quarter")
# 
# # Apply to annual data (with V1032 weights)
# monthly_annual <- pnadc_apply_periods(pnadc_annual_2023, crosswalk,
#                                        weight_var = "V1032",
#                                        anchor = "year")

## ----calibration-units--------------------------------------------------------
# # Monthly calibration (default)
# result_month <- pnadc_apply_periods(pnadc, crosswalk,
#                                      weight_var = "V1028",
#                                      anchor = "quarter",
#                                      calibration_unit = "month")
# # -> Adds weight_monthly column
# 
# # Fortnight calibration
# result_fortnight <- pnadc_apply_periods(pnadc, crosswalk,
#                                          weight_var = "V1028",
#                                          anchor = "quarter",
#                                          calibration_unit = "fortnight")
# # -> Adds weight_fortnight column
# 
# # Weekly calibration
# result_week <- pnadc_apply_periods(pnadc, crosswalk,
#                                     weight_var = "V1028",
#                                     anchor = "quarter",
#                                     calibration_unit = "week")
# # -> Adds weight_weekly column

## ----no-calibrate-------------------------------------------------------------
# result <- pnadc_apply_periods(pnadc, crosswalk,
#                                weight_var = "V1028",
#                                anchor = "quarter",
#                                calibrate = FALSE)
# # Result has ref_month, ref_fortnight, ref_week but no weight_monthly

## ----monthly-analysis---------------------------------------------------------
# # Compute monthly unemployment rate
# monthly_unemployment <- result[determined_month == TRUE, .(
#   unemployment_rate = sum((VD4002 == 2) * weight_monthly, na.rm = TRUE) /
#                       sum((VD4001 == 1) * weight_monthly, na.rm = TRUE)
# ), by = ref_month_yyyymm]
# 
# # Compute monthly population
# monthly_pop <- result[, .(
#   population = sum(weight_monthly, na.rm = TRUE)
# ), by = ref_month_yyyymm]

