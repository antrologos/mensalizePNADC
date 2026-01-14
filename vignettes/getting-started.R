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
# library(mensalizePNADC)
# library(data.table)

## ----load-data----------------------------------------------------------------
# # Load your stacked quarterly PNADC data
# pnadc <- fread("pnadc_stacked.csv",
#   select = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
#              "V2008", "V20081", "V20082", "V2009"))
# 
# # Check data dimensions
# cat("Rows:", nrow(pnadc), "\n")
# cat("Quarters:", uniqueN(pnadc[, .(Ano, Trimestre)]), "\n")

## ----run-mensalize------------------------------------------------------------
# # Identify reference months
# crosswalk <- mensalizePNADC(pnadc)

## ----view-output--------------------------------------------------------------
# # View the crosswalk
# head(crosswalk)
# #    Ano Trimestre    UPA V1008 V1014 V2003   ref_month ref_month_in_quarter ref_month_yyyymm
# # 1: 2012         1 110000     1     1     1 2012-01-01                    1           201201
# # 2: 2012         1 110000     1     1     2 2012-01-01                    1           201201
# 
# # Check the determination rate attribute
# attr(crosswalk, "determination_rate")
# # [1] 0.97

## ----join-data----------------------------------------------------------------
# # Load an original quarterly file with all variables you need
# library(haven)
# original <- read_dta("PNADC_2023T1.dta")
# 
# # Join to add monthly information
# monthly_data <- merge(original, crosswalk,
#   by = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003"),
#   all.x = TRUE)

## ----use-monthly--------------------------------------------------------------
# # Filter to a specific month
# jan_2023 <- monthly_data[ref_month_yyyymm == 202301]
# 
# # Group by month
# by_month <- monthly_data[, .(
#   n_obs = .N,
#   mean_age = weighted.mean(V2009, V1028, na.rm = TRUE)
# ), by = ref_month_yyyymm]
# 
# # Check determination rate by year
# monthly_data[, .(
#   total = .N,
#   determined = sum(!is.na(ref_month_in_quarter)),
#   rate = round(mean(!is.na(ref_month_in_quarter)) * 100, 1)
# ), by = Ano]

## ----compute-weights----------------------------------------------------------
# # Load full data with weight columns
# pnadc_full <- fread("pnadc_stacked_full.csv")
# 
# # Run mensalization with weight computation
# result <- mensalizePNADC(pnadc_full, compute_weights = TRUE, verbose = TRUE)

## ----use-weights--------------------------------------------------------------
# # Compute monthly unemployment rate
# monthly_unemployment <- result[!is.na(weight_monthly), .(
#   unemployment_rate = sum((VD4002 == 2) * weight_monthly, na.rm = TRUE) /
#                       sum((VD4001 == 1) * weight_monthly, na.rm = TRUE)
# ), by = ref_month_yyyymm]
# 
# # Compute monthly population
# monthly_pop <- result[, .(
#   population = sum(weight_monthly, na.rm = TRUE)
# ), by = ref_month_yyyymm]

## ----keep-all-----------------------------------------------------------------
# # Default: all rows returned
# result <- mensalizePNADC(pnadc_full, compute_weights = TRUE)
# nrow(result) == nrow(pnadc_full)  # TRUE
# 
# # About 3% have NA weights
# sum(is.na(result$weight_monthly)) / nrow(result)  # ~0.03
# 
# # Alternative: only return determined rows
# result_determined <- mensalizePNADC(pnadc_full, compute_weights = TRUE, keep_all = FALSE)
# nrow(result_determined) < nrow(pnadc_full)  # TRUE (~97% of rows)

