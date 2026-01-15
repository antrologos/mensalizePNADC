## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  echo = TRUE,
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 10,
  fig.height = 6
)

## ----libraries----------------------------------------------------------------
# # Load required packages
# library(mensalizePNADC)
# library(data.table)
# library(ggplot2)
# library(scales)

## ----load-data----------------------------------------------------------------
# # Load the mensalized data from download-and-prepare workflow
# # This should contain all PNADC variables plus the mensalization results
# library(fst)
# pnadc <- read_fst("path/to/your/pnadc_mensalized.fst", as.data.table = TRUE)
# 
# # Alternative: If you have full microdata separately, join with crosswalk
# full_data <- fread("path/to/pnadc_full.csv")
# crosswalk <- read_fst("path/to/mensalized_crosswalk.fst", as.data.table = TRUE)
# 
# pnadc <- merge(
#   full_data,
#   crosswalk[, .(Ano, Trimestre, UPA, V1008, V1014, V2003,
#                 ref_month, ref_month_in_quarter, ref_month_yyyymm, weight_monthly)],
#   by = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003"),
#   all.x = TRUE
# )

## ----create-variables---------------------------------------------------------
# # Filter to working-age population (14+)
# pnadc <- pnadc[V2009 >= 14]
# 
# # Create labor market indicators
# pnadc[, `:=`(
#   # PEA: Economically Active Population (VD4001 == 1)
#   pea = fifelse(VD4001 == 1, 1L, 0L),
# 
#   # Employed (VD4002 == 1)
#   employed = fifelse(VD4002 == 1, 1L, 0L),
# 
#   # Unemployed (in labor force but not employed)
#   unemployed = fifelse(VD4002 == 2, 1L, 0L)
# )]
# 
# # Formality indicator (among employed)
# # Formal: positions 1,3,5,7 OR positions 8,9 with social security contribution
# pnadc[, formal := fifelse(
#   VD4009 %in% c(1L, 3L, 5L, 7L), 1L,
#   fifelse(VD4009 %in% c(8L, 9L) & VD4012 == 1L, 1L, 0L)
# )]
# pnadc[VD4002 != 1, formal := NA_integer_]

## ----create-quarterly---------------------------------------------------------
# quarterly_total <- pnadc[, .(
#   unemployment_rate = sum(unemployed * V1028, na.rm = TRUE) /
#                       sum(pea * V1028, na.rm = TRUE),
#   participation_rate = sum(pea * V1028, na.rm = TRUE) /
#                        sum(V1028, na.rm = TRUE),
#   formalization_rate = sum(formal * V1028, na.rm = TRUE) /
#                        sum(employed * V1028, na.rm = TRUE)
# ), by = .(Ano, Trimestre)]
# 
# # Add period column for plotting (mid-quarter date)
# quarterly_total[, period := as.Date(paste0(Ano, "-", (Trimestre - 1) * 3 + 2, "-15"))]

