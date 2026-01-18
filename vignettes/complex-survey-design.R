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

## ----packages-----------------------------------------------------------------
# # Core packages
# library(PNADCperiods)
# library(data.table)
# 
# # Survey analysis
# library(survey)       # For svydesign and linearization-based variance
# library(srvyr)        # Tidyverse-friendly wrapper for survey
# 
# # Visualization
# library(ggplot2)
# library(scales)

## ----load-data----------------------------------------------------------------
# # Load stacked PNADC data with ALL required variables
# # This needs: Ano, Trimestre, UPA, V1008, V1014, V2003, V2008, V20081, V20082, V2009
# #             V1028, UF, posest, posest_sxi (for weight calibration)
# #             Estrato (for Strategy 1)
# #             V1028001-V1028200 (for Strategy 2)
# #             Plus any analysis variables (VD4001, VD4002, V2007, etc.)
# 
# pnadc <- fread("path/to/your/pnadc_full_stacked.csv")
# 
# # Step 1: Build crosswalk (identify reference periods)
# crosswalk <- pnadc_identify_periods(pnadc)
# 
# # Step 2: Apply crosswalk and calibrate weights
# pnadc <- pnadc_apply_periods(pnadc, crosswalk,
#                               weight_var = "V1028",
#                               anchor = "quarter",
#                               calibrate = TRUE)
# 
# # Filter to determined observations (those with monthly weights)
# pnadc_monthly <- pnadc[!is.na(weight_monthly)]

## ----design-linearization-----------------------------------------------------
# # Create survey design object for a specific month
# # Let's analyze January 2020 (ref_month_yyyymm = 202001)
# 
# pnadc_jan2020 <- pnadc_monthly[ref_month_yyyymm == 202001]
# 
# # Set up the survey design
# design_jan2020 <- svydesign(
#   ids = ~UPA,                    # PSU/cluster identifier
#   strata = ~Estrato,             # Stratification variable
#   weights = ~weight_monthly,     # Monthly-adjusted weight
#   data = pnadc_jan2020,
#   nest = TRUE                    # UPAs are nested within strata
# )
# 
# # Check the design
# summary(design_jan2020)

## ----estimates-linearization--------------------------------------------------
# # Labor force participation rate (VD4001 == 1 means in labor force)
# # Filter to working-age population (14+) first
# design_14plus <- subset(design_jan2020, V2009 >= 14)
# 
# participation <- svymean(~I(VD4001 == 1), design_14plus, na.rm = TRUE)
# print(participation)
# 
# # Get confidence interval
# confint(participation, level = 0.95)
# 
# # Participation by sex (V2007: 1 = Male, 2 = Female)
# participation_by_sex <- svyby(
#   ~I(VD4001 == 1),
#   by = ~V2007,
#   design = design_14plus,
#   FUN = svymean,
#   na.rm = TRUE
# )
# print(participation_by_sex)

