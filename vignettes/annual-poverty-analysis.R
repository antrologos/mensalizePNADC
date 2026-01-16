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

## ----prerequisites------------------------------------------------------------
# # Core packages
# library(PNADCperiods)
# library(data.table)
# library(fst)
# 
# # For deflation
# library(readxl)      # Read deflator Excel files
# library(deflateBR)   # INPC deflator
# 
# # For survey analysis
# library(survey)
# 
# # For visualization
# library(ggplot2)
# library(scales)
# library(patchwork)   # Combine plots
# library(Hmisc)       # wtd.mean

## ----quick-preview, eval=FALSE------------------------------------------------
# # Load pre-computed monthly poverty estimates
# monthly_poverty <- fst::read_fst("monthly_poverty_aggregates.fst",
#                                   as.data.table = TRUE)
# 
# # Monthly poverty rate for March 2020 (COVID onset)
# monthly_poverty[ref_month_yyyymm == 202003, poverty_rate]
# #> [1] 0.25  # ~25% below USD 8.30/day poverty line
# 
# # Compare to December 2020 (after Auxílio Emergencial)
# monthly_poverty[ref_month_yyyymm == 202012, poverty_rate]
# #> [1] 0.19  # Reduced due to emergency transfers

## ----join-keys----------------------------------------------------------------
# # Join keys between quarterly and annual PNADC
# # Note: Variable names are converted to lowercase during processing
# join_keys <- c("ano", "trimestre", "upa", "v1008", "v1014", "v2003")
# 
# # ano:       Survey year
# # trimestre: Quarter (1-4)
# # upa:       Primary Sampling Unit (census tract)
# # v1008:     Household selection number within UPA
# # v1014:     Panel group (1-8, identifies rotation cohort)
# # v2003:     Person order within household

## ----create-crosswalk---------------------------------------------------------
# library(PNADCperiods)
# library(data.table)
# library(fst)
# 
# # Define paths
# pnad_quarterly_dir <- "path/to/quarterly/data"
# 
# # List quarterly files (2015-2024)
# quarterly_files <- list.files(
#   path = pnad_quarterly_dir,
#   pattern = "PNADC_.*\\.fst$",
#   full.names = TRUE
# )
# 
# # Variables needed for mensalization
# quarterly_vars <- c(
#   "Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
#   "V2008", "V20081", "V20082", "V2009",
#   "V1028", "UF", "posest", "posest_sxi", "Estrato"
# )
# 
# # Load and stack quarterly data
# quarterly_data <- rbindlist(
#   lapply(quarterly_files, function(f) {
#     read_fst(f, as.data.table = TRUE, columns = quarterly_vars)
#   }),
#   fill = TRUE
# )
# 
# # Build the crosswalk (identifies reference periods)
# crosswalk <- pnadc_identify_periods(
#   quarterly_data,
#   verbose = TRUE
# )
# 
# # Check determination rate
# mean(!is.na(crosswalk$ref_month_in_quarter))
# #> [1] 0.97  # ~97% of observations have determined reference months

## ----load-annual-data---------------------------------------------------------
# pnad_annual_dir <- "path/to/annual/data"
# 
# # Define which visit to use for each year
# visit_selection <- data.table(
#   ano = 2015:2024,
#   visita = c(1, 1, 1, 1, 1, 5, 5, 1, 1, 1)  # 2020-2021 use visit 5
# )

## ----load-annual-data-continued-----------------------------------------------
# 
# # Build file paths
# annual_files <- visit_selection[, .(
#   file = file.path(pnad_annual_dir, sprintf("pnadc_%d_visita%d.fst", ano, visita))
# ), by = ano]
# 
# # Variables to load
# annual_vars <- c(
#   # Join keys
#   "ano", "trimestre", "upa", "v1008", "v1014", "v2003",
#   # Demographics
#   "v2005", "v2007", "v2009", "v2010", "uf", "estrato",
#   # Weights and calibration
#   "v1032", "posest", "posest_sxi",
#   # Household per capita income (IBGE pre-calculated)
#   "vd5008"
# )
# 
# # Load and stack annual data
# annual_data <- rbindlist(
#   lapply(annual_files[file.exists(file), file], function(f) {
#     dt <- read_fst(f, as.data.table = TRUE)
#     setnames(dt, tolower(names(dt)))
#     cols_present <- intersect(annual_vars, names(dt))
#     dt[, ..cols_present]
#   }),
#   fill = TRUE
# )

## ----merge-crosswalk----------------------------------------------------------
# # Apply crosswalk to annual data and calibrate V1032 weights
# d <- pnadc_apply_periods(
#   annual_data,
#   crosswalk,
#   weight_var = "V1032",
#   anchor = "year",
#   calibrate = TRUE,
#   verbose = TRUE
# )
# #> Applying crosswalk...
# #> Match rate: 97.0%
# #> Calibrating weights...
# #>   Fetching monthly population from SIDRA...
# #>   Calibrated 28,456,123 observations
# 
# # Check match rate
# mean(!is.na(d$ref_month_in_quarter))
# #> [1] 0.97

## ----construct-income---------------------------------------------------------
# # Filter to household members only
# d <- d[v2005 <= 14 | v2005 == 16]
# 
# # Create household ID
# d[, id_dom := paste(upa, v1008, v1014, sep = "_")]
# 
# # Use IBGE's pre-calculated per capita household income
# # VD5008 already accounts for all income sources and household size
# d[, hhinc_pc_nominal := fifelse(is.na(vd5008), 0, vd5008)]

## ----apply-deflation----------------------------------------------------------
# # Load deflator data (from IBGE documentation)
# deflator <- readxl::read_excel("path/to/deflator_pnadc_2024.xls")
# setDT(deflator)
# deflator <- deflator[, .(ano, trimestre = trim, uf, CO2, CO2e, CO3)]
# 
# # Merge deflators with data
# setkeyv(deflator, c("ano", "trimestre", "uf"))
# setkeyv(d, c("ano", "trimestre", "uf"))
# d <- deflator[d]
# 
# # INPC adjustment factor to reference date (December 2025)
# inpc_factor <- deflateBR::inpc(1,
#                                nominal_dates = as.Date("2024-07-01"),
#                                real_date = "12/2025")
# 
# # Apply deflation
# d[, hhinc_pc := hhinc_pc_nominal * CO2 * inpc_factor]

## ----define-poverty-lines-----------------------------------------------------
# # World Bank poverty line: USD 8.30 PPP per day (upper-middle income threshold)
# poverty_line_830_ppp_daily <- 8.30
# 
# # 2021 PPP conversion factor (World Bank)
# # https://data.worldbank.org/indicator/PA.NUS.PRVT.PP?year=2021
# usd_to_brl_ppp <- 2.45
# days_to_month <- 365/12
# 
# # Monthly value in 2021 BRL
# poverty_line_830_brl_monthly_2021 <- poverty_line_830_ppp_daily *
#                                      usd_to_brl_ppp * days_to_month
# 
# # Deflate to December 2025 reference
# poverty_line_830_brl_monthly_2025 <- deflateBR::inpc(
#   poverty_line_830_brl_monthly_2021,
#   nominal_dates = as.Date("2021-07-01"),
#   real_date = "12/2025"
# )
# 
# d[, poverty_line := poverty_line_830_brl_monthly_2025]

## ----helper-functions---------------------------------------------------------
# # FGT poverty measure family (Foster-Greer-Thorbecke)
# # alpha = 0: Headcount ratio (share below line)
# # alpha = 1: Poverty gap (average shortfall)
# # alpha = 2: Squared poverty gap (sensitive to inequality among poor)
# fgt <- function(x, z, w = NULL, alpha = 0) {
#   if (is.null(w)) w <- rep(1, length(x))
#   if (length(z) == 1) z <- rep(z, length(x))
# 
#   idx <- complete.cases(x, z, w)
#   x <- x[idx]; z <- z[idx]; w <- w[idx]
# 
#   g <- pmax(0, (z - x) / z)
#   fgt_val <- ifelse(x < z, g^alpha, 0)
# 
#   sum(w * fgt_val) / sum(w)
# }

## ----example-fgt-family-------------------------------------------------------
# # Filter to determined observations
# d_monthly <- d[!is.na(ref_month_yyyymm)]
# 
# # Use calibrated monthly weight (from mensalize_annual_pnadc())
# d_monthly[, peso := weight_monthly]
# 
# # Compute monthly poverty statistics
# monthly_poverty <- d_monthly[, .(
#   # FGT-0 (Headcount ratio)
#   poverty_rate = fgt(hhinc_pc, poverty_line, peso, alpha = 0),
# 
#   # FGT-1 (Poverty gap)
#   poverty_gap = fgt(hhinc_pc, poverty_line, peso, alpha = 1),
# 
#   # Mean income
#   mean_income = wtd.mean(hhinc_pc, peso),
# 
#   # Sample size
#   n_obs = .N
# 
# ), by = ref_month_yyyymm]
# 
# # Add date for plotting
# monthly_poverty[, period := as.Date(paste0(
#   ref_month_yyyymm %/% 100, "-",
#   ref_month_yyyymm %% 100, "-15"
# ))]

## ----example-survey-design----------------------------------------------------
# library(survey)
# 
# # Create survey design for one month (e.g., March 2020)
# march_2020 <- d_monthly[ref_month_yyyymm == 202003]
# 
# design <- svydesign(
#   ids = ~upa,
#   strata = ~estrato,
#   weights = ~peso,
#   data = march_2020,
#   nest = TRUE
# )
# 
# # Poverty rate with confidence interval
# poverty_estimate <- svymean(
#   ~I(hhinc_pc < poverty_line),
#   design,
#   na.rm = TRUE
# )
# 
# confint(poverty_estimate)

## ----example-smoothing--------------------------------------------------------
# # Three-month centered rolling average
# monthly_poverty[, poverty_rate_smoothed :=
#                   frollmean(poverty_rate, 3, align = "center")]

