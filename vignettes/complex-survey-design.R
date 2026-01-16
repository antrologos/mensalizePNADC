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

## ----time-series-linearization------------------------------------------------
# # Function to compute participation rate for one month
# compute_participation <- function(month_yyyymm, data) {
# 
#   month_data <- data[ref_month_yyyymm == month_yyyymm & V2009 >= 14]
# 
#   if (nrow(month_data) == 0) return(NULL)
# 
#   # Create survey design
#   design <- svydesign(
#     ids = ~UPA,
#     strata = ~Estrato,
#     weights = ~weight_monthly,
#     data = month_data,
#     nest = TRUE
#   )
# 
#   # Compute overall participation
#   result <- svymean(~I(VD4001 == 1), design, na.rm = TRUE)
# 
#   data.table(
#     ref_month_yyyymm = month_yyyymm,
#     participation = as.numeric(result),
#     se = as.numeric(SE(result))
#   )
# }
# 
# # Get all unique months
# months <- sort(unique(pnadc_monthly$ref_month_yyyymm))
# 
# # Compute for all months (this may take a few minutes)
# results_list <- lapply(months, compute_participation, data = pnadc_monthly)
# monthly_participation <- rbindlist(results_list)
# 
# # Add confidence intervals
# monthly_participation[, `:=`(
#   ci_lower = participation - 1.96 * se,
#   ci_upper = participation + 1.96 * se,
#   period = as.Date(paste0(ref_month_yyyymm %/% 100, "-",
#                           ref_month_yyyymm %% 100, "-15"))
# )]

## ----create-rep-weights-------------------------------------------------------
# # Calculate the adjustment ratio
# pnadc_monthly[, weight_ratio := weight_monthly / V1028]
# 
# # Get names of replication weight columns
# rep_weight_cols <- paste0("V1028", sprintf("%03d", 1:200))
# 
# # Create adjusted replication weight columns
# for (col in rep_weight_cols) {
#   new_col <- paste0(col, "_monthly")
#   pnadc_monthly[, (new_col) := get(col) * weight_ratio]
# }
# 
# # Verify the adjustment (the ratio should be constant within each observation)
# cat("Ratio range:", range(pnadc_monthly$weight_ratio, na.rm = TRUE), "\n")

## ----design-replication-------------------------------------------------------
# # Create survey design with replication weights for January 2020
# pnadc_jan2020_rep <- pnadc_monthly[ref_month_yyyymm == 202001]
# 
# # Get adjusted replication weight column names
# rep_weight_cols_monthly <- paste0("V1028", sprintf("%03d", 1:200), "_monthly")
# 
# # Create replicate weights design
# # type = "bootstrap" matches IBGE's methodology
# design_rep_jan2020 <- svrepdesign(
#   data = pnadc_jan2020_rep,
#   weights = ~weight_monthly,
#   repweights = rep_weight_cols_monthly,
#   type = "bootstrap",
#   combined.weights = TRUE     # The replication weights already include the base weight
# )
# 
# summary(design_rep_jan2020)

## ----estimates-replication----------------------------------------------------
# # Labor force participation rate
# design_rep_14plus <- subset(design_rep_jan2020, V2009 >= 14)
# 
# participation_rep <- svymean(~I(VD4001 == 1), design_rep_14plus, na.rm = TRUE)
# print(participation_rep)
# 
# # Compare with linearization estimate
# cat("\nLinearization SE:", as.numeric(SE(participation)), "\n")
# cat("Replication SE:", as.numeric(SE(participation_rep)), "\n")
# 
# # Participation by sex
# participation_by_sex_rep <- svyby(
#   ~I(VD4001 == 1),
#   by = ~V2007,
#   design = design_rep_14plus,
#   FUN = svymean,
#   na.rm = TRUE
# )
# print(participation_by_sex_rep)

## ----compute-gender-series----------------------------------------------------
# # Prepare analysis data
# pnadc_analysis <- pnadc_monthly[V2009 >= 14]
# pnadc_analysis[, in_labor_force := fifelse(VD4001 == 1, 1L, 0L)]
# pnadc_analysis[, sex := fifelse(V2007 == 1, "Men", "Women")]
# 
# # Function to compute participation by sex for one month
# compute_participation_by_sex <- function(month_yyyymm, data) {
# 
#   month_data <- data[ref_month_yyyymm == month_yyyymm]
# 
#   if (nrow(month_data) == 0) return(NULL)
# 
#   # Create survey design
#   design <- svydesign(
#     ids = ~UPA,
#     strata = ~Estrato,
#     weights = ~weight_monthly,
#     data = month_data,
#     nest = TRUE
#   )
# 
#   # Compute participation by sex
#   result <- svyby(
#     ~in_labor_force,
#     by = ~sex,
#     design = design,
#     FUN = svymean,
#     na.rm = TRUE,
#     vartype = c("se", "ci")
#   )
# 
#   setDT(result)
#   result[, ref_month_yyyymm := month_yyyymm]
# 
#   result
# }
# 
# # Compute for all months
# months <- sort(unique(pnadc_analysis$ref_month_yyyymm))
# results_by_sex <- rbindlist(lapply(months, compute_participation_by_sex, data = pnadc_analysis))
# 
# # Rename columns for clarity
# setnames(results_by_sex,
#          c("in_labor_force", "se", "ci_l", "ci_u"),
#          c("participation", "se", "ci_lower", "ci_upper"))
# 
# # Add date column for plotting
# results_by_sex[, period := as.Date(paste0(
#   ref_month_yyyymm %/% 100, "-",
#   ref_month_yyyymm %% 100, "-15"
# ))]

## ----plot-gender-gap, fig.width=10, fig.height=6------------------------------
# # Create the plot
# ggplot(results_by_sex, aes(x = period, y = participation, color = sex, fill = sex)) +
# 
#   # Confidence bands
#   geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
# 
#   # Point estimates
#   geom_line(linewidth = 0.8) +
# 
#   # Highlight COVID period
#   annotate("rect",
#            xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"),
#            ymin = -Inf, ymax = Inf,
#            fill = "gray80", alpha = 0.3) +
#   annotate("text",
#            x = as.Date("2021-01-01"), y = 0.85,
#            label = "COVID-19", fontface = "italic", size = 3, color = "gray40") +
# 
#   # Scales and labels
#   scale_y_continuous(
#     labels = percent_format(accuracy = 1),
#     limits = c(0.45, 0.85),
#     breaks = seq(0.45, 0.85, 0.05)
#   ) +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   scale_color_manual(values = c("Men" = "#0074D9", "Women" = "#FF4136")) +
#   scale_fill_manual(values = c("Men" = "#0074D9", "Women" = "#FF4136")) +
# 
#   labs(
#     title = "Labor Force Participation by Sex: Monthly Series with 95% Confidence Intervals",
#     subtitle = "Brazil, 2012-2024. Shaded regions show 95% confidence bands from complex survey design.",
#     x = NULL,
#     y = "Labor Force Participation Rate",
#     color = "Sex",
#     fill = "Sex",
#     caption = "Source: PNADC/IBGE. Monthly weights from mensalizePNADC. Variance estimated via Taylor linearization."
#   ) +
# 
#   theme_minimal(base_size = 11) +
#   theme(
#     plot.title = element_text(face = "bold"),
#     legend.position = "bottom",
#     panel.grid.minor = element_blank()
#   )

## ----compute-gap--------------------------------------------------------------
# # Function to compute the gender gap for one month
# compute_gender_gap <- function(month_yyyymm, data) {
# 
#   month_data <- data[ref_month_yyyymm == month_yyyymm]
# 
#   if (nrow(month_data) == 0) return(NULL)
# 
#   design <- svydesign(
#     ids = ~UPA,
#     strata = ~Estrato,
#     weights = ~weight_monthly,
#     data = month_data,
#     nest = TRUE
#   )
# 
#   # Compute participation by sex
#   by_sex <- svyby(~in_labor_force, ~sex, design, svymean, na.rm = TRUE)
# 
#   # Extract values
#   men_rate <- coef(by_sex)["Men"]
#   women_rate <- coef(by_sex)["Women"]
# 
#   # The variance of (Men - Women) = Var(Men) + Var(Women) - 2*Cov(Men, Women)
#   # For independent samples, Cov ≈ 0, so SE(gap) ≈ sqrt(SE_men^2 + SE_women^2)
#   se_men <- SE(by_sex)["Men"]
#   se_women <- SE(by_sex)["Women"]
#   se_gap <- sqrt(se_men^2 + se_women^2)
# 
#   data.table(
#     ref_month_yyyymm = month_yyyymm,
#     gap = men_rate - women_rate,
#     se = se_gap,
#     men_rate = men_rate,
#     women_rate = women_rate
#   )
# }
# 
# # Compute for all months
# gap_results <- rbindlist(lapply(months, compute_gender_gap, data = pnadc_analysis))
# 
# # Add confidence intervals and date
# gap_results[, `:=`(
#   ci_lower = gap - 1.96 * se,
#   ci_upper = gap + 1.96 * se,
#   period = as.Date(paste0(ref_month_yyyymm %/% 100, "-", ref_month_yyyymm %% 100, "-15"))
# )]

## ----plot-gap, fig.width=10, fig.height=5-------------------------------------
# ggplot(gap_results, aes(x = period, y = gap)) +
# 
#   # Confidence band
#   geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
#               fill = "#7b3294", alpha = 0.3) +
# 
#   # Point estimate
#   geom_line(color = "#7b3294", linewidth = 0.9) +
# 
#   # Reference line at pre-COVID average
#   geom_hline(
#     yintercept = gap_results[period < "2020-03-01", mean(gap)],
#     linetype = "dashed", color = "gray50"
#   ) +
# 
#   # Highlight COVID period
#   annotate("rect",
#            xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"),
#            ymin = -Inf, ymax = Inf,
#            fill = "gray80", alpha = 0.3) +
# 
#   # Scales and labels
#   scale_y_continuous(
#     labels = percent_format(accuracy = 1),
#     breaks = seq(0.15, 0.30, 0.025)
#   ) +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
# 
#   labs(
#     title = "Gender Gap in Labor Force Participation (Men - Women)",
#     subtitle = "Monthly series with 95% CI. Dashed line = pre-COVID average. Gap widened during pandemic.",
#     x = NULL,
#     y = "Participation Gap (percentage points)",
#     caption = "Source: PNADC/IBGE. Monthly weights from mensalizePNADC."
#   ) +
# 
#   theme_minimal(base_size = 11) +
#   theme(
#     plot.title = element_text(face = "bold"),
#     panel.grid.minor = element_blank()
#   )

## ----deff---------------------------------------------------------------------
# # Compute design effect for participation rate
# deff(svymean(~I(VD4001 == 1), design_14plus, na.rm = TRUE, deff = TRUE))
# 
# # Design effect > 1 means clustering/stratification increased variance
# # Design effect < 1 means stratification improved precision

## ----singleton----------------------------------------------------------------
# # Set option for handling singleton strata
# options(survey.lonely.psu = "adjust")  # Recommended: conservative adjustment
# 
# # Other options:
# # "remove" - drop singleton strata from variance calculation
# # "average" - use average variance contribution
# # "certainty" - treat as certainty selections (SE = 0)

## ----subpop-------------------------------------------------------------------
# # CORRECT: Subset the design
# design_women <- subset(design_14plus, V2007 == 2)
# svymean(~I(VD4001 == 1), design_women, na.rm = TRUE)
# 
# # INCORRECT: Don't do this - loses design information
# # bad_design <- svydesign(..., data = pnadc[V2007 == 2])

## ----srvyr-example------------------------------------------------------------
# library(srvyr)
# 
# # Convert to srvyr design
# design_srvyr <- pnadc_jan2020 %>%
#   filter(V2009 >= 14) %>%
#   as_survey_design(
#     ids = UPA,
#     strata = Estrato,
#     weights = weight_monthly,
#     nest = TRUE
#   )
# 
# # Compute participation by sex using dplyr verbs
# participation_srvyr <- design_srvyr %>%
#   mutate(sex = if_else(V2007 == 1, "Men", "Women"),
#          in_lf = VD4001 == 1) %>%
#   group_by(sex) %>%
#   summarise(
#     participation = survey_mean(in_lf, na.rm = TRUE, vartype = "ci")
#   )
# 
# print(participation_srvyr)

