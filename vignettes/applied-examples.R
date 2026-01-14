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
# pnadc <- fread("path/to/your/pnadc_mensalized.fst")
# 
# # Alternative: If you have full microdata separately, join with crosswalk
# full_data <- fread("path/to/pnadc_full.csv")
# crosswalk <- fread("path/to/mensalized_crosswalk.fst")
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

## ----create-monthly-----------------------------------------------------------
# # Filter to observations with determined reference month
# pnadc_monthly <- pnadc[!is.na(weight_monthly)]
# 
# monthly_total <- pnadc_monthly[, .(
#   unemployment_rate = sum(unemployed * weight_monthly, na.rm = TRUE) /
#                       sum(pea * weight_monthly, na.rm = TRUE),
#   participation_rate = sum(pea * weight_monthly, na.rm = TRUE) /
#                        sum(weight_monthly, na.rm = TRUE),
#   formalization_rate = sum(formal * weight_monthly, na.rm = TRUE) /
#                        sum(employed * weight_monthly, na.rm = TRUE)
# ), by = ref_month_yyyymm]
# 
# # Add period column for plotting (mid-month date)
# monthly_total[, period := as.Date(paste0(
#   ref_month_yyyymm %/% 100, "-",
#   ref_month_yyyymm %% 100, "-15"
# ))]

## ----covid-unemployment-------------------------------------------------------
# # Filter to COVID period (Oct 2019 - Jan 2022)
# covid_quarterly <- quarterly_total[period >= "2019-10-01" & period <= "2022-01-01"]
# covid_monthly <- monthly_total[period >= "2019-10-01" & period <= "2022-01-01"]
# 
# # Find the unemployment peaks
# peak_quarterly <- covid_quarterly[which.max(unemployment_rate)]
# peak_monthly <- covid_monthly[which.max(unemployment_rate)]
# 
# # Create the comparison plot
# ggplot() +
#   # Quarterly as step function (emphasizes moving-average nature)
#   geom_step(data = covid_quarterly,
#             aes(x = period, y = unemployment_rate),
#             color = "steelblue", linewidth = 1.2, direction = "mid") +
#   geom_point(data = covid_quarterly,
#              aes(x = period, y = unemployment_rate),
#              color = "steelblue", size = 3) +
# 
#   # Monthly as line
#   geom_line(data = covid_monthly,
#             aes(x = period, y = unemployment_rate),
#             color = "darkred", linewidth = 0.9) +
#   geom_point(data = covid_monthly,
#              aes(x = period, y = unemployment_rate),
#              color = "darkred", size = 1.5) +
# 
#   # Annotate monthly peak
#   annotate("segment",
#            x = peak_monthly$period, xend = peak_monthly$period,
#            y = peak_monthly$unemployment_rate + 0.005,
#            yend = peak_monthly$unemployment_rate + 0.02,
#            arrow = arrow(length = unit(0.2, "cm")), color = "darkred") +
#   annotate("text",
#            x = peak_monthly$period,
#            y = peak_monthly$unemployment_rate + 0.022,
#            label = paste0("Monthly peak: ", format(peak_monthly$period, "%b %Y"), "\n",
#                          sprintf("%.1f%%", peak_monthly$unemployment_rate * 100)),
#            size = 3.2, color = "darkred", hjust = 0.5) +
# 
#   # Annotate quarterly peak
#   annotate("segment",
#            x = peak_quarterly$period + 30, xend = peak_quarterly$period + 30,
#            y = peak_quarterly$unemployment_rate - 0.005,
#            yend = peak_quarterly$unemployment_rate - 0.02,
#            arrow = arrow(length = unit(0.2, "cm")), color = "steelblue") +
#   annotate("text",
#            x = peak_quarterly$period + 30,
#            y = peak_quarterly$unemployment_rate - 0.025,
#            label = paste0("Quarterly peak: ", format(peak_quarterly$period, "%b %Y"), "\n",
#                          sprintf("%.1f%%", peak_quarterly$unemployment_rate * 100)),
#            size = 3.2, color = "steelblue", hjust = 0.5) +
# 
#   # Scales and labels
#   scale_y_continuous(labels = percent_format(accuracy = 0.1),
#                      limits = c(0.10, 0.17)) +
#   scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
#   labs(
#     title = "COVID-19 Unemployment Spike: Monthly vs Quarterly",
#     subtitle = "Monthly data (red) shows the true peak; quarterly (blue) averages it away",
#     x = NULL, y = "Unemployment Rate",
#     caption = "Source: PNADC/IBGE. Quarterly shown as step function to emphasize moving-average nature."
#   ) +
#   theme_minimal(base_size = 11) +
#   theme(
#     plot.title = element_text(face = "bold"),
#     panel.grid.minor = element_blank()
#   )

## ----recession-detail---------------------------------------------------------
# # Filter to recession period (Jun 2014 - Jun 2017)
# recession_quarterly <- quarterly_total[period >= "2014-06-01" & period <= "2017-06-01"]
# recession_monthly <- monthly_total[period >= "2014-06-01" & period <= "2017-06-01"]
# 
# # Find the peak
# peak_recession <- recession_monthly[which.max(unemployment_rate)]
# 
# # Calculate month-over-month changes
# setorder(recession_monthly, period)
# recession_monthly[, change := unemployment_rate - shift(unemployment_rate)]
# 
# # Identify months with large jumps (> 1 percentage point)
# big_jumps <- recession_monthly[!is.na(change) & change > 0.01]
# 
# # Create the plot
# ggplot() +
#   # Quarterly
#   geom_step(data = recession_quarterly,
#             aes(x = period, y = unemployment_rate),
#             color = "steelblue", linewidth = 1, direction = "mid", alpha = 0.7) +
#   geom_point(data = recession_quarterly,
#              aes(x = period, y = unemployment_rate),
#              color = "steelblue", size = 2.5) +
# 
#   # Monthly
#   geom_line(data = recession_monthly,
#             aes(x = period, y = unemployment_rate),
#             color = "darkred", linewidth = 0.8) +
#   geom_point(data = recession_monthly,
#              aes(x = period, y = unemployment_rate),
#              color = "darkred", size = 1.2) +
# 
#   # Highlight big monthly jumps
#   geom_point(data = big_jumps,
#              aes(x = period, y = unemployment_rate),
#              color = "darkred", size = 3, shape = 21, fill = "yellow", stroke = 1.5) +
# 
#   # Mark the peak
#   annotate("point", x = peak_recession$period, y = peak_recession$unemployment_rate,
#            color = "darkred", size = 4, shape = 18) +
#   annotate("text",
#            x = peak_recession$period + 45, y = peak_recession$unemployment_rate,
#            label = paste0("Peak: ", format(peak_recession$period, "%b %Y"), "\n",
#                          sprintf("%.1f%%", peak_recession$unemployment_rate * 100)),
#            size = 3, hjust = 0, color = "darkred") +
# 
#   # Scales and labels
#   scale_y_continuous(labels = percent_format(accuracy = 1),
#                      breaks = seq(0.06, 0.14, 0.02)) +
#   scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
#   labs(
#     title = "2014-2017 Recession: Month-by-Month Unemployment Rise",
#     subtitle = "Yellow circles: months with >1 p.p. unemployment increase. Monthly data (red) vs quarterly (blue).",
#     x = NULL, y = "Unemployment Rate",
#     caption = "Source: PNADC/IBGE. Monthly data reveals sudden jumps hidden in quarterly averages."
#   ) +
#   theme_minimal(base_size = 11) +
#   theme(
#     plot.title = element_text(face = "bold"),
#     panel.grid.minor = element_blank()
#   )

## ----mw-2013-transition-------------------------------------------------------
# # Create long format for plotting (needed for all MW examples)
# mw_long <- melt(monthly_mw_exact,
#                 id.vars = c("ref_month_yyyymm", "period", "mw_value", "n_workers"),
#                 measure.vars = c("pct_habitual", "pct_effective"),
#                 variable.name = "income_type",
#                 value.name = "pct_at_mw")
# 
# # Add readable labels
# mw_long[, income_label := fifelse(income_type == "pct_habitual",
#                                    "Habitual Income (current month)",
#                                    "Effective Income (previous month)")]
# 
# # Get adjustment dates for reference lines
# adj_dates <- as.Date(paste0(mw_adjustment_months %/% 100, "-",
#                              mw_adjustment_months %% 100, "-15"))
# 
# # Filter to the first transition period (Oct 2012 - Apr 2013)
# mw_2013 <- mw_long[period >= "2012-10-15" & period <= "2013-04-15" &
#                    mw_value %in% c(622, 678)]
# 
# # Create the plot
# ggplot(mw_2013,
#        aes(x = period, y = pct_at_mw, color = factor(mw_value), linetype = factor(mw_value))) +
#   geom_line(linewidth = 1.2) +
#   geom_point(size = 2.5) +
# 
#   # Mark the adjustment date (January 2013)
#   geom_vline(xintercept = as.Date("2013-01-15"),
#              linetype = "dashed", linewidth = 0.8, color = "gray40") +
# 
#   # Facet by income type
#   facet_wrap(~ income_label, ncol = 2) +
# 
#   # Scales and labels
#   scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
#   scale_color_manual(values = c("622" = "#b2182b", "678" = "#2166ac"),
#                      labels = c("622" = "R$ 622 (old)", "678" = "R$ 678 (new)"),
#                      name = "MW Value") +
#   scale_linetype_manual(values = c("622" = "solid", "678" = "solid"), guide = "none") +
#   labs(
#     title = "MW Transition: January 2013 (R$ 622 -> R$ 678)",
#     subtitle = "Habitual income shows transition in January; Effective income shows same pattern in February",
#     x = NULL, y = "Share Earning Exact MW",
#     caption = "Vertical line = MW adjustment. Habitual income refers to current month; effective to previous month."
#   ) +
#   theme_minimal(base_size = 11) +
#   theme(
#     plot.title = element_text(face = "bold"),
#     legend.position = "bottom",
#     panel.grid.minor = element_blank(),
#     strip.text = element_text(face = "bold")
#   )

## ----mw-2020-double-----------------------------------------------------------
# # Filter to 2020 double adjustment period (Jul 2019 - Jul 2020)
# mw_2020 <- mw_long[period >= "2019-07-15" & period <= "2020-07-15" &
#                    mw_value %in% c(998, 1039, 1045)]
# 
# # Get adjustment dates in this period
# adj_2020 <- adj_dates[adj_dates >= "2019-07-15" & adj_dates <= "2020-07-15"]
# 
# # Create the plot
# ggplot(mw_2020,
#        aes(x = period, y = pct_at_mw, color = factor(mw_value), linetype = factor(mw_value))) +
#   geom_line(linewidth = 1.1) +
#   geom_point(size = 2.5) +
# 
#   # Mark adjustment months
#   geom_vline(xintercept = adj_2020, linetype = "dashed", linewidth = 0.8, color = "gray40") +
# 
#   # Facet by income type
#   facet_wrap(~ income_label, ncol = 2) +
# 
#   # Scales and labels
#   scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
#   scale_color_manual(values = c("998" = "#b2182b", "1039" = "#7b3294", "1045" = "#2166ac"),
#                      labels = c("998" = "R$ 998 (2019)", "1039" = "R$ 1,039 (Jan 2020)",
#                                "1045" = "R$ 1,045 (Feb 2020+)"),
#                      name = "MW Value") +
#   scale_linetype_manual(values = c("998" = "solid", "1039" = "solid", "1045" = "solid"),
#                         guide = "none") +
#   labs(
#     title = "The 2020 Double Adjustment: A Demanding Test",
#     subtitle = "R$ 998 -> R$ 1,039 (Jan) -> R$ 1,045 (Feb). Three values, two transitions, one-month lag preserved.",
#     x = NULL, y = "Share Earning Exact MW",
#     caption = "Vertical lines = MW adjustments. The short-lived R$ 1,039 value (one month only) creates a sharp test."
#   ) +
#   theme_minimal(base_size = 11) +
#   theme(
#     plot.title = element_text(face = "bold"),
#     legend.position = "bottom",
#     panel.grid.minor = element_blank(),
#     strip.text = element_text(face = "bold")
#   )

## ----mw-big-picture-----------------------------------------------------------
# # Create color palette (blue to red gradient)
# mw_vals <- sort(unique(monthly_mw_exact$mw_value))
# n_mw <- length(mw_vals)
# palette_fn <- colorRampPalette(c("#2166ac", "#67a9cf", "#d1e5f0",
#                                   "#fddbc7", "#ef8a62", "#b2182b"))
# mw_colors <- palette_fn(n_mw)
# names(mw_colors) <- as.character(mw_vals)
# 
# # Filter to MW values with meaningful presence (>0.1% at some point)
# mw_long[, max_pct := max(pct_at_mw), by = mw_value]
# mw_long_filtered <- mw_long[max_pct > 0.001]
# 
# # Create the plot
# ggplot(mw_long_filtered,
#        aes(x = period, y = pct_at_mw, color = factor(mw_value), group = mw_value)) +
#   geom_line(linewidth = 0.6, alpha = 0.8) +
# 
#   # Mark MW adjustment months
#   geom_vline(xintercept = adj_dates, linetype = "dashed", alpha = 0.3, color = "gray40") +
# 
#   # Facet by income type
#   facet_wrap(~ income_label, ncol = 1) +
# 
#   # Scales and labels
#   scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   scale_color_manual(values = mw_colors, name = "MW Value (R$)") +
#   labs(
#     title = "Share of Formal Workers Earning Each Exact MW Value",
#     subtitle = "Each line = one MW value. Vertical lines = MW adjustments. Pattern shifted 1 month between panels.",
#     x = NULL, y = "Share of Workers",
#     caption = "Source: PNADC/IBGE. Formal private sector employees age 18+. All calculations use weight_monthly."
#   ) +
#   theme_minimal(base_size = 11) +
#   theme(
#     plot.title = element_text(face = "bold"),
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     panel.grid.minor = element_blank(),
#     strip.text = element_text(face = "bold", size = 11)
#   ) +
#   guides(color = guide_legend(ncol = 2))

## ----reproduce, eval=FALSE----------------------------------------------------
# library(mensalizePNADC)
# library(data.table)
# 
# # Load your stacked PNADC data
# pnadc <- fread("your_pnadc_stacked.csv")
# 
# # Apply mensalization with weight computation
# result <- mensalizePNADC(pnadc, compute_weights = TRUE)
# 
# # Compute monthly series
# monthly_total <- result[!is.na(weight_monthly), .(
#   unemployment_rate = sum((VD4001 == 1 & VD4002 == 2) * weight_monthly) /
#                       sum((VD4001 == 1) * weight_monthly),
#   participation_rate = sum((VD4001 == 1) * weight_monthly) /
#                        sum(weight_monthly)
# ), by = ref_month_yyyymm]

