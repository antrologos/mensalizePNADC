## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  echo = TRUE,
  message = FALSE,

  warning = FALSE
)

## ----packages-----------------------------------------------------------------
# # Install packages if needed
# install.packages(c("PNADcIBGE", "tidyverse", "fst"))
# 
# # Install mensalizePNADC from GitHub
# # install.packages("remotes")
# # remotes::install_github("antrologos/mensalizePNADC")
# 
# # Load packages
# library(PNADcIBGE)
# library(tidyverse)
# library(fst)
# library(mensalizePNADC)

## ----setup-dir----------------------------------------------------------------
# # Set your data directory (adjust path as needed)
# data_dir <- "path/to/your/pnadc_data/"
# 
# # Create directory if it doesn't exist
# dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

## ----editions-----------------------------------------------------------------
# # Define quarters to download (2020-2024 example)
# editions <- expand.grid(
#   year = 2020:2024,
#   quarter = 1:4
# ) |>
#   # Remove quarters not yet available
#   filter(!(year == 2024 & quarter > 3))
# 
# # View the grid
# editions

## ----download-loop------------------------------------------------------------
# # Download and save each quarter
# for (i in 1:nrow(editions)) {
#   year_i <- editions$year[i]
#   quarter_i <- editions$quarter[i]
# 
#   filename <- paste0("pnadc_", year_i, "-", quarter_i, "q.fst")
# 
#   cat("Downloading:", year_i, "Q", quarter_i, "\n")
# 
#   # Download from IBGE
#   pnadc_quarter <- get_pnadc(
#     year = year_i,
#     quarter = quarter_i,
#     labels = FALSE,    # IMPORTANT: Use numeric codes, not labels
#     deflator = FALSE,
#     design = FALSE,
#     savedir = data_dir
#   )
# 
#   # Save as FST format (fast serialization)
#   write_fst(pnadc_quarter, file.path(data_dir, filename))
# 
#   # Clean up temporary files created by PNADcIBGE
#   temp_files <- list.files(data_dir,
#                            pattern = "\\.(zip|sas|txt)$",
#                            full.names = TRUE)
#   file.remove(temp_files)
# 
#   # Free memory
#   rm(pnadc_quarter)
#   gc()
# }

## ----stack-data---------------------------------------------------------------
# # Columns needed for mensalization
# cols_needed <- c(
#   # Time and identifiers
#   "Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
#   # Birthday variables (for reference month algorithm)
#   "V2008", "V20081", "V20082", "V2009",
#   # Weight and stratification (for weight calibration)
#   "V1028", "UF", "posest", "posest_sxi"
# )
# 
# # Stack all quarters
# pnadc_stacked <- NULL
# 
# for (i in 1:nrow(editions)) {
#   year_i <- editions$year[i]
#   quarter_i <- editions$quarter[i]
# 
#   filename <- paste0("pnadc_", year_i, "-", quarter_i, "q.fst")
#   cat("Loading:", filename, "\n")
# 
#   # Read only the columns we need (saves memory)
#   quarter_data <- read_fst(
#     file.path(data_dir, filename),
#     columns = cols_needed
#   )
# 
#   pnadc_stacked <- bind_rows(pnadc_stacked, quarter_data)
#   rm(quarter_data)
#   gc()
# }
# 
# cat("Total observations:", format(nrow(pnadc_stacked), big.mark = ","), "\n")

## ----mensalize----------------------------------------------------------------
# # Apply mensalization with monthly weight computation
# result <- mensalizePNADC(
#   data = pnadc_stacked,
#   compute_weights = TRUE,
#   verbose = TRUE
# )
# 
# # Check the determination rate
# cat("Determination rate:",
#     sprintf("%.1f%%", attr(result, "determination_rate") * 100), "\n")

## ----explore------------------------------------------------------------------
# # View the result structure
# glimpse(result)
# 
# # Summary of reference month distribution
# result |>
#   count(ref_month_in_quarter) |>
#   mutate(pct = n / sum(n) * 100)

## ----save---------------------------------------------------------------------
# # Save the mensalized crosswalk
# write_fst(result, file.path(data_dir, "pnadc_mensalized.fst"))

## ----join---------------------------------------------------------------------
# # Load a quarterly file with all variables
# full_data <- read_fst(file.path(data_dir, "pnadc_2023-1q.fst"))
# 
# # Join with mensalization results
# analysis_data <- full_data |>
#   left_join(
#     result |> select(Ano, Trimestre, UPA, V1008, V1014, V2003,
#                      ref_month, ref_month_in_quarter, weight_monthly),
#     by = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003")
#   )
# 
# # Now you can aggregate by month using weight_monthly
# monthly_unemployment <- analysis_data |>
#   filter(!is.na(ref_month_in_quarter)) |>
#   group_by(ref_month) |>
#   summarize(
#     unemployment_rate = sum((VD4002 == 2) * weight_monthly, na.rm = TRUE) /
#                         sum((VD4001 == 1) * weight_monthly, na.rm = TRUE)
#   )

## ----full-history-------------------------------------------------------------
# # Download all available data (2012-present)
# editions_full <- expand.grid(
#   year = 2012:2025,
#   quarter = 1:4
# ) |>
#   filter(!(year == 2025 & quarter > 3))  # Adjust for latest available
# 
# # Use the same download and stacking loops as above

