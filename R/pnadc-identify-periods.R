#' Identify Reference Periods in PNADC Data
#'
#' Builds a universal crosswalk containing reference periods (month, fortnight,
#' and week) for PNADC survey data based on IBGE's interview timing rules.
#'
#' @description
#' PNADC is a quarterly survey, but each interview actually refers to a specific
#' temporal period within the quarter. This function identifies which month,
#' fortnight (quinzena), and week each observation belongs to, enabling
#' sub-quarterly time series analysis.
#'
#' The algorithm uses:
#' \itemize{
#'   \item IBGE's reference week timing rules (first Saturday with sufficient days in month)
#'   \item Respondent birthdates to constrain possible interview dates
#'   \item UPA-panel level aggregation for months and fortnights
#'   \item Household-level aggregation for weeks (within each quarter)
#'   \item Dynamic exception detection (identifies quarters needing relaxed rules)
#' }
#'
#' @param data A data.frame or data.table with PNADC microdata. Required columns:
#'   \itemize{
#'     \item \code{Ano}: Survey year
#'     \item \code{Trimestre}: Quarter (1-4)
#'     \item \code{UPA}: Primary Sampling Unit
#'     \item \code{V1008}: Household sequence within UPA
#'     \item \code{V1014}: Panel identifier
#'     \item \code{V2008}: Birth day (1-31)
#'     \item \code{V20081}: Birth month (1-12)
#'     \item \code{V20082}: Birth year
#'     \item \code{V2009}: Age
#'   }
#'   Optional but recommended:
#'   \itemize{
#'     \item \code{V2003}: Person sequence within household
#'   }
#' @param verbose Logical. If TRUE (default), display progress information.
#'
#' @return A data.table crosswalk with columns:
#'   \describe{
#'     \item{UPA, V1014}{Join keys (primary sampling unit and panel group)}
#'     \item{ref_month}{Reference month as Date (1st of month)}
#'     \item{ref_month_in_quarter}{Position in quarter (1, 2, 3) or NA}
#'     \item{ref_month_yyyymm}{Integer YYYYMM format (e.g., 202301)}
#'     \item{determined_month}{Logical: TRUE if month was determined}
#'     \item{ref_fortnight}{Reference fortnight as Date (1st or 16th)}
#'     \item{ref_fortnight_in_quarter}{Position in quarter (1-6) or NA}
#'     \item{ref_fortnight_yyyyff}{Integer YYYYFF format (1-24 per year)}
#'     \item{determined_fortnight}{Logical: TRUE if fortnight was determined}
#'     \item{ref_week}{Reference week as Date (Monday of week)}
#'     \item{ref_week_in_quarter}{Position in quarter (1-14) or NA}
#'     \item{ref_week_yyyyww}{Integer ISO YYYYWW format}
#'     \item{determined_week}{Logical: TRUE if week was determined}
#'   }
#'
#' @details
#' ## Temporal Granularity
#'
#' The crosswalk contains three levels of temporal granularity:
#' \itemize{
#'   \item \strong{Month}: 3 per quarter, ~97% determination rate
#'   \item \strong{Fortnight (quinzena)}: 6 per quarter, ~85-90% determination rate
#'   \item \strong{Week}: 13-14 per quarter, ~50-75% determination rate
#' }
#'
#' ## Cross-Quarter Aggregation (Important!)
#'
#' **For optimal determination rates, input data should be stacked across
#' multiple quarters** (ideally 4+ years). The algorithm leverages PNADC's
#' rotating panel design where the same UPA-V1014 is interviewed in the same
#' relative position across quarterly visits.
#'
#' ## Fortnight Definition
#'
#' Fortnights are numbered 1-24 per year (2 per month):
#' \itemize{
#'   \item Fortnight 01: Jan 1-15
#'   \item Fortnight 02: Jan 16-31
#'   \item Fortnight 03: Feb 1-15
#'   \item ... and so on
#' }
#'
#' ## Important Note
#'
#' This function returns only the crosswalk (identification keys + reference
#' periods). To apply the crosswalk to a dataset and optionally calibrate
#' weights, use \code{\link{pnadc_apply_periods}}.
#'
#' @examples
#' \dontrun{
#' # Build crosswalk from stacked quarterly data
#' crosswalk <- pnadc_identify_periods(pnadc_stacked)
#'
#' # Check determination rates
#' crosswalk[, .(
#'   month_rate = mean(determined_month),
#'   fortnight_rate = mean(determined_fortnight),
#'   week_rate = mean(determined_week)
#' )]
#'
#' # Apply to a specific dataset
#' result <- pnadc_apply_periods(pnadc_2023, crosswalk,
#'                               weight_var = "V1028",
#'                               anchor = "quarter")
#' }
#'
#' @seealso \code{\link{pnadc_apply_periods}} to apply the crosswalk and
#'   calibrate weights
#'
#' @export
pnadc_identify_periods <- function(data, verbose = TRUE) {


  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  if (verbose) {
    cat("Building reference period crosswalk...\n")
  }

  # Validate required columns
  validate_pnadc(data, check_weights = FALSE)

  # Convert to data.table (copy to avoid modifying original)
  dt <- ensure_data_table(data, copy = TRUE)

  # ============================================================================
  # STEP 1: Identify reference months
  # ============================================================================

  if (verbose) cat("  Step 1/3: Identifying reference months...\n")

  month_xw <- identify_reference_month(dt, verbose = FALSE)

  month_rate <- mean(!is.na(month_xw$ref_month_in_quarter))
  if (verbose) cat(sprintf("    -> Month determination rate: %.1f%%\n", month_rate * 100))

  # ============================================================================
  # STEP 2: Identify reference fortnights
  # ============================================================================

  if (verbose) cat("  Step 2/3: Identifying reference fortnights...\n")

  fortnight_xw <- identify_reference_fortnight(dt, verbose = FALSE)

  fortnight_rate <- mean(!is.na(fortnight_xw$ref_fortnight_in_quarter))
  if (verbose) cat(sprintf("    -> Fortnight determination rate: %.1f%%\n", fortnight_rate * 100))

  # ============================================================================
  # STEP 3: Identify reference weeks
  # ============================================================================

  if (verbose) cat("  Step 3/3: Identifying reference weeks...\n")

  week_xw <- identify_reference_week(dt, verbose = FALSE)

  week_rate <- mean(!is.na(week_xw$ref_week_in_quarter))
  if (verbose) cat(sprintf("    -> Week determination rate: %.1f%%\n", week_rate * 100))

  # ============================================================================
  # STEP 4: Combine into unified crosswalk
  # ============================================================================

  # Month and fortnight crosswalks are at UPA-V1014 level
  # Week crosswalk is at UPA-V1008 level (household within quarter)

  # First, merge month and fortnight (both at UPA-V1014 level)
  crosswalk <- merge(
    month_xw,
    fortnight_xw,
    by = c("UPA", "V1014"),
    all = TRUE
  )

  # Add determination flags
  crosswalk[, `:=`(
    determined_month = !is.na(ref_month_in_quarter),
    determined_fortnight = !is.na(ref_fortnight_in_quarter)
  )]

  # For weeks, we need to handle the different aggregation level

  # Week crosswalk has Ano, Trimestre, UPA, V1008 - household level within quarter
  # We'll keep the week info at the most detailed level and users will join appropriately

  # Add week columns - since week is at household level within quarter,

  # we aggregate to UPA-V1014 level by taking the unique week if all households agree
  week_agg <- week_xw[, .(
    ref_week = if (length(unique(na.omit(ref_week))) == 1L) unique(na.omit(ref_week)) else as.Date(NA),
    ref_week_in_quarter = if (length(unique(na.omit(ref_week_in_quarter))) == 1L)
      unique(na.omit(ref_week_in_quarter)) else NA_integer_,
    ref_week_yyyyww = if (length(unique(na.omit(ref_week_iso_yyyyww))) == 1L)
      unique(na.omit(ref_week_iso_yyyyww)) else NA_integer_
  ), by = .(UPA, V1014)]

  # Merge week info
  crosswalk <- merge(
    crosswalk,
    week_agg,
    by = c("UPA", "V1014"),
    all.x = TRUE
  )

  # Add week determination flag
  crosswalk[, determined_week := !is.na(ref_week_in_quarter)]

  # Reorder columns for clarity
  setcolorder(crosswalk, c(
    "UPA", "V1014",
    "ref_month", "ref_month_in_quarter", "ref_month_yyyymm", "determined_month",
    "ref_fortnight", "ref_fortnight_in_quarter", "ref_fortnight_yyyyff", "determined_fortnight",
    "ref_week", "ref_week_in_quarter", "ref_week_yyyyww", "determined_week"
  ))

  # ============================================================================
  # SUMMARY
  # ============================================================================

  if (verbose) {
    cat("\nCrosswalk complete:\n")
    cat(sprintf("  - %s unique UPA-panel combinations\n", format(nrow(crosswalk), big.mark = ",")))
    cat(sprintf("  - Month determination: %.1f%%\n", mean(crosswalk$determined_month) * 100))
    cat(sprintf("  - Fortnight determination: %.1f%%\n", mean(crosswalk$determined_fortnight) * 100))
    cat(sprintf("  - Week determination: %.1f%%\n", mean(crosswalk$determined_week) * 100))
  }

  # Store determination rates as attributes
  attr(crosswalk, "determination_rates") <- list(
    month = mean(crosswalk$determined_month),
    fortnight = mean(crosswalk$determined_fortnight),
    week = mean(crosswalk$determined_week)
  )

  crosswalk
}
