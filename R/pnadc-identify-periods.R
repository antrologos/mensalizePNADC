#' Identify Reference Periods in PNADC Data
#'
#' Builds a crosswalk containing reference periods (month, fortnight,
#' and week) for PNADC survey data based on IBGE's interview timing rules.
#'
#' @description
#' PNADC is a quarterly survey, but each interview actually refers to a specific
#' temporal period within the quarter. This function identifies which month,
#' fortnight (quinzena), and week each observation belongs to, enabling
#' sub-quarterly time series analysis.
#'
#' The algorithm uses a **nested identification approach**:
#' \itemize{
#'   \item \strong{Phase 1}: Identify MONTHS for all observations using:
#'     \itemize{
#'       \item IBGE's reference week timing rules (first reference week -- ending in a Saturday -- with sufficient days)
#'       \item Respondent birthdates to constrain possible interview dates
#'       \item UPA-panel level aggregation across ALL quarters (panel design)
#'       \item Dynamic exception detection (identifies quarters needing relaxed rules)
#'     }
#'   \item \strong{Phase 2}: Identify FORTNIGHTS for month-determined observations:
#'     \itemize{
#'       \item Search space constrained to 2 fortnights within determined month
#'       \item Household-level aggregation within each quarter
#'     }
#'   \item \strong{Phase 3}: Identify WEEKS for fortnight-determined observations:
#'     \itemize{
#'       \item Search space constrained to ~2 weeks within determined fortnight
#'       \item Household-level aggregation within each quarter
#'     }
#' }
#'
#' @param data A data.frame or data.table with PNADC microdata. Required columns:
#'   \itemize{
#'     \item \code{Ano}: Survey year
#'     \item \code{Trimestre}: Quarter (1-4)
#'     \item \code{UPA}: Primary Sampling Unit
#'     \item \code{V1008}: Household id/sequence within UPA
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
#'
#' @param verbose Logical. If TRUE (default), display progress information.
#'
#' @param store_date_bounds Logical. If TRUE, stores date bounds and exception
#'   flags in the crosswalk for optimization when calling
#'   \code{pnadc_experimental_periods()}. This enables 10-20x speedup for the
#'   probabilistic strategy by avoiding redundant computation. Default FALSE.
#'
#' @return A data.table crosswalk with columns:
#'   \describe{
#'     \item{Ano, Trimestre, UPA, V1008, V1014}{Join keys (year, quarter, UPA, household, panel)}
#'     \item{ref_month_in_quarter}{Integer. Month position in quarter (1, 2, 3) or NA}
#'     \item{ref_month_in_year}{Integer. Month position in year (1-12) or NA}
#'     \item{ref_fortnight_in_month}{Integer. Fortnight position in month (1 or 2) or NA}
#'     \item{ref_fortnight_in_quarter}{Integer. Fortnight position in quarter (1-6) or NA}
#'     \item{ref_week_in_month}{Integer. Week position in month (1-4) or NA}
#'     \item{ref_week_in_quarter}{Integer. Week position in quarter (1-12) or NA}
#'     \item{date_min}{Date. Lower bound of the interview reference date for the individual. Only returned if store_date_bounds = TRUE}
#'     \item{date_max}{Date. Upper bound of the interview reference date for the individual. Only returned if store_date_bounds = TRUE}
#'     \item{week_1_start}{Date. Sunday of the IBGE first reference week of the month. Only returned if store_date_bounds = TRUE}
#'     \item{week_1_end}{Date. Saturday of the IBGE first reference week of the month. Only returned if store_date_bounds = TRUE}
#'     \item{week_2_start}{Date. Sunday of the IBGE second reference week of the month. Only returned if store_date_bounds = TRUE}
#'     \item{week_2_end}{Date. Saturday of the IBGE second reference week of the month. Only returned if store_date_bounds = TRUE}
#'     \item{week_3_start}{Date. Sunday of the IBGE third reference week of the month. Only returned if store_date_bounds = TRUE}
#'     \item{week_3_end}{Date. Saturday of the IBGE third reference week of the month. Only returned if store_date_bounds = TRUE}
#'     \item{week_4_start}{Date. Sunday of the IBGE fourth reference week of the month. Only returned if store_date_bounds = TRUE}
#'     \item{week_4_end}{Date. Saturday of the IBGE fourth reference week of the month. Only returned if store_date_bounds = TRUE}
#'     \item{month_max_upa}{Integer. Maximum month position across UPA-V1014 group (for debugging). Only returned if store_date_bounds = TRUE}
#'     \item{month_min_upa}{Integer. Minimum month position across UPA-V1014 group (for debugging). Only returned if store_date_bounds = TRUE}
#'     \item{fortnight_max_hh}{Integer. Maximum fortnight position within household (for debugging). Only returned if store_date_bounds = TRUE}
#'     \item{fortnight_min_hh}{Integer. Minimum fortnight position within household (for debugging). Only returned if store_date_bounds = TRUE}
#'     \item{week_min_hh}{Integer. Minimum week position within household (for debugging). Only returned if store_date_bounds = TRUE}
#'     \item{week_max_hh}{Integer. Maximum week position within household (for debugging). Only returned if store_date_bounds = TRUE}
#'     \item{ref_month_yyyymm}{Integer. Identified reference month in the format YYYYMM, where MM follows the IBGE calendar. 1 <= MM  <= 12}
#'     \item{ref_fortnight_yyyyff}{Integer. Identified reference fortnight in the format YYYYFF, where FF follows the IBGE calendar. 1 <= FF  <= 24}
#'     \item{ref_week_yyyyww}{Integer. Identified reference Week in the format YYYYWW, where WW follows the IBGE calendar. 1 <= WW  <= 48}
#'     \item{determined_month}{Logical. Flags if the month was determined.}
#'     \item{determined_fortnight}{Logical. Flags if the fortnight was determined.}
#'     \item{determined_week}{Logical. Flags if the week was determined.}
#'   }
#'
#' @note
#' ## Nested Identification Hierarchy
#'
#' The algorithm enforces strict nesting by construction:
#' \itemize{
#'   \item Fortnights can ONLY be identified for observations with determined months
#'   \item Weeks can ONLY be identified for observations with determined fortnights
#' }
#'
#' This guarantees: \code{determined_week => determined_fortnight => determined_month}
#'
#' ## Aggregation Levels
#'
#' The crosswalk aggregates at different levels:
#' \itemize{
#'   \item \strong{Months}: UPA-V1014 level across ALL quarters
#'     (PNADC panel design ensures same month position)
#'   \item \strong{Fortnights}: Household level within quarter only
#'   \item \strong{Weeks}: Household level within quarter only
#' }
#'
#' @details
#' ## Temporal Granularity
#'
#' The crosswalk contains three levels of temporal granularity:
#' \itemize{
#'   \item \strong{Month}: 3 per quarter, ~97% determination rate (aggregates across quarters)
#'   \item \strong{Fortnight (quinzena)}: 6 per quarter, ~2-8% determination rate (within-quarter only)
#'   \item \strong{Week}: 12 per quarter, ~1-2% determination rate (within-quarter only)
#' }
#'
#' ## Cross-Quarter Aggregation (Important!)
#'
#' **For optimal month determination rates, input data should be stacked across
#' multiple quarters** (ideally 4+ years). The algorithm leverages PNADC's
#' rotating panel design where the same UPA-V1014 is interviewed in the same
#' relative position across quarterly visits.
#'
#' ## Fortnight Definition
#'
#' Fortnights are numbered 1-6 per quarter (2 per month):
#' \itemize{
#'   \item Fortnight 01: Jan 1-15
#'   \item Fortnight 02: Jan 16-31
#'   \item Fortnight 03: Feb 1-15
#'   \item ... and so on
#' }
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
#' # Verify nesting (always TRUE by construction)
#' crosswalk[determined_fortnight, all(determined_month)]
#' crosswalk[determined_week, all(determined_fortnight)]
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
pnadc_identify_periods <- function(data, verbose = TRUE, store_date_bounds = FALSE) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  # Validate parameters
  checkmate::assert_flag(verbose)
  checkmate::assert_flag(store_date_bounds)

  # Freeing memory before starting...
  gc()

  if (verbose) {
    cat("Building reference period crosswalk (nested identification)...\n")
  }

  # Validate required columns
  validate_pnadc(data, check_weights = FALSE)

  # Subset to required columns BEFORE copying (80-90% memory reduction)
  # Instead of copying all 50+ columns, only copy the ~9 columns we actually need
  required_cols <- PNADCperiods:::required_vars_ref_month()
  dt            <- PNADCperiods:::subset_and_copy(data, required_cols)

  # Freeing memory after making the copy...
  gc()

  # ============================================================================
  # PHASE 1: MONTH IDENTIFICATION (ALL observations)
  # ============================================================================
  #
  # This phase identifies the reference MONTH for each observation using:
  # - IBGE's reference week timing rules (parada técnica)
  # - Birthday constraints to narrow possible interview dates
  # - Cross-quarter aggregation at UPA-V1014 level (panel design)
  # - Dynamic exception detection for edge cases
  #
  # ============================================================================

  if (verbose) cat("\nPhase 1: Identifying MONTHS (all observations)...\n")

  # --------------------------------------------------------------------------
  # STEP 1.1: Preprocessing - Convert columns and handle special codes
  # --------------------------------------------------------------------------

  if (verbose) cat("  Step 1.1: Preprocessing data...\n")

  # Converts to integer (including V2009 - saves 4 bytes per row vs numeric)
  num_cols = c("Ano", "Trimestre", "V2008", "V20081", "V20082", "V2009")
  dt[ , names(.SD) := lapply(.SD, as.numeric), .SDcols = num_cols]

  dt[V2008  == 99,  V2008  := NA]
  dt[V20081 == 99,  V20081 := NA]
  dt[V20082 == 9999,V20082 := NA]

  # Initialize output columns to ensure they exist even if phases are skipped
  dt[, `:=`(
    ref_month_in_quarter     = NA_integer_,
    ref_month_in_year        = NA_integer_,
    ref_fortnight_in_month   = NA_integer_,
    ref_fortnight_in_quarter = NA_integer_,
    ref_week_in_month        = NA_integer_,
    ref_week_in_quarter      = NA_integer_
  )]

  # --------------------------------------------------------------------------
  # STEP 1.2: Pre-compute first valid Saturdays for each unique (year, quarter)
  # --------------------------------------------------------------------------

  if (verbose) cat("  Step 1.2: Computing valid interview Saturdays...\n")

  unique_quarters <- unique(dt[, .(Ano, Trimestre)])
  unique_quarters[, `:=`(
    month1 = PNADCperiods:::quarter_month_n(Trimestre, 1L),
    month2 = PNADCperiods:::quarter_month_n(Trimestre, 2L),
    month3 = PNADCperiods:::quarter_month_n(Trimestre, 3L),

    is_leap = PNADCperiods:::is_leap_year(Ano)

  )][, `:=`(

    # Standard rule (min_days=4)
    first_sat_m1 = PNADCperiods:::first_valid_saturday(Ano, month1, min_days = 4L),
    first_sat_m2 = PNADCperiods:::first_valid_saturday(Ano, month2, min_days = 4L),
    first_sat_m3 = PNADCperiods:::first_valid_saturday(Ano, month3, min_days = 4L),

    # Exception rule (min_days=3)
    alt_sat_m1 = PNADCperiods:::first_valid_saturday(Ano, month1, min_days = 3L),
    alt_sat_m2 = PNADCperiods:::first_valid_saturday(Ano, month2, min_days = 3L),
    alt_sat_m3 = PNADCperiods:::first_valid_saturday(Ano, month3, min_days = 3L)

  )][, `:=`(

    # Date bounds (for regular first reference weeks, with 4 or more days in the IBGE reference month)
    # date_min: Sunday of first reference week (first_sat - 6 days, but we use Saturday here)
    # date_max: End of 4th week of month 3 (first_sat + 21 days = 3 more weeks after Saturday)
    date_min = lubridate::ymd(paste(Ano, month1, first_sat_m1, sep = "-")),
    date_max = lubridate::ymd(paste(Ano, month3, first_sat_m3, sep = "-")) + 21L,

    # Calculate alternative bounds (for exception handling: reference weeks with 3 or more days in the IBGE reference month)
    alt_date_min = lubridate::ymd(paste(Ano, month1, alt_sat_m1, sep = "-")),
    alt_date_max = lubridate::ymd(paste(Ano, month3, alt_sat_m3, sep = "-")) + 21L
  )]


  # Join pre-computed values to main data
  dt[unique_quarters,
     on = .(Ano, Trimestre),
     `:=`(month1 = i.month1,
          month2 = i.month2,
          month3 = i.month3,

          first_sat_m1 = i.first_sat_m1,
          first_sat_m2 = i.first_sat_m2,
          first_sat_m3 = i.first_sat_m3,

          alt_sat_m1 = i.alt_sat_m1,
          alt_sat_m2 = i.alt_sat_m2,
          alt_sat_m3 = i.alt_sat_m3,

          date_min = i.date_min,
          date_max = i.date_max,

          alt_date_min = i.alt_date_min,
          alt_date_max = i.alt_date_max,

          is_leap = i.is_leap
          )
     ]

  # Remove unique_quarters - no longer needed
  rm(unique_quarters)

  # --------------------------------------------------------------------------
  # STEP 1.3: Calculate birthdays and adjust for leap years
  # --------------------------------------------------------------------------

  if (verbose) cat("  Step 1.3: Calculating birthdays and adjusting for leap years...\n")

  dt[!is.na(V2008), birthday := lubridate::ymd(paste(Ano, V20081, V2008, sep = "-"), quiet = TRUE)]

  # Handling 29th February birthdays on non-leap years
  dt[(!is_leap) & V2008 == 29 & V20081 == 2, birthday := lubridate::ymd(paste(Ano, 3, 1, sep = "-"))]

  # Diagnostic: Warn if many birthdays failed to parse
  n_failed_birthday <- dt[!is.na(V2008) & is.na(birthday), .N]
  n_valid_birthday_input <- dt[!is.na(V2008), .N]
  if (n_failed_birthday > 0 && n_valid_birthday_input > 0) {
    pct_failed <- 100 * n_failed_birthday / n_valid_birthday_input
    if (pct_failed > 5 && verbose) {
      warning(sprintf("%.1f%% of birthdays failed to parse (%d of %d). Check V2008/V20081 data quality.",
                      pct_failed, n_failed_birthday, n_valid_birthday_input), call. = FALSE)
    }
  }

  dt[, first_sat_after_birthday := PNADCperiods:::first_saturday_on_or_after(birthday)]

  # Determine if interview was before or after birthday
  dt[!is.na(V20082), visit_before_birthday := (Ano - V20082) - V2009]

  # --------------------------------------------------------------------------
  # STEP 1.4: Calculate birthday constraints
  # --------------------------------------------------------------------------

  if (verbose) cat("  Step 1.4: Applying birthday constraints...\n")

  # Apply birthday constraints
  dt[visit_before_birthday == 0L &
       !is.na(first_sat_after_birthday) &
       first_sat_after_birthday > date_min &
       first_sat_after_birthday <= date_max,
     date_min := first_sat_after_birthday]

  dt[visit_before_birthday == 1L &
       !is.na(first_sat_after_birthday) &
       (first_sat_after_birthday - 7L) < date_max &
       (first_sat_after_birthday - 7L) >= date_min,
     date_max := first_sat_after_birthday - 7L]

  # Apply birthday constraints to alternative bounds
  dt[visit_before_birthday == 0L &
       !is.na(first_sat_after_birthday) &
       first_sat_after_birthday > alt_date_min &
       first_sat_after_birthday <= alt_date_max,
     alt_date_min := first_sat_after_birthday]

  dt[visit_before_birthday == 1L &
       !is.na(first_sat_after_birthday) &
       (first_sat_after_birthday - 7L) < alt_date_max &
       (first_sat_after_birthday - 7L) >= alt_date_min,
     alt_date_max := first_sat_after_birthday - 7L]

  # Clean up birthday column
  dt[, birthday := NULL]

  # Freeing memory after deleting columns...
  gc()

  # --------------------------------------------------------------------------
  # STEP 1.5: Convert date bounds to MONTH positions
  # --------------------------------------------------------------------------

  if (verbose) cat("  Step 1.5: Converting to month positions...\n")

  dt[, `:=`(
    month_min_pos     = PNADCperiods:::calculate_month_position_min(date = date_min,     year = Ano, quarter = Trimestre, day_threshold = 3L),
    month_max_pos     = PNADCperiods:::calculate_month_position_max(date = date_max,     year = Ano, quarter = Trimestre, day_threshold = 3L),
    alt_month_min_pos = PNADCperiods:::calculate_month_position_min(date = alt_date_min, year = Ano, quarter = Trimestre, day_threshold = 2L),
    alt_month_max_pos = PNADCperiods:::calculate_month_position_max(date = alt_date_max, year = Ano, quarter = Trimestre, day_threshold = 2L)
  )]

  # --------------------------------------------------------------------------
  # STEP 1.6: MONTH aggregation at UPA-V1014 level ACROSS ALL quarters
  # --------------------------------------------------------------------------

  if (verbose) cat("  Step 1.6: Aggregating at UPA-panel level (cross-quarter)...\n")

  # Set key for faster groupby operations
  data.table::setkey(dt, UPA, V1014)

  dt[, `:=`(
    upa_month_min     = max(month_min_pos,     na.rm = TRUE),
    upa_month_max     = min(month_max_pos,     na.rm = TRUE),
    alt_upa_month_min = max(alt_month_min_pos, na.rm = TRUE),
    alt_upa_month_max = min(alt_month_max_pos, na.rm = TRUE)
  ), by = .(UPA, V1014)]

  # Handle all-NA groups (max returns -Inf, min returns Inf)
  dt[is.infinite(upa_month_min),     upa_month_min     := NA_integer_]
  dt[is.infinite(upa_month_max),     upa_month_max     := NA_integer_]
  dt[is.infinite(alt_upa_month_min), alt_upa_month_min := NA_integer_]
  dt[is.infinite(alt_upa_month_max), alt_upa_month_max := NA_integer_]

  # --------------------------------------------------------------------------
  # STEP 1.7: Dynamic exception detection for MONTHS
  # --------------------------------------------------------------------------

  if (verbose) cat("  Step 1.7: Detecting exception cases...\n")

  dt[, requires_exception := (
    upa_month_min > upa_month_max &
      alt_upa_month_min <= alt_upa_month_max &
      ((month_max_pos == upa_month_max & month_max_pos < alt_upa_month_max) |
         (month_min_pos == upa_month_min & month_min_pos > alt_upa_month_min))
  )]

  # Determine which month within quarter requires exception
  dt[, `:=`(
    requires_exc_m1 = requires_exception & month_min_pos == 2L & alt_upa_month_min == 1L,
    requires_exc_m2 = requires_exception & ((month_max_pos == 1L & alt_upa_month_max >= 2L) |
                                              (month_min_pos == 3L & alt_upa_month_min <= 2L)),
    requires_exc_m3 = requires_exception & month_max_pos == 2L & alt_upa_month_max == 3L
  )]

  # Propagate exception requirement to entire quarter
  dt[, `:=`(
    trim_exc_m1 = sum(requires_exc_m1, na.rm = TRUE) > 0L,
    trim_exc_m2 = sum(requires_exc_m2, na.rm = TRUE) > 0L,
    trim_exc_m3 = sum(requires_exc_m3, na.rm = TRUE) > 0L
  ), by = .(Ano, Trimestre)]

  # Apply month exception rules
  exc_condition <- (dt$trim_exc_m1 == 1L | dt$trim_exc_m2 == 1L | dt$trim_exc_m3 == 1L)
  has_any_exception <- any(exc_condition)

  if (has_any_exception) {
    if (verbose) cat("  Step 1.7b: Applying exception rules...\n")


    # Update first Saturday values where exceptions are needed
    dt[trim_exc_m1 == 1L, first_sat_m1 := alt_sat_m1]
    dt[trim_exc_m2 == 1L, first_sat_m2 := alt_sat_m2]
    dt[trim_exc_m3 == 1L, first_sat_m3 := alt_sat_m3]


    # Recalculate date bounds for exception rows
    dt[exc_condition, `:=`(
      date_min = lubridate::ymd(paste(Ano, month1, first_sat_m1, sep = "-")),
      date_max = lubridate::ymd(paste(Ano, month3, first_sat_m3, sep = "-")) + 21L
    )]

    # Re-apply birthday constraints
    dt[exc_condition &
         visit_before_birthday == 0L &
         !is.na(first_sat_after_birthday) &
         first_sat_after_birthday > date_min &
         first_sat_after_birthday <= date_max,
       date_min := first_sat_after_birthday]

    dt[exc_condition &
         visit_before_birthday == 1L &
         !is.na(first_sat_after_birthday) &
         (first_sat_after_birthday - 7L) < date_max &
         (first_sat_after_birthday - 7L) >= date_min,
       date_max := first_sat_after_birthday - 7L]

    # Recalculate month positions with dynamic thresholds
    dt[exc_condition, `:=`(
      month_min_pos = PNADCperiods:::calculate_month_position_min_dynamic(date = date_min,
                                                                          year = Ano,
                                                                          quarter = Trimestre,
                                                                          exc_m1 = trim_exc_m1,
                                                                          exc_m2 = trim_exc_m2,
                                                                          exc_m3 = trim_exc_m3),

      month_max_pos = PNADCperiods:::calculate_month_position_max_dynamic(date = date_max,
                                                                          year = Ano,
                                                                          quarter = Trimestre,
                                                                          exc_m1 = trim_exc_m1,
                                                                          exc_m2 = trim_exc_m2,
                                                                          exc_m3 = trim_exc_m3)
    )]

    # Re-aggregate ALL UPA-V1014 combinations
    dt[, `:=`(
      upa_month_min_final = max(month_min_pos, na.rm = TRUE),
      upa_month_max_final = min(month_max_pos, na.rm = TRUE)
    ), by = .(UPA, V1014)]

    # Handle all-NA groups (max returns -Inf, min returns Inf)
    dt[is.infinite(upa_month_min_final), upa_month_min_final := NA_integer_]
    dt[is.infinite(upa_month_max_final), upa_month_max_final := NA_integer_]
  } else {
    # No exceptions - reuse aggregation
    dt[, `:=`(
      upa_month_min_final = upa_month_min,
      upa_month_max_final = upa_month_max
    )]
  }

  # Clean up intermediate columns no longer needed
  cols_to_remove <- c("is_leap", "alt_sat_m1", "alt_sat_m2", "alt_sat_m3",
                      "alt_date_min", "alt_date_max",
                      "first_sat_after_birthday", "visit_before_birthday",
                      "month_min_pos", "month_max_pos",
                      "alt_month_min_pos", "alt_month_max_pos",
                      "upa_month_min", "upa_month_max",
                      "alt_upa_month_min", "alt_upa_month_max",
                      "requires_exception", "requires_exc_m1", "requires_exc_m2", "requires_exc_m3",
                      "trim_exc_m1", "trim_exc_m2", "trim_exc_m3")
  cols_present <- intersect(cols_to_remove, names(dt))
  if (length(cols_present) > 0) {
    dt[, (cols_present) := NULL]
  }

  # --------------------------------------------------------------------------
  # STEP 1.8: MONTH DETERMINATION
  # --------------------------------------------------------------------------

  if (verbose) cat("  Step 1.8: Determining reference months...\n")

  dt[upa_month_min_final == upa_month_max_final &
       upa_month_min_final >= 1L & upa_month_max_final <= 3L,
     ref_month_in_quarter := upa_month_min_final]

  dt[!is.na(ref_month_in_quarter),
      ref_month_in_year := ref_month_in_quarter + (Trimestre - 1)*3
     ]

  # Freeing memory after finishing the month determination...
  gc()

  # Calculate stats
  n_total            <- nrow(dt)
  n_month_determined <- dt[!is.na(ref_month_in_quarter), .N]
  month_rate         <- n_month_determined / n_total

  if (verbose) {
    cat(sprintf("  -> Month determination: %.1f%% (%s of %s observations)\n",
                month_rate * 100,
                format(n_month_determined, big.mark = ","),
                format(n_total, big.mark = ",")))
  }

  # ============================================================================
  # PHASE 2: FORTNIGHT IDENTIFICATION (ONLY month-determined observations)
  # ============================================================================
  #
  # Fortnights are identified ONLY for observations where the month is already

  # determined. The search space is constrained to 2 fortnights within the
  # determined month:
  #   - Month 1 → fortnights 1-2
  #   - Month 2 → fortnights 3-4
  #   - Month 3 → fortnights 5-6
  #
  # ============================================================================

  if (verbose) cat("\nPhase 2: Identifying FORTNIGHTS (within determined months)...\n")

  # Initialize fortnight columns
  dt[, ref_fortnight_in_quarter := NA_integer_]

  # Initialize counter (may be updated in else branch)
  n_fortnight_determined <- 0L

  # Pre-compute IBGE month boundaries for all unique (Ano, Trimestre, ref_month_in_quarter) combinations
  # Created here (outside conditional) so it's available for both Phase 2 and Phase 3
  month_boundaries <- unique(dt[!is.na(ref_month_in_quarter),
            .(Ano,
              Trimestre,
              ref_month_in_quarter,
              month = ref_month_in_quarter + (Trimestre - 1)*3,
              ibge_first_sat_day  = fcase(ref_month_in_quarter == 1, first_sat_m1,
                                          ref_month_in_quarter == 2, first_sat_m2,
                                          ref_month_in_quarter == 3, first_sat_m3))])
  if (nrow(month_boundaries) > 0) {
    month_boundaries[, ibge_month_start := lubridate::ymd(paste(Ano, month, ibge_first_sat_day, sep = "-")) - 6L]
    data.table::setkey(month_boundaries, Ano, Trimestre, ref_month_in_quarter)
  }

  if (n_month_determined == 0) {
    if (verbose) cat("  -> No months determined - skipping fortnight identification\n")
  } else {
    if (verbose) cat(sprintf("  Processing %s observations with determined months...\n",
                             format(n_month_determined, big.mark = ",")))

    # --------------------------------------------------------------------------
    # STEP 2.1: Calculate fortnight bounds WITHIN the determined month
    # --------------------------------------------------------------------------

    if (verbose) cat("  Step 2.1: Calculate fortnight bounds WITHIN the determined month...\n")

    # Pre-compute IBGE fortnight boundaries
    # IBGE reference months are 28-day periods starting on Sunday of the first reference week
    # Fortnight 1: Days 0-13 (weeks 1-2), Fortnight 2: Days 14-27 (weeks 3-4)
    fortnight_boundaries = copy(month_boundaries)

    fortnight_boundaries[ , `:=`(
      fortnight_1_start = ibge_month_start,           # Day 0 of IBGE month
      fortnight_1_end   = ibge_month_start + 13L,     # Day 13 (14 days: weeks 1-2)
      fortnight_2_start = ibge_month_start + 14L,     # Day 14 (start of weeks 3-4)
      fortnight_2_end   = ibge_month_start + 27L      # Day 27 (28 days total)
      )][,
         `:=`(
           month = NULL,
           ibge_first_sat_day = NULL,
           ibge_month_start   = NULL
         )]

    # Join pre-computed fortnight boundary values to main data
    dt[fortnight_boundaries,
       on = .(Ano, Trimestre, ref_month_in_quarter),
       `:=`(fortnight_1_start = i.fortnight_1_start,
            fortnight_1_end   = i.fortnight_1_end,
            fortnight_2_start = i.fortnight_2_start,
            fortnight_2_end   = i.fortnight_2_end)]


    # Adjust date_min and date_max for those with determined months
    dt[!is.na(ref_month_in_quarter),
       `:=`(
         date_min  = fifelse(is.na(date_min), fortnight_1_start, date_min),
         date_max  = fifelse(is.na(date_max), fortnight_2_end,   date_max))]

    dt[!is.na(ref_month_in_quarter),
       `:=`(
         date_min  = fifelse(date_min < fortnight_1_start, fortnight_1_start, date_min),
         date_max  = fifelse(date_max > fortnight_2_end,   fortnight_2_end,   date_max)
       )]


    # Identifying the fortnight for individuals
    dt[!is.na(ref_month_in_quarter),
       `:=`(
         fortnight_pos = fcase(
           (date_min >= fortnight_1_start) & (date_max <= fortnight_1_end), 1,
           (date_min >= fortnight_2_start) & (date_max <= fortnight_2_end), 2,
           default = NA_real_)
    )]

    # Freeing memory after this step...
    gc()

    # --------------------------------------------------------------------------
    # STEP 2.2: FORTNIGHT aggregation at household-quarter level
    # --------------------------------------------------------------------------

    if (verbose) cat("  Step 2.2: Aggregating at household level (within quarter)...\n")

    # Aggregate ONLY for month-determined rows
    dt[!is.na(ref_month_in_quarter), `:=`(
      hh_fortnight_max     = max(fortnight_pos,     na.rm = TRUE),
      hh_fortnight_min     = min(fortnight_pos,     na.rm = TRUE)
    ), by = .(Ano, Trimestre, UPA, V1008)]

    # Handle all-NA groups (max returns -Inf, min returns Inf)
    dt[is.infinite(hh_fortnight_max), hh_fortnight_max := NA_integer_]
    dt[is.infinite(hh_fortnight_min), hh_fortnight_min := NA_integer_]

    # --------------------------------------------------------------------------
    # STEP 2.3: FORTNIGHT DETERMINATION
    # --------------------------------------------------------------------------

    if (verbose) cat("  Step 2.3: Determining reference fortnights...\n")

    dt[!is.na(ref_month_in_quarter) &
         hh_fortnight_min == hh_fortnight_max,
       `:=`(
         ref_fortnight_in_month   = hh_fortnight_min,
         ref_fortnight_in_quarter = hh_fortnight_min + (ref_month_in_quarter - 1)*2)
       ]

    # Calculate stats
    n_fortnight_determined <- dt[!is.na(ref_fortnight_in_quarter), .N]
    fortnight_rate         <- n_fortnight_determined / n_total

    if (verbose) {
      cat(sprintf("  -> Fortnight determination: %.1f%% (%s of %s observations)\n",
                  fortnight_rate * 100,
                  format(n_fortnight_determined, big.mark = ","),
                  format(n_total, big.mark = ",")))
    }

    # Clean up intermediate fortnight columns no longer needed
    # (hh_fortnight_max/min are kept for store_date_bounds option)
    cols_to_remove <- c("fortnight_pos", "fortnight_1_start", "fortnight_1_end",
                        "fortnight_2_start", "fortnight_2_end")
    cols_present <- intersect(cols_to_remove, names(dt))
    if (length(cols_present) > 0) {
      dt[, (cols_present) := NULL]
    }
  }

  # ============================================================================
  # PHASE 3: WEEK IDENTIFICATION (ONLY fortnight-determined observations)
  # ============================================================================
  #
  # Weeks are identified ONLY for observations where the fortnight is already
  # determined. The search space is constrained to ~2 weeks within the
  # determined fortnight (fortnights typically span 2-3 ISO weeks).
  #
  # ============================================================================

  if (verbose) cat("\nPhase 3: Identifying WEEKS (within determined fortnights)...\n")


  if (n_fortnight_determined == 0) {
    if (verbose) cat("  -> No fortnights determined - skipping week identification\n")
  } else {
    if (verbose) cat(sprintf("  Processing %s observations with determined fortnights...\n",
                             format(n_fortnight_determined, big.mark = ",")))

    # --------------------------------------------------------------------------
    # STEP 3.1: Calculate week bounds WITHIN the determined fortnight
    # --------------------------------------------------------------------------

    # Pre-compute IBGE week boundaries
    # IBGE reference months are 28-day periods (4 weeks x 7 days), starting Sunday
    # Each week runs Sunday (day N) to Saturday (day N+6)
    weeks_boundaries = copy(month_boundaries)

    weeks_boundaries[ , `:=`(
      week_1_start = ibge_month_start,          # Day 0: Week 1 Sunday
      week_1_end   = ibge_month_start + 6L,     # Day 6: Week 1 Saturday

      week_2_start = ibge_month_start + 7L,     # Day 7: Week 2 Sunday
      week_2_end   = ibge_month_start + 13L,    # Day 13: Week 2 Saturday

      week_3_start = ibge_month_start + 14L,    # Day 14: Week 3 Sunday
      week_3_end   = ibge_month_start + 20L,    # Day 20: Week 3 Saturday

      week_4_start = ibge_month_start + 21L,    # Day 21: Week 4 Sunday
      week_4_end   = ibge_month_start + 27L     # Day 27: Week 4 Saturday
    )][,
       `:=`(
         month = NULL,
         ibge_first_sat_day = NULL,
         ibge_month_start   = NULL
       )]

    # Join pre-computed fortnight boundary values to main data
    dt[weeks_boundaries,
       on = .(Ano, Trimestre, ref_month_in_quarter),
       `:=`(
         week_1_start = i.week_1_start,
         week_1_end   = i.week_1_end,
         week_2_start = i.week_2_start,
         week_2_end   = i.week_2_end,
         week_3_start = i.week_3_start,
         week_3_end   = i.week_3_end,
         week_4_start = i.week_4_start,
         week_4_end   = i.week_4_end
       )]

    # Adjust date_min and date_max for those with determined months
    dt[!is.na(ref_fortnight_in_month),
       `:=`(
         date_min  = fifelse(ref_fortnight_in_month == 1L & is.na(date_min), week_1_start, date_min),
         date_max  = fifelse(ref_fortnight_in_month == 1L & is.na(date_max), week_2_end,   date_max))][,
       `:=`(
         date_min  = fifelse(ref_fortnight_in_month == 2L & is.na(date_min), week_3_start, date_min),
         date_max  = fifelse(ref_fortnight_in_month == 2L & is.na(date_max), week_4_end,   date_max)
         )]


    dt[!is.na(ref_fortnight_in_month),
       `:=`(
         date_min  = fifelse(ref_fortnight_in_month == 1L & date_min < week_1_start, week_1_start, date_min),
         date_max  = fifelse(ref_fortnight_in_month == 1L & date_max > week_2_end,   week_2_end,   date_max)
       )][,
       `:=`(
         date_min  = fifelse(ref_fortnight_in_month == 2L & date_min < week_3_start, week_3_start, date_min),
         date_max  = fifelse(ref_fortnight_in_month == 2L & date_max > week_4_end,   week_4_end,   date_max)
         )]

    # Identifying the weeks for individuals
    dt[!is.na(ref_fortnight_in_quarter),
       `:=`(
         week_pos = fcase(
           (ref_fortnight_in_month == 1L) &  (date_min >= week_1_start) & (date_max <= week_1_end), 1,
           (ref_fortnight_in_month == 1L) &  (date_min >= week_2_start) & (date_max <= week_2_end), 2,
           (ref_fortnight_in_month == 2L) &  (date_min >= week_3_start) & (date_max <= week_3_end), 3,
           (ref_fortnight_in_month == 2L) &  (date_min >= week_4_start) & (date_max <= week_4_end), 4,
           default = NA_real_)
       )]

    # Freeing memory after this step...
    gc()

    # --------------------------------------------------------------------------
    # STEP 3.2: WEEK aggregation at household-quarter level
    # --------------------------------------------------------------------------

    if (verbose) cat("  Step 3.2: Aggregating at household level (within quarter)...\n")

    dt[!is.na(ref_fortnight_in_quarter), `:=`(
      hh_week_max = max(week_pos, na.rm = TRUE),
      hh_week_min = min(week_pos, na.rm = TRUE)
    ), by = .(Ano, Trimestre, UPA, V1008)]

    # Handle all-NA groups (max returns -Inf, min returns Inf)
    dt[is.infinite(hh_week_max), hh_week_max := NA_integer_]
    dt[is.infinite(hh_week_min), hh_week_min := NA_integer_]

    # --------------------------------------------------------------------------
    # STEP 3.4: WEEK DETERMINATION (using sequential values for comparison)
    # --------------------------------------------------------------------------

    if (verbose) cat("  Step 3.4: Determining reference weeks...\n")

    # Determine if min == max using sequential values (handles year boundaries)
    dt[!is.na(ref_fortnight_in_quarter) &
         hh_week_min == hh_week_max,
       `:=`(
         ref_week_in_month   = hh_week_min,
         ref_week_in_quarter = hh_week_min + (ref_month_in_quarter - 1)*4)]


    # Calculate stats
    n_week_determined <- dt[!is.na(ref_week_in_month), .N]
    week_rate         <- n_week_determined / n_total

    if (verbose) {
      cat(sprintf("  -> Week determination: %.1f%% (%s of %s observations)\n",
                  week_rate * 100,
                  format(n_week_determined, big.mark = ","),
                  format(n_total, big.mark = ",")))
    }

    # Clean up intermediate week column no longer needed
    # (hh_week_max/min and week_*_start/end are kept for store_date_bounds option)
    if ("week_pos" %in% names(dt)) {
      dt[, week_pos := NULL]
    }
  }

  # ============================================================================
  # PHASE 4: BUILD CROSSWALK
  # ============================================================================

  if (verbose) cat("\nPhase 4: Building crosswalk...\n")


  # Build crosswalk at household-quarter level
  if (!store_date_bounds) {

  dt = dt[,
     .(Ano, Trimestre, UPA, V1008, V1014,

       ref_month_in_quarter,
       ref_month_in_year,

       ref_fortnight_in_month,
       ref_fortnight_in_quarter,

       ref_week_in_month,
       ref_week_in_quarter)
     ]

  } else {
    # Build column list dynamically to handle cases where phases were skipped
    # Base columns that always exist
    cols_to_keep <- c("Ano", "Trimestre", "UPA", "V1008", "V1014",
                      "ref_month_in_quarter", "ref_month_in_year",
                      "ref_fortnight_in_month", "ref_fortnight_in_quarter",
                      "ref_week_in_month", "ref_week_in_quarter",
                      "date_max", "date_min",
                      "upa_month_max_final", "upa_month_min_final")

    # Conditionally add fortnight bounds if they exist
    if ("hh_fortnight_max" %in% names(dt)) {
      cols_to_keep <- c(cols_to_keep, "hh_fortnight_max", "hh_fortnight_min")
    }

    # Conditionally add week bounds if they exist
    if ("hh_week_max" %in% names(dt)) {
      cols_to_keep <- c(cols_to_keep, "hh_week_max", "hh_week_min",
                        "week_1_start", "week_1_end",
                        "week_2_start", "week_2_end",
                        "week_3_start", "week_3_end",
                        "week_4_start", "week_4_end")
    }

    dt <- dt[, ..cols_to_keep]

    # Rename columns for output
    data.table::setnames(dt,
                         old = c("upa_month_max_final", "upa_month_min_final"),
                         new = c("month_max_upa", "month_min_upa"))
    if ("hh_fortnight_max" %in% names(dt)) {
      data.table::setnames(dt,
                           old = c("hh_fortnight_max", "hh_fortnight_min"),
                           new = c("fortnight_max_hh", "fortnight_min_hh"))
    }
    if ("hh_week_max" %in% names(dt)) {
      data.table::setnames(dt,
                           old = c("hh_week_max", "hh_week_min"),
                           new = c("week_max_hh", "week_min_hh"))
    }
  }

  dt[, `:=`(

    ref_month_yyyymm     = Ano * 100 + ref_month_in_year,
    ref_fortnight_yyyyff = Ano * 100 + (ref_fortnight_in_quarter + (Trimestre - 1)*6),
    ref_week_yyyyww      = Ano * 100 + (ref_week_in_month + (ref_month_in_year - 1)*4),

    determined_month     = !is.na(ref_month_in_quarter),
    determined_fortnight = !is.na(ref_fortnight_in_quarter),
    determined_week      = !is.na(ref_week_in_quarter)
    )]


  gc()

  # ============================================================================
  # SUMMARY
  # ============================================================================

  if (verbose) {
    cat("\n========================================\n")
    cat("Crosswalk complete (nested identification):\n")
    cat("========================================\n")
  }

  dt
}
