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
#' The algorithm uses a **nested identification approach**:
#' \itemize{
#'   \item \strong{Phase 1}: Identify MONTHS for all observations using:
#'     \itemize{
#'       \item IBGE's reference week timing rules (first Saturday with sufficient days)
#'       \item Respondent birthdates to constrain possible interview dates
#'       \item UPA-panel level aggregation across ALL quarters (panel design)
#'       \item Dynamic exception detection (identifies quarters needing relaxed rules)
#'     }
#'   \item \strong{Phase 2}: Identify FORTNIGHTS only for month-determined observations:
#'     \itemize{
#'       \item Search space constrained to 2 fortnights within determined month
#'       \item Household-level aggregation within each quarter
#'     }
#'   \item \strong{Phase 3}: Identify WEEKS only for fortnight-determined observations:
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
#'     \item{Ano, Trimestre, UPA, V1008, V1014}{Join keys (year, quarter, UPA, household, panel)}
#'     \item{ref_month_start}{Sunday of first IBGE reference week of the month}
#'     \item{ref_month_end}{Saturday of last IBGE reference week of the month}
#'     \item{ref_month_in_quarter}{Position in quarter (1, 2, 3) or NA}
#'     \item{ref_month_yyyymm}{Integer YYYYMM format (e.g., 202301)}
#'     \item{ref_month_weeks}{Number of IBGE reference weeks in month (4 or 5)}
#'     \item{determined_month}{Logical: TRUE if month was determined}
#'     \item{ref_fortnight_start}{Sunday of first IBGE week of the fortnight}
#'     \item{ref_fortnight_end}{Saturday of last IBGE week of the fortnight}
#'     \item{ref_fortnight_in_quarter}{Position in quarter (1-6) or NA}
#'     \item{ref_fortnight_yyyyff}{Integer YYYYFF format (1-24 per year)}
#'     \item{determined_fortnight}{Logical: TRUE if fortnight was determined}
#'     \item{ref_week_start}{Sunday of the IBGE reference week}
#'     \item{ref_week_end}{Saturday of the IBGE reference week}
#'     \item{ref_week_in_quarter}{Position in quarter (1-14) or NA}
#'     \item{ref_week_yyyyww}{Integer IBGE YYYYWW format}
#'     \item{determined_week}{Logical: TRUE if week was determined}
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
#'   \item \strong{Week}: 13-14 per quarter, ~1-2% determination rate (within-quarter only)
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
#' Fortnights are numbered 1-24 per year (2 per month):
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
#' @param store_date_bounds Logical. If TRUE, stores date bounds and exception
#'   flags in the crosswalk for optimization when calling
#'   \code{pnadc_experimental_periods()}. This enables 10-20x speedup for the
#'   probabilistic strategy by avoiding redundant computation. Default FALSE.
#'
#' @export
pnadc_identify_periods <- function(data, verbose = TRUE, store_date_bounds = FALSE) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  if (verbose) {
    cat("Building reference period crosswalk (nested identification)...\n")
  }

  # Validate required columns
  validate_pnadc(data, check_weights = FALSE)

  # OPTIMIZATION: Subset to required columns BEFORE copying (80-90% memory reduction)
  # Instead of copying all 50+ columns, only copy the ~9 columns we actually need
  required_cols <- required_vars_ref_month()
  dt <- subset_and_copy(data, required_cols)

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

  # OPTIMIZATION: Use consolidated helper for type conversion and NA code handling
  # Converts to integer (including V2009 - saves 4 bytes per row vs numeric)
  convert_pnadc_columns(dt)

  # --------------------------------------------------------------------------
  # STEP 1.2: Pre-compute first valid Saturdays for each unique (year, quarter)
  # --------------------------------------------------------------------------

  if (verbose) cat("  Step 1.2: Computing valid interview Saturdays...\n")

  unique_quarters <- unique(dt[, .(Ano, Trimestre)])
  unique_quarters[, `:=`(
    month1 = quarter_month_n(Trimestre, 1L),
    month2 = quarter_month_n(Trimestre, 2L),
    month3 = quarter_month_n(Trimestre, 3L)
  )]

  # Standard rule (min_days=4)
  unique_quarters[, `:=`(
    first_sat_m1 = first_valid_saturday(Ano, month1, min_days = 4L),
    first_sat_m2 = first_valid_saturday(Ano, month2, min_days = 4L),
    first_sat_m3 = first_valid_saturday(Ano, month3, min_days = 4L)
  )]

  # Exception rule (min_days=3)
  unique_quarters[, `:=`(
    alt_sat_m1 = first_valid_saturday(Ano, month1, min_days = 3L),
    alt_sat_m2 = first_valid_saturday(Ano, month2, min_days = 3L),
    alt_sat_m3 = first_valid_saturday(Ano, month3, min_days = 3L)
  )]

  # Join pre-computed values to main data
  dt[unique_quarters,
     on = .(Ano, Trimestre),
     `:=`(month1 = i.month1, month2 = i.month2, month3 = i.month3,
          first_sat_m1 = i.first_sat_m1, first_sat_m2 = i.first_sat_m2,
          first_sat_m3 = i.first_sat_m3,
          alt_sat_m1 = i.alt_sat_m1, alt_sat_m2 = i.alt_sat_m2,
          alt_sat_m3 = i.alt_sat_m3)]

  # OPTIMIZATION: Remove unique_quarters - no longer needed
  rm(unique_quarters)

  # --------------------------------------------------------------------------
  # STEP 1.3: Calculate birthday constraints
  # --------------------------------------------------------------------------

  if (verbose) cat("  Step 1.3: Applying birthday constraints...\n")

  dt[, birthday := make_birthday(V20081, V2008, Ano)]
  dt[, first_sat_after_birthday := first_saturday_on_or_after(birthday)]

  # Determine if interview was before or after birthday
  dt[, visit_before_birthday := NA_integer_]
  dt[!is.na(V20082), visit_before_birthday := as.integer((Ano - V20082) - V2009)]

  # --------------------------------------------------------------------------
  # STEP 1.4: Calculate date bounds using STANDARD rules
  # --------------------------------------------------------------------------

  dt[, `:=`(
    date_min = make_date(Ano, month1, first_sat_m1),
    date_max = make_date(Ano, month3, first_sat_m3) + 21L
  )]

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

  # Calculate alternative bounds (for exception handling)
  dt[, `:=`(
    alt_date_min = make_date(Ano, month1, alt_sat_m1),
    alt_date_max = make_date(Ano, month3, alt_sat_m3) + 21L
  )]

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

  # --------------------------------------------------------------------------
  # STEP 1.5: Convert date bounds to MONTH positions
  # --------------------------------------------------------------------------

  if (verbose) cat("  Step 1.5: Converting to month positions...\n")

  dt[, `:=`(
    month_min_pos = calculate_month_position_min(date_min, Ano, Trimestre, day_threshold = 3L),
    month_max_pos = calculate_month_position_max(date_max, Ano, Trimestre, day_threshold = 3L),
    alt_month_min_pos = calculate_month_position_min(alt_date_min, Ano, Trimestre, day_threshold = 2L),
    alt_month_max_pos = calculate_month_position_max(alt_date_max, Ano, Trimestre, day_threshold = 2L)
  )]

  # --------------------------------------------------------------------------
  # STEP 1.6: MONTH aggregation at UPA-V1014 level ACROSS ALL quarters
  # --------------------------------------------------------------------------

  if (verbose) cat("  Step 1.6: Aggregating at UPA-panel level (cross-quarter)...\n")

  # OPTIMIZATION: Set key for faster groupby operations
  data.table::setkey(dt, UPA, V1014)

  dt[, `:=`(
    upa_month_min = max(month_min_pos, na.rm = TRUE),
    upa_month_max = min(month_max_pos, na.rm = TRUE),
    alt_upa_month_min = max(alt_month_min_pos, na.rm = TRUE),
    alt_upa_month_max = min(alt_month_max_pos, na.rm = TRUE)
  ), by = .(UPA, V1014)]

  # OPTIMIZATION: Use helper function for infinite value handling
  fix_infinite_values(dt, c("upa_month_min", "upa_month_max", "alt_upa_month_min", "alt_upa_month_max"))

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
    trim_exc_m1 = as.integer(sum(requires_exc_m1, na.rm = TRUE) > 0L),
    trim_exc_m2 = as.integer(sum(requires_exc_m2, na.rm = TRUE) > 0L),
    trim_exc_m3 = as.integer(sum(requires_exc_m3, na.rm = TRUE) > 0L)
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
      date_min = make_date(Ano, month1, first_sat_m1),
      date_max = make_date(Ano, month3, first_sat_m3) + 21L
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
      month_min_pos = calculate_month_position_min_dynamic(
        date_min, Ano, Trimestre, trim_exc_m1, trim_exc_m2, trim_exc_m3
      ),
      month_max_pos = calculate_month_position_max_dynamic(
        date_max, Ano, Trimestre, trim_exc_m1, trim_exc_m2, trim_exc_m3
      )
    )]

    # Re-aggregate ALL UPA-V1014 combinations
    dt[, `:=`(
      upa_month_min_final = max(month_min_pos, na.rm = TRUE),
      upa_month_max_final = min(month_max_pos, na.rm = TRUE)
    ), by = .(UPA, V1014)]
  } else {
    # No exceptions - reuse aggregation
    dt[, `:=`(
      upa_month_min_final = upa_month_min,
      upa_month_max_final = upa_month_max
    )]
  }

  # OPTIMIZATION: Use helper function for infinite value handling
  fix_infinite_values(dt, c("upa_month_min_final", "upa_month_max_final"))

  # --------------------------------------------------------------------------
  # STEP 1.8: MONTH DETERMINATION
  # --------------------------------------------------------------------------

  if (verbose) cat("  Step 1.8: Determining reference months...\n")

  dt[, ref_month_in_quarter := NA_integer_]
  dt[upa_month_min_final == upa_month_max_final &
       upa_month_min_final >= 1L & upa_month_max_final <= 3L,
     ref_month_in_quarter := upa_month_min_final]

  # OPTIMIZATION: Calculate stats more efficiently using helper function
  month_stats <- compute_determination_stats(dt, c("Ano", "Trimestre", "UPA", "V1008", "V1014"), "ref_month_in_quarter")
  n_total <- month_stats$n_total
  n_month_determined <- month_stats$n_determined
  month_rate <- n_month_determined / n_total

  if (verbose) {
    cat(sprintf("  -> Month determination: %.1f%% (%s of %s households)\n",
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

  # OPTIMIZATION: Use .N instead of which() + length() to count determined observations
  n_month_det <- dt[!is.na(ref_month_in_quarter), .N]

  if (n_month_det == 0) {
    if (verbose) cat("  -> No months determined - skipping fortnight identification\n")
  } else {
    if (verbose) cat(sprintf("  Processing %s observations with determined months...\n",
                             format(n_month_det, big.mark = ",")))

    # --------------------------------------------------------------------------
    # STEP 2.1: Calculate fortnight bounds WITHIN the determined month
    # --------------------------------------------------------------------------

    # The fortnight position is determined by:
    # 1. Which month (ref_month_in_quarter: 1, 2, or 3)
    # 2. Whether the date falls in the 1st half (days 1-15) or 2nd half (days 16+)

    # For month-determined rows, constrain the search to 2 fortnights
    dt[!is.na(ref_month_in_quarter), `:=`(
      fortnight_lower = (ref_month_in_quarter - 1L) * 2L + 1L,  # First fortnight of month
      fortnight_upper = ref_month_in_quarter * 2L               # Second fortnight of month
    )]

    # OPTIMIZATION: Pre-compute IBGE month boundaries for all unique (year, month) combinations
    # This avoids redundant computation in ibge_fortnight_in_quarter (169,000x reduction)
    all_dates <- c(
      dt[!is.na(ref_month_in_quarter), date_min],
      dt[!is.na(ref_month_in_quarter), date_max],
      dt[!is.na(ref_month_in_quarter), alt_date_min],
      dt[!is.na(ref_month_in_quarter), alt_date_max]
    )
    all_dates <- all_dates[!is.na(all_dates)]
    sat_dates <- ibge_week_saturday(all_dates)
    all_years <- fast_year(sat_dates)
    all_months <- fast_month(sat_dates)
    month_boundaries <- precompute_ibge_month_boundaries(all_years, all_months, min_days = 4L)
    rm(all_dates, sat_dates, all_years, all_months)

    # Calculate actual fortnight positions from date bounds (using optimized function)
    dt[!is.na(ref_month_in_quarter), `:=`(
      fortnight_min_pos = ibge_fortnight_in_quarter_fast(date_min, Trimestre, Ano, month_boundaries),
      fortnight_max_pos = ibge_fortnight_in_quarter_fast(date_max, Trimestre, Ano, month_boundaries)
    )]

    # Constrain to the determined month's fortnights
    dt[!is.na(ref_month_in_quarter), `:=`(
      fortnight_min_pos = pmax(fortnight_min_pos, fortnight_lower, na.rm = TRUE),
      fortnight_max_pos = pmin(fortnight_max_pos, fortnight_upper, na.rm = TRUE)
    )]

    # Also calculate alternative positions for exception handling (using optimized function)
    dt[!is.na(ref_month_in_quarter), `:=`(
      alt_fortnight_min_pos = ibge_fortnight_in_quarter_fast(alt_date_min, Trimestre, Ano, month_boundaries),
      alt_fortnight_max_pos = ibge_fortnight_in_quarter_fast(alt_date_max, Trimestre, Ano, month_boundaries)
    )]

    # OPTIMIZATION: Free memory - month_boundaries no longer needed
    rm(month_boundaries)

    dt[!is.na(ref_month_in_quarter), `:=`(
      alt_fortnight_min_pos = pmax(alt_fortnight_min_pos, fortnight_lower, na.rm = TRUE),
      alt_fortnight_max_pos = pmin(alt_fortnight_max_pos, fortnight_upper, na.rm = TRUE)
    )]

    # --------------------------------------------------------------------------
    # STEP 2.2: FORTNIGHT aggregation at household-quarter level
    # --------------------------------------------------------------------------

    if (verbose) cat("  Step 2.2: Aggregating at household level (within quarter)...\n")

    # Aggregate ONLY for month-determined rows
    dt[!is.na(ref_month_in_quarter), `:=`(
      hh_fortnight_min = max(fortnight_min_pos, na.rm = TRUE),
      hh_fortnight_max = min(fortnight_max_pos, na.rm = TRUE),
      alt_hh_fortnight_min = max(alt_fortnight_min_pos, na.rm = TRUE),
      alt_hh_fortnight_max = min(alt_fortnight_max_pos, na.rm = TRUE)
    ), by = .(Ano, Trimestre, UPA, V1008)]

    # OPTIMIZATION: Use helper function for infinite value handling
    fix_infinite_values(dt, c("hh_fortnight_min", "hh_fortnight_max",
                               "alt_hh_fortnight_min", "alt_hh_fortnight_max"))

    # --------------------------------------------------------------------------
    # STEP 2.3: Exception handling for fortnights
    # --------------------------------------------------------------------------

    dt[!is.na(ref_month_in_quarter), requires_exc_fortnight := (
      hh_fortnight_min > hh_fortnight_max &
      alt_hh_fortnight_min <= alt_hh_fortnight_max
    )]

    dt[!is.na(ref_month_in_quarter),
       trim_exc_fortnight := as.integer(sum(requires_exc_fortnight, na.rm = TRUE) > 0L),
       by = .(Ano, Trimestre)]

    dt[!is.na(ref_month_in_quarter) & trim_exc_fortnight == 1L, `:=`(
      hh_fortnight_min = alt_hh_fortnight_min,
      hh_fortnight_max = alt_hh_fortnight_max
    )]

    # --------------------------------------------------------------------------
    # STEP 2.4: FORTNIGHT DETERMINATION
    # --------------------------------------------------------------------------

    if (verbose) cat("  Step 2.4: Determining reference fortnights...\n")

    dt[!is.na(ref_month_in_quarter) &
         hh_fortnight_min == hh_fortnight_max &
         hh_fortnight_min >= 1L & hh_fortnight_max <= 6L,
       ref_fortnight_in_quarter := hh_fortnight_min]

    # OPTIMIZATION: Calculate stats more efficiently using helper function
    fortnight_stats <- compute_determination_stats(dt, c("Ano", "Trimestre", "UPA", "V1008", "V1014"), "ref_fortnight_in_quarter")
    n_fortnight_determined <- fortnight_stats$n_determined
    fortnight_rate <- n_fortnight_determined / n_total

    if (verbose) {
      cat(sprintf("  -> Fortnight determination: %.1f%% (%s of %s households)\n",
                  fortnight_rate * 100,
                  format(n_fortnight_determined, big.mark = ","),
                  format(n_total, big.mark = ",")))
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

  # Initialize week columns
  dt[, ref_week_yyyyww := NA_integer_]

  # OPTIMIZATION: Use .N instead of which() + length() to count determined observations
  n_fortnight_det <- dt[!is.na(ref_fortnight_in_quarter), .N]

  if (n_fortnight_det == 0) {
    if (verbose) cat("  -> No fortnights determined - skipping week identification\n")
  } else {
    if (verbose) cat(sprintf("  Processing %s observations with determined fortnights...\n",
                             format(n_fortnight_det, big.mark = ",")))

    # --------------------------------------------------------------------------
    # STEP 3.1: Calculate week bounds WITHIN the determined fortnight
    # --------------------------------------------------------------------------

    # Calculate fortnight start/end dates
    dt[!is.na(ref_fortnight_in_quarter), `:=`(
      fortnight_month = ((ref_fortnight_in_quarter - 1L) %/% 2L) + 1L,  # 1, 1, 2, 2, 3, 3
      fortnight_half = ((ref_fortnight_in_quarter - 1L) %% 2L) + 1L     # 1, 2, 1, 2, 1, 2
    )]

    days_in_month_vec <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

    dt[!is.na(ref_fortnight_in_quarter), `:=`(
      fortnight_month_num = quarter_month_n(Trimestre, fortnight_month)
    )]

    dt[!is.na(ref_fortnight_in_quarter), `:=`(
      fortnight_start_day = fifelse(fortnight_half == 1L, 1L, 16L),
      fortnight_end_day = fifelse(fortnight_half == 1L, 15L,
                                  fifelse(fortnight_month_num == 2L & is_leap_year(Ano),
                                          29L, days_in_month_vec[fortnight_month_num]))
    )]

    # Convert fortnight bounds to ISO weeks
    dt[!is.na(ref_fortnight_in_quarter), `:=`(
      fortnight_start_yyyyww = date_to_yyyyww(make_date(Ano, fortnight_month_num, fortnight_start_day)),
      fortnight_end_yyyyww = date_to_yyyyww(make_date(Ano, fortnight_month_num, fortnight_end_day))
    )]

    # Calculate actual week positions from date bounds
    dt[!is.na(ref_fortnight_in_quarter), `:=`(
      week_min_yyyyww = date_to_yyyyww(date_min),
      week_max_yyyyww = date_to_yyyyww(date_max)
    )]

    # Constrain to the determined fortnight's weeks
    dt[!is.na(ref_fortnight_in_quarter), `:=`(
      week_min_yyyyww = pmax(week_min_yyyyww, fortnight_start_yyyyww, na.rm = TRUE),
      week_max_yyyyww = pmin(week_max_yyyyww, fortnight_end_yyyyww, na.rm = TRUE)
    )]

    # Also calculate alternative positions for exception handling
    dt[!is.na(ref_fortnight_in_quarter), `:=`(
      alt_week_min_yyyyww = date_to_yyyyww(alt_date_min),
      alt_week_max_yyyyww = date_to_yyyyww(alt_date_max)
    )]

    dt[!is.na(ref_fortnight_in_quarter), `:=`(
      alt_week_min_yyyyww = pmax(alt_week_min_yyyyww, fortnight_start_yyyyww, na.rm = TRUE),
      alt_week_max_yyyyww = pmin(alt_week_max_yyyyww, fortnight_end_yyyyww, na.rm = TRUE)
    )]

    # --------------------------------------------------------------------------
    # STEP 3.2: WEEK aggregation at household-quarter level
    # --------------------------------------------------------------------------

    if (verbose) cat("  Step 3.2: Aggregating at household level (within quarter)...\n")

    # Convert to sequential for proper comparison (handles year boundaries)
    dt[!is.na(ref_fortnight_in_quarter), `:=`(
      week_min_seq = yyyyww_to_seq(week_min_yyyyww),
      week_max_seq = yyyyww_to_seq(week_max_yyyyww),
      alt_week_min_seq = yyyyww_to_seq(alt_week_min_yyyyww),
      alt_week_max_seq = yyyyww_to_seq(alt_week_max_yyyyww)
    )]

    dt[!is.na(ref_fortnight_in_quarter), `:=`(
      hh_week_min_seq = max(week_min_seq, na.rm = TRUE),
      hh_week_max_seq = min(week_max_seq, na.rm = TRUE),
      alt_hh_week_min_seq = max(alt_week_min_seq, na.rm = TRUE),
      alt_hh_week_max_seq = min(alt_week_max_seq, na.rm = TRUE)
    ), by = .(Ano, Trimestre, UPA, V1008)]

    # Also store the YYYYWW format for final output
    dt[!is.na(ref_fortnight_in_quarter), `:=`(
      hh_week_min = max(week_min_yyyyww, na.rm = TRUE),
      hh_week_max = min(week_max_yyyyww, na.rm = TRUE)
    ), by = .(Ano, Trimestre, UPA, V1008)]

    # OPTIMIZATION: Use helper function for infinite value handling
    fix_infinite_values(dt, c("hh_week_min", "hh_week_max", "hh_week_min_seq", "hh_week_max_seq",
                               "alt_hh_week_min_seq", "alt_hh_week_max_seq"))

    # --------------------------------------------------------------------------
    # STEP 3.3: Exception handling for weeks (using sequential values)
    # --------------------------------------------------------------------------

    dt[!is.na(ref_fortnight_in_quarter), requires_exc_week := (
      hh_week_min_seq > hh_week_max_seq &
      alt_hh_week_min_seq <= alt_hh_week_max_seq
    )]

    dt[!is.na(ref_fortnight_in_quarter),
       trim_exc_week := as.integer(sum(requires_exc_week, na.rm = TRUE) > 0L),
       by = .(Ano, Trimestre)]

    dt[!is.na(ref_fortnight_in_quarter) & trim_exc_week == 1L, `:=`(
      hh_week_min_seq = alt_hh_week_min_seq,
      hh_week_max_seq = alt_hh_week_max_seq
    )]

    # --------------------------------------------------------------------------
    # STEP 3.4: WEEK DETERMINATION (using sequential values for comparison)
    # --------------------------------------------------------------------------

    if (verbose) cat("  Step 3.4: Determining reference weeks...\n")

    # Determine if min == max using sequential values (handles year boundaries)
    dt[!is.na(ref_fortnight_in_quarter) &
         hh_week_min_seq == hh_week_max_seq &
         !is.na(hh_week_min_seq) & !is.infinite(hh_week_min_seq),
       ref_week_yyyyww := hh_week_min]

    # OPTIMIZATION: Calculate stats more efficiently using helper function
    week_stats <- compute_determination_stats(dt, c("Ano", "Trimestre", "UPA", "V1008", "V1014"), "ref_week_yyyyww")
    n_week_determined <- week_stats$n_determined
    week_rate <- n_week_determined / n_total

    if (verbose) {
      cat(sprintf("  -> Week determination: %.1f%% (%s of %s households)\n",
                  week_rate * 100,
                  format(n_week_determined, big.mark = ","),
                  format(n_total, big.mark = ",")))
    }
  }

  # ============================================================================
  # PHASE 4: BUILD CROSSWALK
  # ============================================================================

  if (verbose) cat("\nPhase 4: Building crosswalk...\n")

  # Build crosswalk at household-quarter level
  # OPTIMIZATION: When store_date_bounds=TRUE, include date bounds for pnadc_experimental_periods()
  if (store_date_bounds) {
    # Aggregate date bounds at UPA-V1014 level (for month probabilistic)
    # and household level (for fortnight/week probabilistic)
    # Store as INTEGER days since epoch for fast arithmetic
    dt[, `:=`(
      upa_date_min_int = suppressWarnings(as.integer(max(date_min, na.rm = TRUE))),
      upa_date_max_int = suppressWarnings(as.integer(min(date_max, na.rm = TRUE)))
    ), by = .(Ano, Trimestre, UPA, V1014)]

    dt[, `:=`(
      hh_date_min_int = suppressWarnings(as.integer(max(date_min, na.rm = TRUE))),
      hh_date_max_int = suppressWarnings(as.integer(min(date_max, na.rm = TRUE)))
    ), by = .(Ano, Trimestre, UPA, V1008)]

    # Fix infinites from empty groups
    fix_infinite_values(dt, c("upa_date_min_int", "upa_date_max_int",
                               "hh_date_min_int", "hh_date_max_int"))

    # Include all bounds in crosswalk
    crosswalk <- dt[, .(
      ref_month_in_quarter = ref_month_in_quarter[1L],
      ref_fortnight_in_quarter = ref_fortnight_in_quarter[1L],
      ref_week_yyyyww = ref_week_yyyyww[1L],
      upa_date_min_int = upa_date_min_int[1L],
      upa_date_max_int = upa_date_max_int[1L],
      hh_date_min_int = hh_date_min_int[1L],
      hh_date_max_int = hh_date_max_int[1L],
      upa_month_min = upa_month_min_final[1L],
      upa_month_max = upa_month_max_final[1L]
    ), by = .(Ano, Trimestre, UPA, V1008, V1014)]

    # Store exception flags as attribute
    exc_cols <- c("trim_exc_m1", "trim_exc_m2", "trim_exc_m3")
    if (all(exc_cols %in% names(dt))) {
      exception_flags <- unique(dt[, .(Ano, Trimestre, trim_exc_m1, trim_exc_m2, trim_exc_m3)])
      attr(crosswalk, "exception_flags") <- exception_flags
    }

    if (verbose) cat("  - Date bounds stored for experimental strategies\n")
  } else {
    crosswalk <- dt[, .(
      ref_month_in_quarter = ref_month_in_quarter[1L],
      ref_fortnight_in_quarter = ref_fortnight_in_quarter[1L],
      ref_week_yyyyww = ref_week_yyyyww[1L]
    ), by = .(Ano, Trimestre, UPA, V1008, V1014)]
  }

  # OPTIMIZATION: Free memory from working data.table - no longer needed
  rm(dt)

  # Calculate derived month columns
  crosswalk[, `:=`(
    ref_month_start = as.Date(NA),
    ref_month_end = as.Date(NA),
    ref_month_yyyymm = NA_integer_,
    ref_month_weeks = NA_integer_
  )]
  crosswalk[!is.na(ref_month_in_quarter), `:=`(
    temp_month = quarter_month_n(Trimestre, ref_month_in_quarter)
  )]
  crosswalk[!is.na(temp_month), `:=`(
    ref_month_start = ibge_month_start(Ano, temp_month, min_days = 4L),
    ref_month_end = ibge_month_end(Ano, temp_month, min_days = 4L),
    ref_month_yyyymm = yyyymm(Ano, temp_month),
    ref_month_weeks = ibge_month_weeks(Ano, temp_month, min_days = 4L)
  )]
  crosswalk[, temp_month := NULL]

  # Calculate derived fortnight columns
  crosswalk[, ref_fortnight_yyyyff := NA_integer_]
  crosswalk[!is.na(ref_fortnight_in_quarter), `:=`(
    ref_fortnight_yyyyff = ibge_fortnight_in_quarter_to_yyyyff(Ano, Trimestre, ref_fortnight_in_quarter)
  )]

  crosswalk[, ref_fortnight_start := as.Date(NA)]
  crosswalk[, ref_fortnight_end := as.Date(NA)]
  crosswalk[!is.na(ref_fortnight_yyyyff), ref_fortnight_start := ibge_yyyyff_to_date(ref_fortnight_yyyyff)]
  crosswalk[!is.na(ref_fortnight_start), ref_fortnight_end := ref_fortnight_start + ibge_fortnight_weeks(
    fast_year(ref_fortnight_start),
    fast_month(ref_fortnight_start),
    fifelse(fast_mday(ref_fortnight_start) <= 14L, 1L, 2L),
    min_days = 4L
  ) * 7L - 1L]

  # Calculate derived week columns
  crosswalk[, ref_week_start := as.Date(NA)]
  crosswalk[, ref_week_end := as.Date(NA)]
  crosswalk[!is.na(ref_week_yyyyww), ref_week_start := ibge_yyyyww_to_date(ref_week_yyyyww)]
  crosswalk[!is.na(ref_week_start), ref_week_end := ref_week_start + 6L]

  crosswalk[, ref_week_in_quarter := NA_integer_]
  crosswalk[!is.na(ref_week_start), ref_week_in_quarter := ibge_week_in_quarter(ref_week_start, Trimestre, Ano, min_days = 4L)]

  # Add determination flags
  crosswalk[, `:=`(
    determined_month = !is.na(ref_month_in_quarter),
    determined_fortnight = !is.na(ref_fortnight_in_quarter),
    determined_week = !is.na(ref_week_in_quarter)
  )]

  # Reorder columns for clarity
  setcolorder(crosswalk, c(
    "Ano", "Trimestre", "UPA", "V1008", "V1014",
    "ref_month_start", "ref_month_end", "ref_month_in_quarter", "ref_month_yyyymm", "ref_month_weeks", "determined_month",
    "ref_fortnight_start", "ref_fortnight_end", "ref_fortnight_in_quarter", "ref_fortnight_yyyyff", "determined_fortnight",
    "ref_week_start", "ref_week_end", "ref_week_in_quarter", "ref_week_yyyyww", "determined_week"
  ))

  # ============================================================================
  # SUMMARY
  # ============================================================================

  month_rate <- mean(crosswalk$determined_month)
  fortnight_rate <- mean(crosswalk$determined_fortnight)
  week_rate <- mean(crosswalk$determined_week)

  if (verbose) {
    cat("\n========================================\n")
    cat("Crosswalk complete (nested identification):\n")
    cat(sprintf("  - %s unique household-quarter observations\n",
                format(nrow(crosswalk), big.mark = ",")))
    cat(sprintf("  - Month determination:    %6.2f%%\n", month_rate * 100))
    cat(sprintf("  - Fortnight determination: %6.2f%% (nested within months)\n", fortnight_rate * 100))
    cat(sprintf("  - Week determination:      %6.2f%% (nested within fortnights)\n", week_rate * 100))
    cat("========================================\n")
  }

  # Store determination rates as attributes
  attr(crosswalk, "determination_rates") <- list(
    month = month_rate,
    fortnight = fortnight_rate,
    week = week_rate
  )

  crosswalk
}
