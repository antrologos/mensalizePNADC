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
#'   \item UPA-panel level aggregation for months (across quarters - panel design)
#'   \item Household-level aggregation for fortnights and weeks (within each quarter only)
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
#'     \item{Ano, Trimestre, UPA, V1008, V1014}{Join keys (year, quarter, UPA, household, panel)}
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
#' @note
#' ## Aggregation Levels
#'
#' The crosswalk aggregates at different levels:
#' \itemize{
#'   \item \strong{Months}: Can aggregate across quarters at UPA-V1014 level
#'     (PNADC panel design ensures same month position)
#'   \item \strong{Fortnights}: Household level within quarter only
#'     (no consistent position across quarterly visits)
#'   \item \strong{Weeks}: Household level within quarter only
#'     (no consistent position across quarterly visits)
#' }
#'
#' @details
#' ## Temporal Granularity
#'
#' The crosswalk contains three levels of temporal granularity:
#' \itemize{
#'   \item \strong{Month}: 3 per quarter, ~97% determination rate (aggregates across quarters)
#'   \item \strong{Fortnight (quinzena)}: 6 per quarter, ~2-5% determination rate (within-quarter only)
#'   \item \strong{Week}: 13-14 per quarter, ~1-2% determination rate (within-quarter only)
#' }
#'
#' \strong{Why are fortnight/week rates so low?} Only months can aggregate at
#' UPA-V1014 level across ALL quarters (leveraging the panel design where the
#' same units are interviewed in consistent relative positions). Fortnights
#' and weeks are determined only from birthday constraints within a single
#' quarter, which rarely narrows the interview window to a single 15-day or
#' 7-day period.
#'
#' ## Cross-Quarter Aggregation (Important!)
#'
#' **For optimal month determination rates, input data should be stacked across
#' multiple quarters** (ideally 4+ years). The algorithm leverages PNADC's
#' rotating panel design where the same UPA-V1014 is interviewed in the same
#' relative position across quarterly visits. This cross-quarter aggregation
#' only applies to months - fortnight and week determination is based on
#' within-quarter constraints only.
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
  # SHARED PREPROCESSING (Steps 1-3)
  # These steps are identical for month, fortnight, and week identification
  # ============================================================================

  if (verbose) cat("  Preprocessing data (shared computation)...\n")

  # Batch convert character columns using set() for efficiency
  int_cols <- c("Ano", "Trimestre", "V2008", "V20081", "V20082")
  for (col in int_cols) {
    if (is.character(dt[[col]])) {
      data.table::set(dt, j = col, value = as.integer(dt[[col]]))
    }
  }
  if (!is.numeric(dt$V2009)) {
    data.table::set(dt, j = "V2009", value = as.numeric(dt$V2009))
  }

  # Handle special codes for unknown values
  dt[V2008 == 99L, V2008 := NA_integer_]
  dt[V20081 == 99L, V20081 := NA_integer_]
  dt[V20082 == 9999L, V20082 := NA_integer_]

  # --------------------------------------------------------------------------
  # STEP 1: Pre-compute first valid Saturdays for each unique (year, quarter)
  # --------------------------------------------------------------------------

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

  # --------------------------------------------------------------------------
  # STEP 2: Calculate birthday and first Saturday after birthday
  # --------------------------------------------------------------------------

  dt[, birthday := make_birthday(V20081, V2008, Ano)]
  dt[, first_sat_after_birthday := first_saturday_on_or_after(birthday)]

  # Determine if interview was before or after birthday
  dt[, visit_before_birthday := NA_integer_]
  dt[!is.na(V20082), visit_before_birthday := as.integer((Ano - V20082) - V2009)]

  # --------------------------------------------------------------------------
  # STEP 3: Calculate date bounds using STANDARD rules
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

  # Calculate alternative bounds
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

  # ============================================================================
  # STEP 4: Convert date bounds to position types (PHASED for memory efficiency)
  # ============================================================================

  if (verbose) cat("  Converting date bounds to period positions...\n")

  # --------------------------------------------------------------------------
  # PHASE 4a: MONTH positions (needed first for month aggregation)
  # --------------------------------------------------------------------------
  dt[, `:=`(
    month_min_pos = calculate_month_position_min(date_min, Ano, Trimestre, day_threshold = 3L),
    month_max_pos = calculate_month_position_max(date_max, Ano, Trimestre, day_threshold = 3L),
    alt_month_min_pos = calculate_month_position_min(alt_date_min, Ano, Trimestre, day_threshold = 2L),
    alt_month_max_pos = calculate_month_position_max(alt_date_max, Ano, Trimestre, day_threshold = 2L)
  )]

  # ============================================================================
  # STEP 5: Aggregate and determine reference periods
  # ============================================================================

  if (verbose) cat("  Aggregating constraints and determining periods...\n")

  # --------------------------------------------------------------------------
  # 5a: MONTH - Aggregate at UPA-V1014 level ACROSS ALL quarters
  # OPTIMIZATION: Removed setkey() - using keyless by= is equally efficient
  # for one-time aggregation and avoids O(n log n) sorting overhead
 # --------------------------------------------------------------------------

  dt[, `:=`(
    upa_month_min = max(month_min_pos, na.rm = TRUE),
    upa_month_max = min(month_max_pos, na.rm = TRUE),
    alt_upa_month_min = max(alt_month_min_pos, na.rm = TRUE),
    alt_upa_month_max = min(alt_month_max_pos, na.rm = TRUE)
  ), by = .(UPA, V1014)]

  # OPTIMIZATION: Handle infinite values with vectorized set() instead of 4 separate subset operations
  for (col in c("upa_month_min", "upa_month_max", "alt_upa_month_min", "alt_upa_month_max")) {
    inf_idx <- which(is.infinite(dt[[col]]))
    if (length(inf_idx) > 0L) {
      data.table::set(dt, i = inf_idx, j = col, value = NA_integer_)
    }
  }

  # --------------------------------------------------------------------------
  # PHASE 4b: FORTNIGHT and WEEK positions (computed now, after month aggregation)
  # This phased approach reduces peak memory by ~400MB for 10M row datasets
  # --------------------------------------------------------------------------

  # Fortnight positions (1-6 per quarter)
  dt[, `:=`(
    fortnight_min_pos = date_to_fortnight_in_quarter(date_min, Trimestre),
    fortnight_max_pos = date_to_fortnight_in_quarter(date_max, Trimestre),
    alt_fortnight_min_pos = date_to_fortnight_in_quarter(alt_date_min, Trimestre),
    alt_fortnight_max_pos = date_to_fortnight_in_quarter(alt_date_max, Trimestre)
  )]

  # Week positions (ISO YYYYWW)
  dt[, `:=`(
    week_min_yyyyww = date_to_yyyyww(date_min),
    week_max_yyyyww = date_to_yyyyww(date_max),
    alt_week_min_yyyyww = date_to_yyyyww(alt_date_min),
    alt_week_max_yyyyww = date_to_yyyyww(alt_date_max)
  )]

  # --------------------------------------------------------------------------
  # 5b+5c: FORTNIGHT and WEEK - Combined aggregation (SAME grouping key)
  # OPTIMIZATION: Single groupby pass instead of two separate passes
  # Both use household-quarter level: (Ano, Trimestre, UPA, V1008)
  # --------------------------------------------------------------------------

  dt[, `:=`(
    # Fortnight aggregation
    hh_fortnight_min = max(fortnight_min_pos, na.rm = TRUE),
    hh_fortnight_max = min(fortnight_max_pos, na.rm = TRUE),
    alt_hh_fortnight_min = max(alt_fortnight_min_pos, na.rm = TRUE),
    alt_hh_fortnight_max = min(alt_fortnight_max_pos, na.rm = TRUE),
    # Week aggregation (same grouping key - combined for efficiency)
    hh_week_min = max(week_min_yyyyww, na.rm = TRUE),
    hh_week_max = min(week_max_yyyyww, na.rm = TRUE),
    alt_hh_week_min = max(alt_week_min_yyyyww, na.rm = TRUE),
    alt_hh_week_max = min(alt_week_max_yyyyww, na.rm = TRUE)
  ), by = .(Ano, Trimestre, UPA, V1008)]

  # OPTIMIZATION: Handle infinite values with vectorized set() for all 8 columns
  hh_cols <- c("hh_fortnight_min", "hh_fortnight_max", "alt_hh_fortnight_min", "alt_hh_fortnight_max",
               "hh_week_min", "hh_week_max", "alt_hh_week_min", "alt_hh_week_max")
  for (col in hh_cols) {
    inf_idx <- which(is.infinite(dt[[col]]))
    if (length(inf_idx) > 0L) {
      data.table::set(dt, i = inf_idx, j = col, value = NA_integer_)
    }
  }

  # ============================================================================
  # STEP 6: Dynamic exception detection for MONTHS
  # (Months have the most complex exception logic)
  # ============================================================================

  if (verbose) cat("  Applying exception rules...\n")

  # An observation requires exception if:
  # 1. Standard rules produce impossible result (upa_month_min > upa_month_max)
  # 2. Alternative rules would produce valid result (alt_upa_month_min <= alt_upa_month_max)
  # 3. The individual's constraint is the binding one that could be relaxed
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
    # Update first Saturday values where exceptions are needed
    dt[trim_exc_m1 == 1L, first_sat_m1 := alt_sat_m1]
    dt[trim_exc_m2 == 1L, first_sat_m2 := alt_sat_m2]
    dt[trim_exc_m3 == 1L, first_sat_m3 := alt_sat_m3]

    # Recalculate date bounds ONLY for exception rows
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
  }

  # --------------------------------------------------------------------------
  # Final month aggregation to UPA-panel level ACROSS ALL QUARTERS
  # CRITICAL: Must re-aggregate ALL UPA-V1014 when there are exceptions,
  # because month_min_pos may have been updated for exception rows
  # --------------------------------------------------------------------------
  if (has_any_exception) {
    # Re-aggregate ALL UPA-V1014 combinations (not just affected ones)
    # This ensures updated positions from exception rows are combined with
    # positions from other quarters for the same UPA-V1014
    dt[, `:=`(
      upa_month_min_final = max(month_min_pos, na.rm = TRUE),
      upa_month_max_final = min(month_max_pos, na.rm = TRUE)
    ), by = .(UPA, V1014)]
  } else {
    # No exceptions - reuse aggregation from Step 5a
    dt[, `:=`(
      upa_month_min_final = upa_month_min,
      upa_month_max_final = upa_month_max
    )]
  }

  # --------------------------------------------------------------------------
  # 6b: Exception detection for FORTNIGHT and WEEK (simpler logic)
  # --------------------------------------------------------------------------

  # Fortnight exceptions
  dt[, requires_exc_fortnight := (
    hh_fortnight_min > hh_fortnight_max &
    alt_hh_fortnight_min <= alt_hh_fortnight_max
  )]

  dt[, trim_exc_fortnight := as.integer(sum(requires_exc_fortnight, na.rm = TRUE) > 0L),
     by = .(Ano, Trimestre)]

  dt[trim_exc_fortnight == 1L, `:=`(
    hh_fortnight_min = alt_hh_fortnight_min,
    hh_fortnight_max = alt_hh_fortnight_max
  )]

  # Week exceptions
  dt[, requires_exc_week := (
    hh_week_min > hh_week_max &
    alt_hh_week_min <= alt_hh_week_max
  )]

  dt[, trim_exc_week := as.integer(sum(requires_exc_week, na.rm = TRUE) > 0L),
     by = .(Ano, Trimestre)]

  dt[trim_exc_week == 1L, `:=`(
    hh_week_min = alt_hh_week_min,
    hh_week_max = alt_hh_week_max
  )]

  # --------------------------------------------------------------------------
  # MEMORY CLEANUP: Remove intermediate columns no longer needed
  # This reduces memory footprint by ~500MB for 10M row datasets
  # --------------------------------------------------------------------------
  cleanup_cols <- c(
    # Position calculation intermediates (no longer needed after aggregation)
    "month_min_pos", "month_max_pos", "alt_month_min_pos", "alt_month_max_pos",
    "fortnight_min_pos", "fortnight_max_pos", "alt_fortnight_min_pos", "alt_fortnight_max_pos",
    "week_min_yyyyww", "week_max_yyyyww", "alt_week_min_yyyyww", "alt_week_max_yyyyww",
    # Aggregated values (intermediate, final values are used for determination)
    "upa_month_min", "upa_month_max",
    "alt_upa_month_min", "alt_upa_month_max",
    "alt_hh_fortnight_min", "alt_hh_fortnight_max",
    "alt_hh_week_min", "alt_hh_week_max",
    # Exception detection flags
    "requires_exception", "requires_exc_m1", "requires_exc_m2", "requires_exc_m3",
    "trim_exc_m1", "trim_exc_m2", "trim_exc_m3",
    "requires_exc_fortnight", "trim_exc_fortnight",
    "requires_exc_week", "trim_exc_week",
    # Date calculation intermediates
    "date_min", "date_max", "alt_date_min", "alt_date_max",
    "first_sat_m1", "first_sat_m2", "first_sat_m3",
    "alt_sat_m1", "alt_sat_m2", "alt_sat_m3",
    "first_sat_after_birthday", "visit_before_birthday",
    "month1", "month2", "month3"
  )
  # Only remove columns that exist (some may not exist if no exceptions)
  existing_cleanup <- intersect(cleanup_cols, names(dt))
  if (length(existing_cleanup) > 0L) {
    dt[, (existing_cleanup) := NULL]
  }

  # ============================================================================
  # STEP 7: Final determination
  # ============================================================================

  if (verbose) cat("  Assigning reference periods...\n")

  # Month: determined if min_final == max_final
  dt[, ref_month_in_quarter := NA_integer_]
  dt[upa_month_min_final == upa_month_max_final &
       upa_month_min_final >= 1L & upa_month_max_final <= 3L,
     ref_month_in_quarter := upa_month_min_final]

  # Fortnight: determined if min == max
  dt[, ref_fortnight_in_quarter := NA_integer_]
  dt[hh_fortnight_min == hh_fortnight_max &
       hh_fortnight_min >= 1L & hh_fortnight_max <= 6L,
     ref_fortnight_in_quarter := hh_fortnight_min]

  # Week: determined if min == max
  dt[, ref_week_yyyyww := NA_integer_]
  dt[hh_week_min == hh_week_max & !is.infinite(hh_week_min),
     ref_week_yyyyww := hh_week_min]

  # ============================================================================
  # STEP 8: Build crosswalk at household-quarter level
  # ============================================================================

  if (verbose) cat("  Building crosswalk...\n")

  # OPTIMIZATION: Use first-per-group pattern instead of unique()
  # This avoids O(n log n) hash-based deduplication and is more memory efficient
  # Values are constant within each household-quarter, so [1] gives correct result
  crosswalk <- dt[, .(
    ref_month_in_quarter = ref_month_in_quarter[1L],
    ref_fortnight_in_quarter = ref_fortnight_in_quarter[1L],
    ref_week_yyyyww = ref_week_yyyyww[1L]
  ), by = .(Ano, Trimestre, UPA, V1008, V1014)]

  # Calculate derived month columns using crosswalk's Ano/Trimestre context
  crosswalk[!is.na(ref_month_in_quarter), `:=`(
    ref_month = make_date(Ano, quarter_month_n(Trimestre, ref_month_in_quarter), 1L),
    ref_month_yyyymm = yyyymm(Ano, quarter_month_n(Trimestre, ref_month_in_quarter))
  )]
  crosswalk[is.na(ref_month_in_quarter), `:=`(
    ref_month = as.Date(NA),
    ref_month_yyyymm = NA_integer_
  )]

  # Calculate derived fortnight columns
  crosswalk[, ref_fortnight_yyyyff := NA_integer_]
  crosswalk[!is.na(ref_fortnight_in_quarter), `:=`(
    ref_fortnight_yyyyff = fortnight_in_quarter_to_yyyyff(Ano, Trimestre, ref_fortnight_in_quarter)
  )]

  crosswalk[, ref_fortnight := as.Date(NA)]
  crosswalk[!is.na(ref_fortnight_yyyyff), ref_fortnight := yyyyff_to_date(ref_fortnight_yyyyff)]

  # Calculate derived week columns
  crosswalk[, ref_week := as.Date(NA)]
  crosswalk[!is.na(ref_week_yyyyww), ref_week := yyyyww_to_date(ref_week_yyyyww)]

  crosswalk[, ref_week_in_quarter := NA_integer_]
  crosswalk[!is.na(ref_week), ref_week_in_quarter := week_in_quarter(ref_week, Trimestre, Ano)]

  # Add determination flags
  crosswalk[, `:=`(
    determined_month = !is.na(ref_month_in_quarter),
    determined_fortnight = !is.na(ref_fortnight_in_quarter),
    determined_week = !is.na(ref_week_in_quarter)
  )]

  # Reorder columns for clarity
  setcolorder(crosswalk, c(
    "Ano", "Trimestre", "UPA", "V1008", "V1014",
    "ref_month", "ref_month_in_quarter", "ref_month_yyyymm", "determined_month",
    "ref_fortnight", "ref_fortnight_in_quarter", "ref_fortnight_yyyyff", "determined_fortnight",
    "ref_week", "ref_week_in_quarter", "ref_week_yyyyww", "determined_week"
  ))

  # ============================================================================
  # SUMMARY
  # ============================================================================

  month_rate <- mean(crosswalk$determined_month)
  fortnight_rate <- mean(crosswalk$determined_fortnight)
  week_rate <- mean(crosswalk$determined_week)

  if (verbose) {
    cat("\nCrosswalk complete:\n")
    cat(sprintf("  - %s unique household-quarter observations\n", format(nrow(crosswalk), big.mark = ",")))
    cat(sprintf("  - Month determination: %.1f%%\n", month_rate * 100))
    cat(sprintf("  - Fortnight determination: %.1f%%\n", fortnight_rate * 100))
    cat(sprintf("  - Week determination: %.1f%%\n", week_rate * 100))
  }

  # Store determination rates as attributes
  attr(crosswalk, "determination_rates") <- list(
    month = month_rate,
    fortnight = fortnight_rate,
    week = week_rate
  )

  crosswalk
}
