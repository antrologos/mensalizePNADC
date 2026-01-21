#' Identify Reference Month in PNADC Data
#'
#' Determines which month within each quarter corresponds to each survey
#' observation based on IBGE's "Parada Tecnica" (technical break) rules.
#'
#' @description
#' PNADC is a quarterly survey, but each interview actually refers to a specific
#' month within the quarter. This function identifies which month that observation belongs
#' to, enabling a shorter term time series analysis.
#'
#' The algorithm uses:
#' \itemize{
#'   \item IBGE's reference week timing rules (first Saturday with sufficient days in month)
#'   \item Respondent birthdates to constrain possible interview dates
#'   \item UPA-panel level aggregation (everyone in same sampling unit interviewed together)
#'   \item Dynamic exception detection (identifies quarters needing relaxed rules from the data)
#' }
#'
#' @param data A data.frame or data.table with PNADC microdata. Required columns:
#'   \itemize{
#'     \item \code{Ano}: Survey year
#'     \item \code{Trimestre}: Quarter (1-4)
#'     \item \code{UPA}: Primary Sampling Unit
#'     \item \code{V1014}: Panel identifier
#'     \item \code{V2008}: Birth day (1-31)
#'     \item \code{V20081}: Birth month (1-12)
#'     \item \code{V20082}: Birth year
#'     \item \code{V2009}: Age
#'   }
#'   Optional but recommended for complete crosswalk:
#'   \itemize{
#'     \item \code{V1008}: Household sequence within UPA
#'     \item \code{V2003}: Person sequence within household
#'   }
#' @param verbose Logical. If TRUE (default), display progress bar and step information.
#' @param .pb Optional progress bar object created by \code{txtProgressBar}. If provided,
#'   the function updates this progress bar instead of creating its own. Used internally
#'   by \code{pnadc_identify_periods()} to show unified progress across all steps.
#' @param .pb_offset Integer. Offset to add to progress bar updates when using an external
#'   progress bar. Default is 0.
#'
#' @return A data.table with the original key columns plus:
#'   \itemize{
#'     \item \code{ref_month}: Reference month as Date (first day of month, e.g., "2023-01-01")
#'     \item \code{ref_month_in_quarter}: Position in quarter (1, 2, 3) or NA if indeterminate
#'     \item \code{ref_month_yyyymm}: Integer YYYYMM format (e.g., 202301)
#'   }
#'
#' @details
#' ## Exception Detection
#'
#' This function detects which quarters need relaxed timing rules
#' based on the data itself. 
#'
#' The algorithm detects exceptions at the per-month level within each quarter:
#' \itemize{
#'   \item Checks if standard rules produce impossible results (min > max)
#'   \item Checks if relaxed rules would resolve the issue
#'   \item Applies exception rules to entire quarter when any UPA requires it
#' }
#'
#' ## Cross-Quarter Aggregation (Important!)
#'
#' **For optimal determination rates (~97%), input data should be stacked across
#' multiple quarters** (ideally 4+ years). The algorithm leverages PNADC's rotating
#' panel design where the same UPA-V1014 is interviewed in the same relative week
#' across all quarterly visits.
#'
#' \itemize{
#'   \item **Per-quarter processing**: ~65-75% determination rate
#'   \item **Multi-quarter stacked**: ~97% determination rate
#' }
#'
#' ## Processing Steps
#'
#' The function processes data in the following steps:
#' \enumerate{
#'   \item Calculate first valid interview Saturday for each month in quarter (standard rule)
#'   \item For each person, calculate possible interview date range based on birthday constraints
#'   \item Convert date ranges to month-in-quarter positions
#'   \item Aggregate to UPA-panel level (all persons must agree)
#'   \item Calculate alternative (exception) date ranges and positions
#'   \item Dynamically detect which quarters/months need exception rules
#'   \item Apply exception rules and recalculate final month positions
#' }
#'
#' @examples
#' \dontrun{
#' # Identify reference months
#' result <- identify_reference_month(pnadc_data)
#'
#' # Check determination rate
#' result[, .(
#'   total = .N,
#'   determined = sum(!is.na(ref_month_in_quarter)),
#'   rate = mean(!is.na(ref_month_in_quarter))
#' ), by = .(Ano, Trimestre)]
#' }
#'
#' @export
identify_reference_month <- function(data, verbose = TRUE, .pb = NULL, .pb_offset = 0L) {

  # Note: Validation is done in pnadc_identify_periods() for fail-fast behavior.
  # When called directly, caller is responsible for valid input.

  # Initialize progress bar (8 steps total)
  # If external progress bar provided, use it; otherwise create our own
  use_external_pb <- !is.null(.pb)
  if (verbose && !use_external_pb) {
    cat("Identifying reference months...\n")
    pb <- txtProgressBar(min = 0, max = 8, style = 3)
  } else if (use_external_pb) {
    pb <- .pb
  }

  # Helper to update progress bar
  update_pb <- function(step) {
    if (verbose || use_external_pb) {
      setTxtProgressBar(pb, step + .pb_offset)
    }
  }

  # OPTIMIZATION: Subset to required columns BEFORE copying (80-90% memory reduction)
  # Instead of copying 50+ columns, only copy the ~9 columns we actually need
  required_cols <- required_vars_ref_month()
  dt <- subset_and_copy(data, required_cols)

  # OPTIMIZATION: Use consolidated helper for type conversion and NA code handling
  # Converts to integer (including V2009 - saves 4 bytes per row vs numeric)
  convert_pnadc_columns(dt)

  # ============================================================================
  # STEP 1: Pre-compute first valid Saturdays for each unique (year, quarter)
  # ============================================================================

  unique_quarters <- unique(dt[, .(Ano, Trimestre)])
  unique_quarters[, `:=`(
    month1 = quarter_month_n(Trimestre, 1L),
    month2 = quarter_month_n(Trimestre, 2L),
    month3 = quarter_month_n(Trimestre, 3L)
  )]

  # Calculate first valid Saturday for each month using STANDARD rule (min_days=4)
  # Standard rule: first Saturday with >= 4 days in month
  # dow <= 3 means first Saturday is day (7 - dow), otherwise (14 - dow)
  unique_quarters[, `:=`(
    first_sat_m1 = first_valid_saturday(Ano, month1, min_days = 4L),
    first_sat_m2 = first_valid_saturday(Ano, month2, min_days = 4L),
    first_sat_m3 = first_valid_saturday(Ano, month3, min_days = 4L)
  )]

  # Calculate first valid Saturday using ALTERNATIVE/EXCEPTION rule (min_days=3)
  # Exception rule: first Saturday with >= 3 days in month
  # dow <= 4 means first Saturday is day (7 - dow), otherwise (14 - dow)
  unique_quarters[, `:=`(
    alt_sat_m1 = first_valid_saturday(Ano, month1, min_days = 3L),
    alt_sat_m2 = first_valid_saturday(Ano, month2, min_days = 3L),
    alt_sat_m3 = first_valid_saturday(Ano, month3, min_days = 3L)
  )]

  # OPTIMIZATION: Use data.table join instead of merge() for speed
  # Join pre-computed values back to main data
  dt[unique_quarters,
     on = .(Ano, Trimestre),
     `:=`(month1 = i.month1, month2 = i.month2, month3 = i.month3,
          first_sat_m1 = i.first_sat_m1, first_sat_m2 = i.first_sat_m2,
          first_sat_m3 = i.first_sat_m3,
          alt_sat_m1 = i.alt_sat_m1, alt_sat_m2 = i.alt_sat_m2,
          alt_sat_m3 = i.alt_sat_m3)]

  update_pb(1)

  # ============================================================================
  # STEP 2: Calculate birthday and first Saturday after birthday
  # ============================================================================

  dt[, birthday := make_birthday(V20081, V2008, Ano)]
  dt[, first_sat_after_birthday := first_saturday_on_or_after(birthday)]

  # Determine if interview was before or after birthday
  # visitapreaniv = (Ano - V20082) - V2009
  # = 0 if interview was after birthday (age matches year calculation)
  # = 1 if interview was before birthday (still younger by 1 year)
  dt[, visit_before_birthday := NA_integer_]
  dt[!is.na(V20082), visit_before_birthday := as.integer((Ano - V20082) - V2009)]

  update_pb(2)

  # ============================================================================
  # STEP 3: Calculate date bounds and month positions using STANDARD rules
  # ============================================================================

  # Initialize date bounds using standard Saturday values
  # OPTIMIZATION: Combined into single := call
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

  # Convert date bounds to month-in-quarter positions using STANDARD thresholds (day <= 3)
  # OPTIMIZATION: Combined into single := call
  dt[, `:=`(
    month_min_pos = calculate_month_position_min(date_min, Ano, Trimestre, day_threshold = 3L),
    month_max_pos = calculate_month_position_max(date_max, Ano, Trimestre, day_threshold = 3L)
  )]

  # OPTIMIZATION: Remove birthday columns - no longer needed after Step 3
  # (keep first_sat_after_birthday for exception handling in Step 6)
  dt[, birthday := NULL]

  update_pb(3)

  # ============================================================================
  # STEP 4: Calculate date bounds and month positions using ALTERNATIVE rules
  # ============================================================================

  # Initialize alternative date bounds using alternative Saturday values
  # OPTIMIZATION: Combined into single := call
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

  # Convert alternative date bounds to month positions using EXCEPTION thresholds (day <= 2)
  # OPTIMIZATION: Combined into single := call
  dt[, `:=`(
    alt_month_min_pos = calculate_month_position_min(alt_date_min, Ano, Trimestre, day_threshold = 2L),
    alt_month_max_pos = calculate_month_position_max(alt_date_max, Ano, Trimestre, day_threshold = 2L)
  )]

  update_pb(4)

  # ============================================================================
  # STEP 4b: SINGLE aggregation pass for BOTH standard and alternative positions
  # (OPTIMIZATION: Combined from two separate aggregations)
  # (OPTIMIZATION: setkey for faster groupby operations)
  # ============================================================================

  # Set key for faster groupby on UPA, V1014 (used multiple times)
  data.table::setkey(dt, UPA, V1014)

  dt[, `:=`(
    upa_month_min = max(month_min_pos, na.rm = TRUE),
    upa_month_max = min(month_max_pos, na.rm = TRUE),
    alt_upa_month_min = max(alt_month_min_pos, na.rm = TRUE),
    alt_upa_month_max = min(alt_month_max_pos, na.rm = TRUE)
  ), by = .(UPA, V1014)]

  # Handle infinite values from all-NA groups (max returns -Inf, min returns Inf)
  dt[is.infinite(upa_month_min), upa_month_min := NA_integer_]
  dt[is.infinite(upa_month_max), upa_month_max := NA_integer_]
  dt[is.infinite(alt_upa_month_min), alt_upa_month_min := NA_integer_]
  dt[is.infinite(alt_upa_month_max), alt_upa_month_max := NA_integer_]

  # OPTIMIZATION: Remove date columns - no longer needed after aggregation
  # (alternative positions are now aggregated into upa_month_* columns)
  dt[, c("alt_date_min", "alt_date_max", "alt_month_min_pos", "alt_month_max_pos") := NULL]

  update_pb(5)

  # ============================================================================
  # STEP 5: Dynamically detect which quarters need exception rules
  # ============================================================================

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
  # requerexcecaomes1: need to relax month 1 rule (first month of quarter)
  # requerexcecaomes2: need to relax month 2 rule (second month of quarter)
  # requerexcecaomes3: need to relax month 3 rule (third month of quarter)
  # OPTIMIZATION: Combined into single := call
  dt[, `:=`(
    requires_exc_m1 = requires_exception & month_min_pos == 2L & alt_upa_month_min == 1L,
    requires_exc_m2 = requires_exception & ((month_max_pos == 1L & alt_upa_month_max >= 2L) |
                                              (month_min_pos == 3L & alt_upa_month_min <= 2L)),
    requires_exc_m3 = requires_exception & month_max_pos == 2L & alt_upa_month_max == 3L
  )]

  # Propagate exception requirement to entire quarter (Ano, Trimestre)
  # If ANY UPA in a quarter requires exception for a month, apply to ALL
  # OPTIMIZATION: Use sum() > 0 instead of max() to avoid -Inf cleanup

  dt[, `:=`(
    trim_exc_m1 = as.integer(sum(requires_exc_m1, na.rm = TRUE) > 0L),
    trim_exc_m2 = as.integer(sum(requires_exc_m2, na.rm = TRUE) > 0L),
    trim_exc_m3 = as.integer(sum(requires_exc_m3, na.rm = TRUE) > 0L)
  ), by = .(Ano, Trimestre)]

  update_pb(6)

  # ============================================================================
  # STEP 6: Apply exception rules and recalculate (OPTIMIZED: conditional)
  # ============================================================================

  # OPTIMIZATION: Compute exception condition once and reuse
  # Most quarters (~91%) don't need exceptions, so we skip recalculation for them
  exc_condition <- (dt$trim_exc_m1 == 1L | dt$trim_exc_m2 == 1L | dt$trim_exc_m3 == 1L)
  has_any_exception <- any(exc_condition)

  if (has_any_exception) {
    # Update first Saturday values where exceptions are needed
    dt[trim_exc_m1 == 1L, first_sat_m1 := alt_sat_m1]
    dt[trim_exc_m2 == 1L, first_sat_m2 := alt_sat_m2]
    dt[trim_exc_m3 == 1L, first_sat_m3 := alt_sat_m3]

    # Recalculate date bounds ONLY for exception rows
    # OPTIMIZATION: Combined into single := call
    dt[exc_condition, `:=`(
      date_min = make_date(Ano, month1, first_sat_m1),
      date_max = make_date(Ano, month3, first_sat_m3) + 21L
    )]

    # Re-apply birthday constraints ONLY for exception rows
    # Must combine exception condition with birthday conditions
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

    # Recalculate month positions ONLY for exception rows with dynamic thresholds
    # OPTIMIZATION: Combined into single := call
    dt[exc_condition, `:=`(
      month_min_pos = calculate_month_position_min_dynamic(
        date_min, Ano, Trimestre, trim_exc_m1, trim_exc_m2, trim_exc_m3
      ),
      month_max_pos = calculate_month_position_max_dynamic(
        date_max, Ano, Trimestre, trim_exc_m1, trim_exc_m2, trim_exc_m3
      )
    )]
  }

  # Final aggregation to UPA-panel level ACROSS ALL QUARTERS
  # OPTIMIZATION: Skip aggregation if no exceptions - reuse results from Step 4b
  if (has_any_exception) {
    # Re-aggregate only when exceptions modified some positions
    dt[, `:=`(
      upa_month_min_final = max(month_min_pos, na.rm = TRUE),
      upa_month_max_final = min(month_max_pos, na.rm = TRUE)
    ), by = .(UPA, V1014)]
  } else {
    # No exceptions - reuse aggregation from Step 4b (simple column copy)
    dt[, `:=`(
      upa_month_min_final = upa_month_min,
      upa_month_max_final = upa_month_max
    )]
  }

  # OPTIMIZATION: Remove intermediate columns no longer needed after Step 6
  # This reduces memory footprint before final output selection
  temp_cols <- c(
    "month1", "month2", "month3",
    "first_sat_m1", "first_sat_m2", "first_sat_m3",
    "alt_sat_m1", "alt_sat_m2", "alt_sat_m3",
    "first_sat_after_birthday", "visit_before_birthday",
    "date_min", "date_max",
    "month_min_pos", "month_max_pos",
    "upa_month_min", "upa_month_max",
    "alt_upa_month_min", "alt_upa_month_max",
    "requires_exception", "requires_exc_m1", "requires_exc_m2", "requires_exc_m3",
    "trim_exc_m1", "trim_exc_m2", "trim_exc_m3"
  )
  dt[, (intersect(temp_cols, names(dt))) := NULL]
  # OPTIMIZATION: Removed explicit gc() call - R's garbage collector runs automatically
  # and explicit calls can actually slow down performance

  update_pb(7)

  # ============================================================================
  # STEP 7: Assign reference month
  # ============================================================================

  # Assign reference month: if min == max, that's the month; otherwise indeterminate
  dt[, ref_month_in_quarter := NA_integer_]
  dt[upa_month_min_final == upa_month_max_final &
       upa_month_min_final >= 1L & upa_month_max_final <= 3L,
     ref_month_in_quarter := upa_month_min_final]

  # Calculate final reference month Date and YYYYMM
  dt[!is.na(ref_month_in_quarter), `:=`(
    ref_month = make_date(Ano, quarter_month_n(Trimestre, ref_month_in_quarter), 1L),
    ref_month_yyyymm = yyyymm(Ano, quarter_month_n(Trimestre, ref_month_in_quarter))
  )]

  dt[is.na(ref_month_in_quarter), `:=`(
    ref_month = as.Date(NA),
    ref_month_yyyymm = NA_integer_
  )]

  # ============================================================================
  # STEP 8: Select output columns and return
  # ============================================================================

  key_cols <- intersect(join_key_vars(), names(dt))
  output_cols <- c(key_cols, "ref_month", "ref_month_in_quarter", "ref_month_yyyymm")

  result <- dt[, ..output_cols]

  # Store determination rate as attribute
  attr(result, "determination_rate") <- mean(!is.na(result$ref_month_in_quarter))

  update_pb(8)

  # Close progress bar and show summary (only if we created our own)
  if (verbose && !use_external_pb) {
    close(pb)
    cat(sprintf("  Determination rate: %.1f%%\n", attr(result, "determination_rate") * 100))
  }

  result
}


#' Calculate Month Position in Quarter (for date_min)
#'
#' Converts a date_min to its month position within the quarter (1, 2, or 3).
#' For date_min: if day <= threshold and not in first month of quarter, subtract 1.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param quarter Integer quarter vector (1-4)
#' @param day_threshold Integer threshold for adjustment (3 for standard, 2 for exception)
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
calculate_month_position_min <- function(date, year, quarter, day_threshold = 3L) {
  first_month <- quarter_first_month(quarter)
  date_month <- fast_month(date)
  date_day <- fast_mday(date)

  pos <- date_month - first_month + 1L

  # Adjust if day <= threshold and not in first month of quarter
  adjust <- (date_day <= day_threshold & date_month > first_month)
  pos <- pos - as.integer(adjust)

  pmin(pmax(pos, 1L), 3L)
}


#' Calculate Month Position in Quarter (for date_max)
#'
#' Converts a date_max to its month position within the quarter (1, 2, or 3).
#' For date_max: if day <= threshold, use the month of (date - 3 days) instead.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param quarter Integer quarter vector (1-4)
#' @param day_threshold Integer threshold for adjustment (3 for standard, 2 for exception)
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
calculate_month_position_max <- function(date, year, quarter, day_threshold = 3L) {
  first_month <- quarter_first_month(quarter)
  date_day <- fast_mday(date)

  # When day <= threshold, use the month of (date - 3 days)
  needs_adjust <- date_day <= day_threshold
  adjusted_date <- date - needs_adjust * 3L
  adjusted_month <- fast_month(adjusted_date)

  pos <- adjusted_month - first_month + 1L

  pmin(pmax(pos, 1L), 3L)
}


#' Calculate Month Position (date_min) with Dynamic Per-Month Thresholds
#'
#' Applies different thresholds based on which month the date falls in and
#' whether that month has exception rules enabled.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param quarter Integer quarter vector (1-4)
#' @param exc_m1 Integer: exception flag for month 1 of quarter (0 or 1)
#' @param exc_m2 Integer: exception flag for month 2 of quarter (0 or 1)
#' @param exc_m3 Integer: exception flag for month 3 of quarter (0 or 1)
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
calculate_month_position_min_dynamic <- function(date, year, quarter, exc_m1, exc_m2, exc_m3) {
  first_month <- quarter_first_month(quarter)
  date_month <- fast_month(date)
  date_day <- fast_mday(date)

  pos <- date_month - first_month + 1L

  # Determine threshold based on which month the date falls in
  # Month 1 of quarter: months 1, 4, 7, 10
  # Month 2 of quarter: months 2, 5, 8, 11
  # Month 3 of quarter: months 3, 6, 9, 12
  # Use modular arithmetic to identify position in quarter
  month_in_quarter <- ((date_month - 1L) %% 3L) + 1L  # 1, 2, or 3

  # Threshold is 2 if exception applies for this month, 3 otherwise
  # OPTIMIZATION: Use fifelse instead of rep() + subsetting
  threshold <- fifelse(
    (month_in_quarter == 1L & exc_m1 == 1L) |
    (month_in_quarter == 2L & exc_m2 == 1L) |
    (month_in_quarter == 3L & exc_m3 == 1L),
    2L, 3L
  )

  # Adjust if day <= threshold and not in first month of quarter
  adjust <- (date_day <= threshold & date_month > first_month)
  pos <- pos - as.integer(adjust)

  pmin(pmax(pos, 1L), 3L)
}


#' Calculate Month Position (date_max) with Dynamic Per-Month Thresholds
#'
#' Applies different thresholds based on which month the date falls in and
#' whether that month has exception rules enabled.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param quarter Integer quarter vector (1-4)
#' @param exc_m1 Integer: exception flag for month 1 of quarter (0 or 1)
#' @param exc_m2 Integer: exception flag for month 2 of quarter (0 or 1)
#' @param exc_m3 Integer: exception flag for month 3 of quarter (0 or 1)
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
calculate_month_position_max_dynamic <- function(date, year, quarter, exc_m1, exc_m2, exc_m3) {
  first_month <- quarter_first_month(quarter)
  date_month <- fast_month(date)
  date_day <- fast_mday(date)

  # Determine threshold based on which month the date falls in
  # Use modular arithmetic to identify position in quarter
  month_in_quarter <- ((date_month - 1L) %% 3L) + 1L  # 1, 2, or 3

  # Threshold is 2 if exception applies for this month, 3 otherwise
  # OPTIMIZATION: Use fifelse instead of rep() + subsetting
  threshold <- fifelse(
    (month_in_quarter == 1L & exc_m1 == 1L) |
    (month_in_quarter == 2L & exc_m2 == 1L) |
    (month_in_quarter == 3L & exc_m3 == 1L),
    2L, 3L
  )

  # When day <= threshold, use the month of (date - 3 days)
  needs_adjust <- date_day <= threshold
  adjusted_date <- date - needs_adjust * 3L
  adjusted_month <- fast_month(adjusted_date)

  pos <- adjusted_month - first_month + 1L

  pmin(pmax(pos, 1L), 3L)
}
