#' Identify Reference Week in PNADC Data
#'
#' Determines which IBGE reference week each survey observation corresponds to
#' based on IBGE's interview timing rules and birthday constraints.
#'
#' @description
#' PNADC is a quarterly survey where each interview occurs during a specific
#' week within the quarter. This function identifies which IBGE reference week
#' that observation belongs to, enabling weekly time series analysis.
#'
#' The algorithm uses:
#' \itemize{
#'   \item IBGE's reference week timing rules (first Saturday with sufficient days in month)
#'   \item IBGE week boundaries (Sunday-Saturday, not ISO Monday-Sunday)
#'   \item Respondent birthdates to constrain possible interview dates
#'   \item Household-level aggregation (all persons in same household interviewed on same day)
#'   \item Dynamic exception detection (identifies quarters needing relaxed rules from the data)
#' }
#'
#' **Key difference from monthly identification:** This function aggregates at the
#' household level WITHIN each quarter, not at the UPA-V1014 level across quarters.
#' This is because households are interviewed in the same month position across
#' quarterly visits, but NOT in the same week position.
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
#'   Optional but recommended for complete crosswalk:
#'   \itemize{
#'     \item \code{V2003}: Person sequence within household
#'   }
#' @param verbose Logical. If TRUE (default), display progress bar and step information.
#' @param .pb Optional progress bar object created by \code{txtProgressBar}. If provided,
#'   the function updates this progress bar instead of creating its own.
#' @param .pb_offset Integer. Offset to add to progress bar updates when using an external
#'   progress bar. Default is 0.
#'
#' @return A data.table with the original key columns plus:
#'   \itemize{
#'     \item \code{ref_week_start}: Reference week start (Sunday of IBGE week)
#'     \item \code{ref_week_end}: Reference week end (Saturday of IBGE week)
#'     \item \code{ref_week_in_quarter}: Position in quarter (1-12, always 4 weeks
#'       per month × 3 months = 12 weeks per quarter) or NA if indeterminate.
#'       Technical stops are handled via fallback rules (see Technical Stops section).
#'     \item \code{ref_week_yyyyww}: Integer YYYYWW format (IBGE-based)
#'   }
#'
#' @details
#' ## Household Aggregation
#'
#' All persons in the same household (UPA + V1008) are interviewed on the same day.
#' The algorithm aggregates birthday constraints across household members to narrow
#' down the possible interview week:
#' \itemize{
#'   \item If person A has weeks 2-4 as possibilities
#'   \item And person B has weeks 3-5 as possibilities
#'   \item The household's interview week must be 3-4 (the intersection)
#' }
#'
#' Aggregation is done by \code{(Ano, Trimestre, UPA, V1008)} - within each quarter
#' only, because households are NOT interviewed in the same week across quarterly visits.
#'
#' ## Expected Determination Rate
#'
#' The weekly determination rate (~1.5%) is much lower than monthly (~97%) because:
#' \itemize{
#'   \item Cannot aggregate across quarters (no consistent week position in panel design)
#'   \item Finer granularity (12 weeks vs 3 months per quarter)
#'   \item Birthday constraints rarely narrow the interview window to a single 7-day period
#' }
#'
#' ## IBGE Reference Weeks
#'
#' Weeks follow IBGE reference calendar:
#' \itemize{
#'   \item Weeks start on Sunday and end on Saturday
#'   \item A week belongs to the IBGE month where its Saturday falls
#'   \item The first valid Saturday must have >= 4 days in the month
#'   \item Each IBGE month has exactly 4 reference weeks (28 days)
#'   \item Each IBGE quarter has exactly 12 reference weeks
#' }
#'
#' ## Technical Stops (Paradas Técnicas)
#'
#' Technical stops are periods (full weeks from Sunday to Saturday) that fall
#' between valid IBGE reference weeks. They are NOT part of any IBGE month.
#' When a person's possible date range falls entirely within a technical stop,
#' the following rules are applied:
#'
#' \itemize{
#'   \item \strong{Rule 3.1 - Quarter boundary}: If the technical stop is between
#'     quarters, the interview is assigned to the last valid week of the quarter
#'     the observation belongs to (week 12), since interviews must belong to
#'     their source quarter.
#'   \item \strong{Rule 3.2 - Household consensus}: If the technical stop is within
#'     a quarter, the algorithm checks if other household members have a valid
#'     week assignment. If all assigned members agree on a single week, that
#'     week is used.
#'   \item \strong{Rule 3.3 - Fallback}: If no household consensus exists, the
#'     interview is assigned to the first valid week AFTER the technical stop
#'     (that still belongs to the same quarter).
#' }
#'
#' @examples
#' \dontrun{
#' # Identify reference weeks
#' result <- identify_reference_week(pnadc_data)
#'
#' # Check determination rate
#' result[, .(
#'   total = .N,
#'   determined = sum(!is.na(ref_week_in_quarter)),
#'   rate = mean(!is.na(ref_week_in_quarter))
#' ), by = .(Ano, Trimestre)]
#' }
#'
#' @export
identify_reference_week <- function(data, verbose = TRUE, .pb = NULL, .pb_offset = 0L) {

  # Note: Validation is done in pnadc_identify_periods() for fail-fast behavior.
  # When called directly, caller is responsible for valid input.

  # Initialize progress bar (8 steps total)
  use_external_pb <- !is.null(.pb)
  if (verbose && !use_external_pb) {
    cat("Identifying reference weeks...\n")
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
  required_cols <- required_vars_ref_month()
  dt <- subset_and_copy(data, required_cols)

  # OPTIMIZATION: Use consolidated helper for type conversion and NA code handling
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
  unique_quarters[, `:=`(
    first_sat_m1 = first_valid_saturday(Ano, month1, min_days = 4L),
    first_sat_m2 = first_valid_saturday(Ano, month2, min_days = 4L),
    first_sat_m3 = first_valid_saturday(Ano, month3, min_days = 4L)
  )]

  # Calculate first valid Saturday using ALTERNATIVE/EXCEPTION rule (min_days=3)
  unique_quarters[, `:=`(
    alt_sat_m1 = first_valid_saturday(Ano, month1, min_days = 3L),
    alt_sat_m2 = first_valid_saturday(Ano, month2, min_days = 3L),
    alt_sat_m3 = first_valid_saturday(Ano, month3, min_days = 3L)
  )]

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
  dt[, visit_before_birthday := NA_integer_]
  dt[!is.na(V20082), visit_before_birthday := as.integer((Ano - V20082) - V2009)]

  update_pb(2)

  # ============================================================================
  # STEP 3: Calculate date bounds using STANDARD rules
  # ============================================================================

  # Calculate the last Saturday of the quarter using IBGE month end
  # (IBGE months always have exactly 4 reference weeks)
  dt[, quarter_end := ibge_month_end(Ano, month3, min_days = 4L)]

  # Initialize date bounds using standard Saturday values
  dt[, `:=`(
    date_min = make_date(Ano, month1, first_sat_m1),
    date_max = quarter_end
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

  # Remove birthday column - no longer needed
  dt[, birthday := NULL]

  update_pb(3)

  # ============================================================================
  # STEP 4: Convert date bounds to IBGE weeks
  # ============================================================================

  # Convert to IBGE week position within quarter (1-12)
  dt[, `:=`(
    week_min_pos = ibge_week_in_quarter(date_min, Trimestre, Ano, min_days = 4L),
    week_max_pos = ibge_week_in_quarter(date_max, Trimestre, Ano, min_days = 4L)
  )]

  # Calculate alternative date bounds using EXCEPTION rule (min_days=3)
  dt[, alt_quarter_end := ibge_month_end(Ano, month3, min_days = 3L)]
  dt[, `:=`(
    alt_date_min = make_date(Ano, month1, alt_sat_m1),
    alt_date_max = alt_quarter_end
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

  # Convert alternative bounds to IBGE week positions
  dt[, `:=`(
    alt_week_min_pos = ibge_week_in_quarter(alt_date_min, Trimestre, Ano, min_days = 3L),
    alt_week_max_pos = ibge_week_in_quarter(alt_date_max, Trimestre, Ano, min_days = 3L)
  )]

  update_pb(4)

  # ============================================================================
  # STEP 5: Aggregate constraints at HOUSEHOLD level WITHIN each quarter
  # Key insight: All persons in same household (UPA + V1008) interviewed on same day
  # ============================================================================

  # Set key for faster groupby operations
  data.table::setkey(dt, Ano, Trimestre, UPA, V1008)

  # Aggregate: max of mins (latest possible start), min of maxes (earliest possible end)
  # Using week positions (integers) for comparison
  dt[, `:=`(
    hh_week_min = max(week_min_pos, na.rm = TRUE),
    hh_week_max = min(week_max_pos, na.rm = TRUE),
    alt_hh_week_min = max(alt_week_min_pos, na.rm = TRUE),
    alt_hh_week_max = min(alt_week_max_pos, na.rm = TRUE)
  ), by = .(Ano, Trimestre, UPA, V1008)]

  # Handle infinite values from all-NA groups (max returns -Inf, min returns Inf)
  dt[is.infinite(hh_week_min), hh_week_min := NA_integer_]
  dt[is.infinite(hh_week_max), hh_week_max := NA_integer_]
  dt[is.infinite(alt_hh_week_min), alt_hh_week_min := NA_integer_]
  dt[is.infinite(alt_hh_week_max), alt_hh_week_max := NA_integer_]

  # Clean up alternative date columns
  dt[, c("alt_date_min", "alt_date_max", "alt_week_min_pos", "alt_week_max_pos",
         "alt_quarter_end") := NULL]

  update_pb(5)

  # ============================================================================
  # STEP 6: Dynamic exception detection (fallback to min_days=3 rule)
  # ============================================================================
  #
  # IBGE's "Parada Técnica" defines two rules for first valid Saturday:
  #   - Standard rule: First Saturday with >= 4 days in the month
  #   - Exception rule: First Saturday with >= 3 days in the month
  #
  # The exception rule is a fallback used when standard rules produce an
  # impossible result (hh_week_min > hh_week_max after birthday constraints
  # are applied). This can happen in edge cases where the standard Saturday
  # selection creates contradictory date bounds.
  #
  # When ANY household in a quarter requires the exception, we apply it
  # to the ENTIRE quarter for consistency - this follows how IBGE applies
  # exceptions at the quarter level in their methodology.
  #
  # The detection logic:
  #   1. requires_exception = TRUE if standard rules are impossible BUT
  #      alternative (min_days=3) rules would be valid
  #   2. If any household in the quarter requires exception, all use it
  #   3. The alternative bounds (alt_hh_*) replace standard bounds

  # Check if standard rules produce impossible result but alt rules work
  dt[, requires_exception := (
    hh_week_min > hh_week_max &
    alt_hh_week_min <= alt_hh_week_max
  )]

  # Propagate exception to entire quarter for consistency
  dt[, trim_has_exception := as.integer(sum(requires_exception, na.rm = TRUE) > 0L),
     by = .(Ano, Trimestre)]

  # Apply exception rules (fallback to min_days=3) where needed
  exc_condition <- (dt$trim_has_exception == 1L)
  has_any_exception <- any(exc_condition)

  if (has_any_exception) {
    # Replace standard bounds with alternative bounds for entire quarter
    dt[exc_condition, `:=`(
      hh_week_min = alt_hh_week_min,
      hh_week_max = alt_hh_week_max
    )]
  }

  # Remove exception-related intermediate columns
  dt[, c("requires_exception", "trim_has_exception",
         "alt_hh_week_min", "alt_hh_week_max") := NULL]

  update_pb(6)

  # ============================================================================
  # STEP 6b: Handle Technical Stops (Paradas Técnicas)
  # ============================================================================
  #
 # Technical stops are periods (full weeks from Sunday to Saturday) that fall
  # between valid IBGE reference weeks. They occur:
  #   - Between quarters (most common)
  #   - Between months within a quarter (less common)
  #
  # When a person's possible date range falls entirely within a technical stop,
  # special handling rules apply:
  #
  # Rule 3.1: Quarter boundary technical stop
  #   - The interview MUST belong to the quarter the observation came from
  #   - Assign to the last valid week of that quarter (week 12)
  #
  # Rule 3.2: Within-quarter technical stop - household resolution
  #   - Check if other household members have a valid week assignment
  #   - If all assigned members agree on a week, use that week
  #
  # Rule 3.3: Fallback to week after technical stop
  #   - If no household consensus or no other members assigned,
  #   - Assign to the first valid week AFTER the technical stop
  #   - (that still belongs to the same quarter)

  # Identify cases that are still impossible after exception handling
  # These are potential technical stop cases
  dt[, in_technical_stop_range := (
    (hh_week_min > hh_week_max) |
    is.na(hh_week_min) |
    is.na(hh_week_max)
  )]

  # Only proceed if there are any technical stop cases
  has_tech_stop_cases <- any(dt$in_technical_stop_range, na.rm = TRUE)

  if (has_tech_stop_cases) {
    # Detect what type of technical stop for each observation
    # Use the midpoint of date_min and date_max as representative date
    dt[in_technical_stop_range == TRUE, tech_stop_type := detect_technical_stop_type(
      date_min + as.integer((date_max - date_min) / 2),
      Trimestre, Ano, min_days = 4L
    )]

    # Rule 3.1: Quarter boundary - assign to last week of quarter (week 12)
    # The interview must belong to the quarter the data comes from
    dt[in_technical_stop_range == TRUE & tech_stop_type == "quarter_boundary",
       `:=`(hh_week_min = 12L, hh_week_max = 12L)]

    # Rule 3.2 & 3.3: Within-quarter technical stop
    # First, do an initial assignment for non-technical-stop cases
    dt[, ref_week_temp := NA_integer_]
    dt[hh_week_min == hh_week_max & !is.na(hh_week_min) & hh_week_min >= 1L,
       ref_week_temp := hh_week_min]

    # Rule 3.2: Try to resolve using household consensus
    # Get the weeks assigned to other household members
    dt[, hh_consensus_week := {
      assigned_weeks <- ref_week_temp[!is.na(ref_week_temp)]
      if (length(assigned_weeks) > 0 && length(unique(assigned_weeks)) == 1L) {
        unique(assigned_weeks)
      } else {
        NA_integer_
      }
    }, by = .(Ano, Trimestre, UPA, V1008)]

    # Apply household consensus for within-quarter technical stops
    dt[in_technical_stop_range == TRUE &
       tech_stop_type == "within_quarter" &
       !is.na(hh_consensus_week),
       `:=`(hh_week_min = hh_consensus_week, hh_week_max = hh_consensus_week)]

    # Rule 3.3: Fallback to first valid week after technical stop
    # For cases that couldn't be resolved by household consensus
    dt[in_technical_stop_range == TRUE &
       tech_stop_type == "within_quarter" &
       is.na(hh_consensus_week),
       `:=`(
         fallback_week = first_valid_week_after_technical_stop(
           date_min + as.integer((date_max - date_min) / 2),
           Trimestre, Ano, min_days = 4L
         )
       )]

    # Apply fallback week
    dt[in_technical_stop_range == TRUE &
       tech_stop_type == "within_quarter" &
       is.na(hh_consensus_week) &
       !is.na(fallback_week),
       `:=`(hh_week_min = fallback_week, hh_week_max = fallback_week)]

    # Clean up temporary columns
    cols_to_remove <- intersect(
      c("tech_stop_type", "ref_week_temp", "hh_consensus_week", "fallback_week"),
      names(dt)
    )
    if (length(cols_to_remove) > 0) {
      dt[, (cols_to_remove) := NULL]
    }
  }

  # Clean up technical stop tracking column
  dt[, in_technical_stop_range := NULL]

  update_pb(7)

  # ============================================================================
  # STEP 7: Assign reference week
  # ============================================================================

  # Assign reference week: if min == max, that's the week; otherwise indeterminate
  dt[, ref_week_in_quarter := NA_integer_]
  dt[hh_week_min == hh_week_max & !is.na(hh_week_min) & hh_week_min >= 1L,
     ref_week_in_quarter := hh_week_min]

  # Calculate IBGE week dates from position (Sunday start, Saturday end)
  dt[, `:=`(ref_week_start = as.Date(NA), ref_week_end = as.Date(NA))]
  dt[!is.na(ref_week_in_quarter), `:=`(
    ref_week_start = ibge_week_dates_from_position(Ano, Trimestre, ref_week_in_quarter)$start,
    ref_week_end = ibge_week_dates_from_position(Ano, Trimestre, ref_week_in_quarter)$end
  )]

  # Calculate YYYYWW format from the Saturday of each week
  dt[, ref_week_yyyyww := NA_integer_]
  dt[!is.na(ref_week_end),
     ref_week_yyyyww := date_to_ibge_yyyyww(ref_week_end)]

  update_pb(8)

  # ============================================================================
  # STEP 8: Select output columns and return
  # ============================================================================

  # Clean up intermediate columns
  temp_cols <- c(
    "month1", "month2", "month3",
    "first_sat_m1", "first_sat_m2", "first_sat_m3",
    "alt_sat_m1", "alt_sat_m2", "alt_sat_m3",
    "first_sat_after_birthday", "visit_before_birthday",
    "date_min", "date_max", "quarter_end",
    "week_min_pos", "week_max_pos",
    "hh_week_min", "hh_week_max"
  )
  dt[, (intersect(temp_cols, names(dt))) := NULL]
  # OPTIMIZATION: Removed explicit gc() call - R's garbage collector runs automatically

  # Select output columns - use new column names
  key_cols <- intersect(join_key_vars(), names(dt))
  output_cols <- c(key_cols, "ref_week_start", "ref_week_end", "ref_week_in_quarter", "ref_week_yyyyww")

  # Return unique rows at the output key level (removes person-level duplicates)
  result <- unique(dt[, ..output_cols])

  # Store determination rate as attribute
  attr(result, "determination_rate") <- mean(!is.na(result$ref_week_in_quarter))

  # Close progress bar and show summary (only if we created our own)
  if (verbose && !use_external_pb) {
    close(pb)
    cat(sprintf("  Determination rate: %.1f%%\n", attr(result, "determination_rate") * 100))
  }

  result
}
