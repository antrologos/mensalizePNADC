#' Identify Reference Week in PNADC Data
#'
#' Determines which ISO week each survey observation corresponds to based on
#' IBGE's interview timing rules and birthday constraints.
#'
#' @description
#' PNADC is a quarterly survey where each interview occurs during a specific
#' week within the quarter. This function identifies which ISO 8601 week that
#' observation belongs to, enabling weekly time series analysis.
#'
#' The algorithm uses:
#' \itemize{
#'   \item IBGE's reference week timing rules (first Saturday with sufficient days in month)
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
#'     \item \code{ref_week}: Reference week as Date (Monday of that week)
#'     \item \code{ref_week_in_quarter}: Position in quarter (1-14) or NA if indeterminate
#'     \item \code{ref_week_yyyyww}: Integer YYYYWW format (e.g., 202301)
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
#' The weekly determination rate (~1-2%) is much lower than monthly (~97%) because:
#' \itemize{
#'   \item Cannot aggregate across quarters (no consistent week position in panel design)
#'   \item Finer granularity (13-14 weeks vs 3 months per quarter)
#'   \item Birthday constraints rarely narrow the interview window to a single 7-day period
#' }
#'
#' ## ISO 8601 Weeks
#'
#' Weeks follow ISO 8601 standard:
#' \itemize{
#'   \item Week 1 is the week containing January 4th
#'   \item Weeks start on Monday
#'   \item Dec 31 may be in week 1 of the following year
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

  # Initialize progress bar (7 steps total)
  use_external_pb <- !is.null(.pb)
  if (verbose && !use_external_pb) {
    cat("Identifying reference weeks...\n")
    pb <- txtProgressBar(min = 0, max = 7, style = 3)
  } else if (use_external_pb) {
    pb <- .pb
  }

  # Helper to update progress bar
  update_pb <- function(step) {
    if (verbose || use_external_pb) {
      setTxtProgressBar(pb, step + .pb_offset)
    }
  }

  # Convert to data.table (copy to avoid modifying original)
  dt <- ensure_data_table(data, copy = TRUE)

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

  # Initialize date bounds using standard Saturday values
  dt[, `:=`(
    date_min = make_date(Ano, month1, first_sat_m1),
    date_max = make_date(Ano, month3, first_sat_m3) + 21L  # 4 interview weeks
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
  # STEP 4: Convert date bounds to ISO weeks
  # ============================================================================

  # Convert to ISO week format
  dt[, `:=`(
    week_min_yyyyww = date_to_yyyyww(date_min),
    week_max_yyyyww = date_to_yyyyww(date_max)
  )]

  # Also calculate week position within quarter for output
  dt[, `:=`(
    week_min_pos = week_in_quarter(date_min, Trimestre, Ano),
    week_max_pos = week_in_quarter(date_max, Trimestre, Ano)
  )]

  # Calculate alternative date bounds using EXCEPTION rule (min_days=3)
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

  # Convert alternative bounds to ISO weeks
  dt[, `:=`(
    alt_week_min_yyyyww = date_to_yyyyww(alt_date_min),
    alt_week_max_yyyyww = date_to_yyyyww(alt_date_max)
  )]

  update_pb(4)

  # ============================================================================
  # STEP 5: Aggregate constraints at HOUSEHOLD level WITHIN each quarter
  # Key insight: All persons in same household (UPA + V1008) interviewed on same day
  # ============================================================================

  # Set key for faster groupby operations
  data.table::setkey(dt, Ano, Trimestre, UPA, V1008)

  # Aggregate: max of mins (latest possible start), min of maxes (earliest possible end)
  # For YYYYWW format, we compare as integers (works because format is lexicographically correct)
  dt[, `:=`(
    hh_week_min = max(week_min_yyyyww, na.rm = TRUE),
    hh_week_max = min(week_max_yyyyww, na.rm = TRUE),
    alt_hh_week_min = max(alt_week_min_yyyyww, na.rm = TRUE),
    alt_hh_week_max = min(alt_week_max_yyyyww, na.rm = TRUE)
  ), by = .(Ano, Trimestre, UPA, V1008)]

  # Handle infinite values from all-NA groups (max returns -Inf, min returns Inf)
  dt[is.infinite(hh_week_min), hh_week_min := NA_integer_]
  dt[is.infinite(hh_week_max), hh_week_max := NA_integer_]
  dt[is.infinite(alt_hh_week_min), alt_hh_week_min := NA_integer_]
  dt[is.infinite(alt_hh_week_max), alt_hh_week_max := NA_integer_]

  # Clean up alternative date columns
  dt[, c("alt_date_min", "alt_date_max", "alt_week_min_yyyyww", "alt_week_max_yyyyww") := NULL]

  update_pb(5)

  # ============================================================================
  # STEP 6: Dynamic exception detection and application
  # ============================================================================

  # An observation requires exception if:
  # 1. Standard rules produce impossible result (hh_week_min > hh_week_max)
  # 2. Alternative rules would produce valid result (alt_hh_week_min <= alt_hh_week_max)
  dt[, requires_exception := (
    hh_week_min > hh_week_max &
    alt_hh_week_min <= alt_hh_week_max
  )]

  # Propagate exception requirement to entire quarter
  dt[, trim_has_exception := as.integer(sum(requires_exception, na.rm = TRUE) > 0L),
     by = .(Ano, Trimestre)]

  # Apply exception rules where needed
  exc_condition <- (dt$trim_has_exception == 1L)
  has_any_exception <- any(exc_condition)

  if (has_any_exception) {
    # For quarters with exceptions, use alternative bounds
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
  # STEP 7: Assign reference week
  # ============================================================================

  # Assign reference week: if min == max, that's the week; otherwise indeterminate
  dt[, ref_week_yyyyww := NA_integer_]
  dt[hh_week_min == hh_week_max & !is.infinite(hh_week_min),
     ref_week_yyyyww := hh_week_min]

  # Calculate final reference week Date (Monday of that week)
  dt[, ref_week := as.Date(NA)]
  dt[!is.na(ref_week_yyyyww),
     ref_week := yyyyww_to_date(ref_week_yyyyww)]

  # Calculate week position in quarter
  dt[, ref_week_in_quarter := NA_integer_]
  dt[!is.na(ref_week),
     ref_week_in_quarter := week_in_quarter(ref_week, Trimestre, Ano)]

  # ============================================================================
  # STEP 8: Select output columns and return
  # ============================================================================

  # Clean up intermediate columns
  temp_cols <- c(
    "month1", "month2", "month3",
    "first_sat_m1", "first_sat_m2", "first_sat_m3",
    "alt_sat_m1", "alt_sat_m2", "alt_sat_m3",
    "first_sat_after_birthday", "visit_before_birthday",
    "date_min", "date_max",
    "week_min_yyyyww", "week_max_yyyyww",
    "week_min_pos", "week_max_pos",
    "hh_week_min", "hh_week_max"
  )
  dt[, (intersect(temp_cols, names(dt))) := NULL]
  gc()

  # Select output columns
  key_cols <- intersect(join_key_vars(), names(dt))
  output_cols <- c(key_cols, "ref_week", "ref_week_in_quarter", "ref_week_yyyyww")

  result <- dt[, ..output_cols]

  # Store determination rate as attribute
  attr(result, "determination_rate") <- mean(!is.na(result$ref_week_in_quarter))

  update_pb(7)

  # Close progress bar and show summary (only if we created our own)
  if (verbose && !use_external_pb) {
    close(pb)
    cat(sprintf("  Determination rate: %.1f%%\n", attr(result, "determination_rate") * 100))
  }

  result
}
