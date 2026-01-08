#' Identify Reference Month in PNADC Data
#'
#' Determines which month within each quarter corresponds to each survey
#' observation based on IBGE's "Parada Tecnica" (technical break) rules.
#'
#' @description
#' PNADC is a quarterly survey, but each interview actually refers to a specific
#' week within the quarter. This function identifies which month that week belongs
#' to, enabling monthly (instead of quarterly) time series analysis.
#'
#' The algorithm uses:
#' \itemize{
#'   \item IBGE's reference week timing rules (first Saturday with sufficient days in month)
#'   \item Respondent birthdates to constrain possible interview dates
#'   \item UPA-panel level aggregation (everyone in same sampling unit interviewed together)
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
#'
#' @param exception_quarters Character vector of quarters with relaxed timing rules,
#'   in format "YYYYtQ" (e.g., "2016t3"). If NULL (default), uses the built-in list
#'   of known exceptions: 2016t3, 2016t4, 2017t2, 2022t3, 2023t2.
#'
#' @return A data.table with the original key columns plus:
#'   \itemize{
#'     \item \code{ref_month}: Reference month as Date (first day of month, e.g., "2023-01-01")
#'     \item \code{ref_month_in_quarter}: Position in quarter (1, 2, 3) or NA if indeterminate
#'     \item \code{ref_month_yyyymm}: Integer YYYYMM format (e.g., 202301)
#'   }
#'
#' @details
#' ## Cross-Quarter Aggregation (Important!)
#'
#' **For optimal determination rates (~95%), input data should be stacked across
#' multiple quarters** (ideally 2+ years). The algorithm leverages PNADC's rotating
#' panel design where the same UPA-V1014 is interviewed in the same relative week
#' across all quarterly visits.
#'
#' \itemize{
#'   \item **Per-quarter processing**: ~65-75% determination rate
#'   \item **Multi-quarter stacked**: ~95% determination rate
#' }
#'
#' The cross-quarter aggregation dramatically improves accuracy by combining
#' birthday constraints from multiple interview rounds.
#'
#' ## Processing Steps
#'
#' The function processes data in the following steps:
#' \enumerate{
#'   \item Calculate first valid interview Saturday for each month in quarter
#'   \item For each person, calculate possible interview date range based on birthday constraints
#'   \item Convert date ranges to month-in-quarter positions
#'   \item Aggregate to UPA-panel level (all persons must agree)
#'   \item Handle exception quarters with relaxed timing rules
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
#' @seealso \code{\link{get_exception_quarters}} for the list of exception quarters
#'
#' @export
identify_reference_month <- function(data, exception_quarters = NULL) {

  # Validate input
  validate_pnadc(data, check_weights = FALSE)

  # Convert to data.table (copy to avoid modifying original)
  dt <- ensure_data_table(data, copy = TRUE)

  # Convert character columns to proper types (PNADC data often has character columns)
  if (is.character(dt$Ano)) dt[, Ano := as.integer(Ano)]
  if (is.character(dt$Trimestre)) dt[, Trimestre := as.integer(Trimestre)]
  if (is.character(dt$V2008)) dt[, V2008 := as.integer(V2008)]
  if (is.character(dt$V20081)) dt[, V20081 := as.integer(V20081)]
  if (is.character(dt$V20082)) dt[, V20082 := as.integer(V20082)]
  if (!is.numeric(dt$V2009)) dt[, V2009 := as.numeric(V2009)]

  # Handle special codes for unknown values
  # V2008 = 99: unknown birth day
  # V20081 = 99: unknown birth month
  # V20082 = 9999: unknown birth year
  dt[V2008 == 99L, V2008 := NA_integer_]
  dt[V20081 == 99L, V20081 := NA_integer_]
  dt[V20082 == 9999L, V20082 := NA_integer_]

  # Use default exception quarters if not provided
  if (is.null(exception_quarters)) {
    exception_quarters <- get_exception_quarters()
  }

  # Create quarter identifier for exception matching
  dt[, quarter_id := paste0(Ano, "t", Trimestre)]

  # Step 1: Pre-compute first valid Saturdays for each unique (year, quarter)
  # This is MUCH faster than computing per-row
  unique_quarters <- unique(dt[, .(Ano, Trimestre)])
  unique_quarters[, `:=`(
    month1 = quarter_month_n(Trimestre, 1L),
    month2 = quarter_month_n(Trimestre, 2L),
    month3 = quarter_month_n(Trimestre, 3L)
  )]

  # Calculate first valid Saturday for each month (standard rule: min_days=4)
  unique_quarters[, `:=`(
    first_sat_m1 = first_valid_saturday(Ano, month1, min_days = 4L),
    first_sat_m2 = first_valid_saturday(Ano, month2, min_days = 4L),
    first_sat_m3 = first_valid_saturday(Ano, month3, min_days = 4L)
  )]

  # Also calculate alternative (exception) rule: min_days=3
  unique_quarters[, `:=`(
    alt_sat_m1 = first_valid_saturday(Ano, month1, min_days = 3L),
    alt_sat_m2 = first_valid_saturday(Ano, month2, min_days = 3L),
    alt_sat_m3 = first_valid_saturday(Ano, month3, min_days = 3L)
  )]

  # Pre-compute date_min and date_max for each quarter (base values before birthday adjustment)
  unique_quarters[, `:=`(
    base_date_min = make_date(Ano, month1, first_sat_m1),
    base_date_max = make_date(Ano, month3, first_sat_m3) + 21L,
    alt_date_min = make_date(Ano, month1, alt_sat_m1),
    alt_date_max = make_date(Ano, month3, alt_sat_m3) + 21L
  )]

  # Merge pre-computed values back to main data
  dt <- merge(dt, unique_quarters, by = c("Ano", "Trimestre"), all.x = TRUE)

  # Step 2: Calculate birthday and first Saturday after birthday
  dt[, birthday := make_birthday(V20081, V2008, Ano)]
  dt[, first_sat_after_birthday := first_saturday_on_or_after(birthday)]

  # Determine if interview was before or after birthday
  # Only calculate for people with known birth year
  # visitapreaniv = (Ano - V20082) - V2009
  # = 0 if interview was after birthday (age matches year calculation)
  # = 1 if interview was before birthday (still younger by 1 year)
  dt[, visit_before_birthday := NA_integer_]
  dt[!is.na(V20082), visit_before_birthday := as.integer((Ano - V20082) - V2009)]

  # Initialize date bounds from base values
  dt[, `:=`(date_min = base_date_min, date_max = base_date_max)]

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

  # Step 3: Convert date bounds to month-in-quarter positions
  # Use separate functions for min and max since they have different edge-case handling
  dt[, month_min_pos := calculate_month_position_min(date_min, Ano, Trimestre)]
  dt[, month_max_pos := calculate_month_position_max(date_max, Ano, Trimestre)]

  # Step 5: Aggregate to UPA-panel level ACROSS ALL QUARTERS

  # PNADC is a rotating panel - same UPA-V1014 is interviewed in the same MONTH POSITION
  # (always month 1, 2, or 3) across all their quarterly visits.
  # So constraints from any quarter apply to all quarters for that UPA-V1014.
  # This is the key insight: aggregate by (UPA, V1014) only, NOT by (Ano, Trimestre, UPA, V1014)
  dt[, `:=`(
    upa_month_min = max(month_min_pos, na.rm = TRUE),
    upa_month_max = min(month_max_pos, na.rm = TRUE)
  ), by = .(UPA, V1014)]

  # Assign reference month: if min == max, that's the month; otherwise indeterminate
  # Use data.table's native assignment which is faster than ifelse
  dt[, ref_month_in_quarter := NA_integer_]
  dt[upa_month_min == upa_month_max & upa_month_min >= 1L & upa_month_max <= 3L,
     ref_month_in_quarter := upa_month_min]

  # Step 6: Handle exception quarters
  # Try relaxed rule (min_days = 3) for quarters that have issues
  dt <- apply_exception_rules(dt, exception_quarters)

  # Step 7: Calculate final reference month Date and YYYYMM
  # Use vectorized approach: calculate the actual month from quarter and position
  dt[!is.na(ref_month_in_quarter), `:=`(
    ref_month = make_date(Ano, quarter_month_n(Trimestre, ref_month_in_quarter), 1L),
    ref_month_yyyymm = yyyymm(Ano, quarter_month_n(Trimestre, ref_month_in_quarter))
  )]

  dt[is.na(ref_month_in_quarter), `:=`(
    ref_month = as.Date(NA),
    ref_month_yyyymm = NA_integer_
  )]

  # Select output columns
  # Include all available join keys
  key_cols <- intersect(join_key_vars(), names(dt))
  output_cols <- c(key_cols, "ref_month", "ref_month_in_quarter", "ref_month_yyyymm")

  result <- dt[, ..output_cols]

  # Add class for nice printing
  class(result) <- c("pnadc_crosswalk", class(result))
  attr(result, "determination_rate") <- mean(!is.na(result$ref_month_in_quarter))

  result
}

#' Calculate Month Position in Quarter (for date_min)
#'
#' Converts a date_min to its month position within the quarter (1, 2, or 3).
#' For date_min: if day <= 3 and not in first month of quarter, subtract 1.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param quarter Integer quarter vector (1-4)
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
calculate_month_position_min <- function(date, year, quarter) {
  # Get the first month of the quarter (vectorized)
  first_month <- quarter_first_month(quarter)

  # Extract month and day from date using fast functions
  date_month <- fast_month(date)
  date_day <- fast_mday(date)

  # Calculate position (1, 2, or 3)
  pos <- date_month - first_month + 1L

  # Stata: mesminnotrim = mesminnotrim - 1 if day(dataminentrev)<=3 & mofd(dataminentrev)>Trimestre*3-2
  # i.e., if day <= 3 and not in the first month of quarter, subtract 1
  # Use vectorized pmin/pmax instead of nested ifelse
  adjust <- (date_day <= 3L & date_month > first_month)
  pos <- pos - as.integer(adjust)

  # Clamp to valid range using pmin/pmax (faster than nested ifelse)
  pmin(pmax(pos, 1L), 3L)
}

#' Calculate Month Position in Quarter (for date_max)
#'
#' Converts a date_max to its month position within the quarter (1, 2, or 3).
#' For date_max: if day <= 3, use the month of (date - 3 days) instead.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param quarter Integer quarter vector (1-4)
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
calculate_month_position_max <- function(date, year, quarter) {
  # Get the first month of the quarter (vectorized)
  first_month <- quarter_first_month(quarter)

  # Extract month and day from date using fast functions
  date_day <- fast_mday(date)

  # Stata: mesmaxnotrim = mofd(datamaxentrev-3) if day(datamaxentrev)<=3
  # When day <= 3, use the month of (date - 3 days)
  # Avoid creating intermediate Date objects - just adjust the date value directly
  needs_adjust <- date_day <= 3L
  adjusted_date <- date - needs_adjust * 3L
  adjusted_month <- fast_month(adjusted_date)

  # Calculate position (1, 2, or 3)
  pos <- adjusted_month - first_month + 1L

  # Clamp to valid range using pmin/pmax (faster than nested ifelse)
  pmin(pmax(pos, 1L), 3L)
}

#' Calculate Month Position in Quarter for Exception Rules (for date_min)
#'
#' Same as calculate_month_position_min but uses day <= 2 threshold instead of <= 3.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param quarter Integer quarter vector (1-4)
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
calculate_month_position_min_exc <- function(date, year, quarter) {
  first_month <- quarter_first_month(quarter)
  date_month <- fast_month(date)
  date_day <- fast_mday(date)
  pos <- date_month - first_month + 1L
  # For exception quarters, threshold is 2 instead of 3
  adjust <- (date_day <= 2L & date_month > first_month)
  pos <- pos - as.integer(adjust)
  pmin(pmax(pos, 1L), 3L)
}

#' Calculate Month Position in Quarter for Exception Rules (for date_max)
#'
#' Same as calculate_month_position_max but uses day <= 2 threshold instead of <= 3.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param quarter Integer quarter vector (1-4)
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
calculate_month_position_max_exc <- function(date, year, quarter) {
  first_month <- quarter_first_month(quarter)
  date_day <- fast_mday(date)
  # For exception quarters, threshold is 2 instead of 3
  needs_adjust <- date_day <= 2L
  adjusted_date <- date - needs_adjust * 3L
  adjusted_month <- fast_month(adjusted_date)
  pos <- adjusted_month - first_month + 1L
  pmin(pmax(pos, 1L), 3L)
}

#' Apply Exception Rules for Specific Quarters
#'
#' For quarters where the standard rule produces impossible results, apply
#' relaxed timing rules (min_days = 3 instead of 4).
#'
#' Uses pre-computed alternative Saturday values (alt_sat_m1/2/3, alt_date_min/max)
#' that were already merged into the main data table.
#'
#' @param dt data.table with standard rule results and pre-computed alt_* columns
#' @param exception_quarters Character vector of exception quarters ("YYYYtQ")
#' @return data.table with exception rules applied
#' @keywords internal
#' @noRd
apply_exception_rules <- function(dt, exception_quarters) {

  # Identify observations in exception quarters that are currently indeterminate
  in_exception <- dt$quarter_id %in% exception_quarters
  currently_na <- is.na(dt$ref_month_in_quarter)
  needs_exception <- in_exception & currently_na

  if (!any(needs_exception)) {
    return(dt)
  }

  # For these observations, use pre-computed relaxed rule values
  exc_dt <- dt[needs_exception]

  # Use pre-computed alternative date bounds (already in exc_dt from merge)
  exc_dt[, date_min_exc := alt_date_min]
  exc_dt[, date_max_exc := alt_date_max]

  # Apply birthday constraints (same logic as before)
  exc_dt[visit_before_birthday == 0L &
           !is.na(first_sat_after_birthday) &
           first_sat_after_birthday > date_min_exc &
           first_sat_after_birthday <= date_max_exc,
         date_min_exc := first_sat_after_birthday]

  exc_dt[visit_before_birthday == 1L &
           !is.na(first_sat_after_birthday) &
           (first_sat_after_birthday - 7L) < date_max_exc &
           (first_sat_after_birthday - 7L) >= date_min_exc,
         date_max_exc := first_sat_after_birthday - 7L]

  # Calculate positions with exception rule
  # Note: for exception quarters, the day threshold changes from 3 to 2
  # This is handled by the exception-specific position functions
  exc_dt[, month_min_pos_exc := calculate_month_position_min_exc(date_min_exc, Ano, Trimestre)]
  exc_dt[, month_max_pos_exc := calculate_month_position_max_exc(date_max_exc, Ano, Trimestre)]

  # Aggregate to UPA-panel level ACROSS ALL QUARTERS
  exc_dt[, `:=`(
    upa_month_min_exc = max(month_min_pos_exc, na.rm = TRUE),
    upa_month_max_exc = min(month_max_pos_exc, na.rm = TRUE)
  ), by = .(UPA, V1014)]

  # Determine if exception rule resolves the issue
  # Use data.table's native assignment which is faster than ifelse
  exc_dt[, ref_month_in_quarter_exc := NA_integer_]
  exc_dt[upa_month_min_exc == upa_month_max_exc &
           upa_month_min_exc >= 1L & upa_month_max_exc <= 3L,
         ref_month_in_quarter_exc := upa_month_min_exc]

  # Update original dt where exception rule works
  resolved <- !is.na(exc_dt$ref_month_in_quarter_exc)

  if (any(resolved)) {
    # Get the row indices in original dt that need updating
    # We need a way to match back - use a temporary ID
    dt[, .row_id := .I]
    exc_dt[, .row_id := dt[needs_exception, .row_id]]

    # Update only rows where exception resolved the issue
    resolved_rows <- exc_dt[resolved, .row_id]
    resolved_values <- exc_dt[resolved, ref_month_in_quarter_exc]

    dt[.row_id %in% resolved_rows,
       ref_month_in_quarter := resolved_values[match(.row_id, resolved_rows)]]

    dt[, .row_id := NULL]
  }

  dt
}

#' Get Exception Quarters
#'
#' Returns the list of quarters where IBGE used non-standard technical break timing.
#' In these quarters, the first reference week of a month only needs 3 days
#' (instead of the usual 4) within that month.
#'
#' @return Character vector of exception quarters in "YYYYtQ" format
#'
#' @details
#' The exception quarters are documented in IBGE's methodology notes:
#' \itemize{
#'   \item 2016t3: September 25 - October 1 technical break
#'   \item 2016t4: December 25-31 technical break
#'   \item 2017t2: June 25 - July 1 technical break
#'   \item 2022t3: September 25 - October 1 technical break
#'   \item 2023t2: June 25 - July 1 technical break
#' }
#'
#' @examples
#' get_exception_quarters()
#' # [1] "2016t3" "2016t4" "2017t2" "2022t3" "2023t2"
#'
#' @export
get_exception_quarters <- function() {
  c("2016t3", "2016t4", "2017t2", "2022t3", "2023t2")
}

#' Print Method for pnadc_crosswalk
#'
#' @param x A pnadc_crosswalk object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns x
#' @export
print.pnadc_crosswalk <- function(x, ...) {
  cat("PNADC Reference Month Crosswalk\n")
  cat("-------------------------------\n")
  cat("Observations:", format(nrow(x), big.mark = ","), "\n")

  det_rate <- attr(x, "determination_rate")
  if (!is.null(det_rate)) {
    cat("Determination rate:", sprintf("%.1f%%", det_rate * 100), "\n")
  }

  if (nrow(x) > 0 && "ref_month_yyyymm" %in% names(x)) {
    date_range <- range(x$ref_month_yyyymm, na.rm = TRUE)
    if (!any(is.na(date_range))) {
      cat("Date range:", date_range[1], "-", date_range[2], "\n")
    }
  }

  cat("\nJoin keys:", paste(intersect(join_key_vars(), names(x)), collapse = ", "), "\n")
  cat("Output columns:", paste(setdiff(names(x), join_key_vars()), collapse = ", "), "\n")

  invisible(x)
}
