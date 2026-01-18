#' Identify Reference Fortnight in PNADC Data
#'
#' Determines which fortnight (quinzena - 15-day period) within each quarter
#' corresponds to each survey observation based on IBGE's interview timing rules.
#'
#' @description
#' PNADC is a quarterly survey where each interview occurs during a specific
#' fortnight within the quarter. This function identifies which fortnight (1-6
#' per quarter, 1-24 per year) that observation belongs to, enabling bi-weekly
#' time series analysis.
#'
#' The algorithm uses:
#' \itemize{
#'   \item IBGE's reference week timing rules (first Saturday with sufficient days in month)
#'   \item Respondent birthdates to constrain possible interview dates
#'   \item UPA-panel level aggregation (everyone in same sampling unit interviewed together)
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
#' @param verbose Logical. If TRUE (default), display progress information.
#' @param .pb Optional progress bar object.
#' @param .pb_offset Integer. Offset for progress bar updates.
#'
#' @return A data.table with the original key columns plus:
#'   \itemize{
#'     \item \code{ref_fortnight}: Reference fortnight as Date (1st or 16th of month)
#'     \item \code{ref_fortnight_in_quarter}: Position in quarter (1-6) or NA
#'     \item \code{ref_fortnight_yyyyff}: Integer YYYYFF format (1-24 per year)
#'   }
#'
#' @details
#' ## Fortnight Definition
#'
#' Each year has 24 fortnights (2 per month):
#' \itemize{
#'   \item Fortnight 01: Jan 1-15
#'   \item Fortnight 02: Jan 16-31
#'   \item Fortnight 03: Feb 1-15
#'   \item Fortnight 04: Feb 16-28/29
#'   \item ... and so on through fortnight 24 (Dec 16-31)
#' }
#'
#' Each quarter has 6 fortnights (e.g., Q1 = fortnights 01-06).
#'
#' ## Expected Determination Rate
#'
#' Fortnight determination rate is typically low (~2-5%) because:
#' \itemize{
#'   \item Unlike months, fortnights cannot aggregate across quarters
#'   \item Birthday constraints typically narrow the date range by only a few weeks
#'   \item A 15-day fortnight window is difficult to pinpoint without cross-quarter data
#' }
#'
#' The algorithm determines fortnight when the interview date range falls
#' entirely within a single 15-day period.
#'
#' \strong{Note}: The month algorithm achieves ~97% determination by aggregating
#' at UPA-V1014 level across ALL quarters (leveraging the panel design where
#' the same units are interviewed in consistent relative positions). Fortnights
#' and weeks cannot use this cross-quarter aggregation because their positions
#' are not consistent across panel visits.
#'
#' @seealso \code{\link{pnadc_identify_periods}} for the main function that
#'   builds a complete crosswalk including months, fortnights, and weeks.
#'   \code{\link{identify_reference_month}} for month-only identification.
#'   \code{\link{identify_reference_week}} for week-only identification.
#'
#' @examples
#' \dontrun{
#' # Identify reference fortnights
#' result <- identify_reference_fortnight(pnadc_data)
#'
#' # Check determination rate by quarter
#' result[, .(
#'   total = .N,
#'   determined = sum(!is.na(ref_fortnight_in_quarter)),
#'   rate = mean(!is.na(ref_fortnight_in_quarter))
#' ), by = .(Ano, Trimestre)]
#'
#' # Note: Fortnight determination rate is typically low (~2-5%)
#' # because fortnights cannot aggregate across quarters like months can.
#' }
#'
#' @export
identify_reference_fortnight <- function(data, verbose = TRUE, .pb = NULL, .pb_offset = 0L) {

  # Initialize progress bar (7 steps total)
  use_external_pb <- !is.null(.pb)
  if (verbose && !use_external_pb) {
    cat("Identifying reference fortnights...\n")
    pb <- txtProgressBar(min = 0, max = 7, style = 3)
  } else if (use_external_pb) {
    pb <- .pb
  }

  update_pb <- function(step) {
    if (verbose || use_external_pb) {
      setTxtProgressBar(pb, step + .pb_offset)
    }
  }

  # Convert to data.table (copy to avoid modifying original)
  dt <- ensure_data_table(data, copy = TRUE)

  # Batch convert character columns
  int_cols <- c("Ano", "Trimestre", "V2008", "V20081", "V20082")
  for (col in int_cols) {
    if (is.character(dt[[col]])) {
      data.table::set(dt, j = col, value = as.integer(dt[[col]]))
    }
  }
  if (!is.numeric(dt$V2009)) {
    data.table::set(dt, j = "V2009", value = as.numeric(dt$V2009))
  }

  # Handle special codes
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

  # Join to main data
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

  dt[, visit_before_birthday := NA_integer_]
  dt[!is.na(V20082), visit_before_birthday := as.integer((Ano - V20082) - V2009)]

  update_pb(2)

  # ============================================================================
  # STEP 3: Calculate date bounds using STANDARD rules
  # ============================================================================

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

  dt[, birthday := NULL]

  update_pb(3)

  # ============================================================================
  # STEP 4: Convert date bounds to fortnight positions
  # ============================================================================

  # Calculate fortnight position within quarter (1-6)
  dt[, `:=`(
    fortnight_min_pos = date_to_fortnight_in_quarter(date_min, Trimestre),
    fortnight_max_pos = date_to_fortnight_in_quarter(date_max, Trimestre),
    fortnight_min_yyyyff = date_to_yyyyff(date_min),
    fortnight_max_yyyyff = date_to_yyyyff(date_max)
  )]

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

  # Convert alternative bounds to fortnight positions
  dt[, `:=`(
    alt_fortnight_min_pos = date_to_fortnight_in_quarter(alt_date_min, Trimestre),
    alt_fortnight_max_pos = date_to_fortnight_in_quarter(alt_date_max, Trimestre)
  )]

  update_pb(4)

  # ============================================================================
  # STEP 5: Aggregate to household level WITHIN QUARTER
  # Unlike months (which have consistent position across panel visits),
  # fortnights do NOT aggregate across quarters - only within quarter.
  # ============================================================================

  data.table::setkey(dt, Ano, Trimestre, UPA, V1008)

  dt[, `:=`(
    hh_fortnight_min = max(fortnight_min_pos, na.rm = TRUE),
    hh_fortnight_max = min(fortnight_max_pos, na.rm = TRUE),
    alt_hh_fortnight_min = max(alt_fortnight_min_pos, na.rm = TRUE),
    alt_hh_fortnight_max = min(alt_fortnight_max_pos, na.rm = TRUE)
  ), by = .(Ano, Trimestre, UPA, V1008)]

  # Handle infinite values from all-NA groups (max returns -Inf, min returns Inf)
  dt[is.infinite(hh_fortnight_min), hh_fortnight_min := NA_integer_]
  dt[is.infinite(hh_fortnight_max), hh_fortnight_max := NA_integer_]
  dt[is.infinite(alt_hh_fortnight_min), alt_hh_fortnight_min := NA_integer_]
  dt[is.infinite(alt_hh_fortnight_max), alt_hh_fortnight_max := NA_integer_]

  # Clean up alternative date columns
  dt[, c("alt_date_min", "alt_date_max",
         "alt_fortnight_min_pos", "alt_fortnight_max_pos") := NULL]

  update_pb(5)

  # ============================================================================
  # STEP 6: Dynamic exception detection (fallback to min_days=3 rule)
  # ============================================================================
  #
  # IBGE's "Parada Tecnica" defines two rules for first valid Saturday:
  #   - Standard rule: First Saturday with >= 4 days in the month
  #   - Exception rule: First Saturday with >= 3 days in the month
  #
  # The exception rule is a fallback used when standard rules produce an
  # impossible result (hh_fortnight_min > hh_fortnight_max after birthday
  # constraints are applied). This can happen in edge cases where the
  # standard Saturday selection creates contradictory date bounds.
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
    hh_fortnight_min > hh_fortnight_max &
    alt_hh_fortnight_min <= alt_hh_fortnight_max
  )]

  # Propagate exception to entire quarter for consistency
  dt[, trim_has_exception := as.integer(sum(requires_exception, na.rm = TRUE) > 0L),
     by = .(Ano, Trimestre)]

  # Apply exception rules (fallback to min_days=3) where needed
  exc_condition <- (dt$trim_has_exception == 1L)
  has_any_exception <- any(exc_condition)

  if (has_any_exception) {
    dt[exc_condition, `:=`(
      hh_fortnight_min = alt_hh_fortnight_min,
      hh_fortnight_max = alt_hh_fortnight_max
    )]
  }

  # Clean up
  dt[, c("requires_exception", "trim_has_exception",
         "alt_hh_fortnight_min", "alt_hh_fortnight_max") := NULL]

  update_pb(6)

  # ============================================================================
  # STEP 7: Assign reference fortnight and select output columns
  # ============================================================================

  # Assign if min == max
  dt[, ref_fortnight_in_quarter := NA_integer_]
  dt[hh_fortnight_min == hh_fortnight_max &
       hh_fortnight_min >= 1L & hh_fortnight_max <= 6L,
     ref_fortnight_in_quarter := hh_fortnight_min]

  # Calculate YYYYFF (1-24 per year)
  dt[, ref_fortnight_yyyyff := NA_integer_]
  dt[!is.na(ref_fortnight_in_quarter), `:=`(
    ref_fortnight_yyyyff = fortnight_in_quarter_to_yyyyff(
      Ano, Trimestre, ref_fortnight_in_quarter
    )
  )]

  # Calculate ref_fortnight Date (1st or 16th of month)
  dt[, ref_fortnight := as.Date(NA)]
  dt[!is.na(ref_fortnight_yyyyff), ref_fortnight := yyyyff_to_date(ref_fortnight_yyyyff)]

  # Clean up intermediate columns
  temp_cols <- c(
    "month1", "month2", "month3",
    "first_sat_m1", "first_sat_m2", "first_sat_m3",
    "alt_sat_m1", "alt_sat_m2", "alt_sat_m3",
    "first_sat_after_birthday", "visit_before_birthday",
    "date_min", "date_max",
    "fortnight_min_pos", "fortnight_max_pos",
    "fortnight_min_yyyyff", "fortnight_max_yyyyff",
    "hh_fortnight_min", "hh_fortnight_max"
  )
  dt[, (intersect(temp_cols, names(dt))) := NULL]
  gc()

  # Select output columns - include quarter/household keys for within-quarter aggregation
  key_cols <- intersect(c("Ano", "Trimestre", "UPA", "V1008", "V1014"), names(dt))
  output_cols <- c(key_cols, "ref_fortnight", "ref_fortnight_in_quarter", "ref_fortnight_yyyyff")

  result <- unique(dt[, ..output_cols])

  # Store determination rate
  attr(result, "determination_rate") <- mean(!is.na(result$ref_fortnight_in_quarter))

  update_pb(7)

  if (verbose && !use_external_pb) {
    close(pb)
    cat(sprintf("  Determination rate: %.1f%%\n", attr(result, "determination_rate") * 100))
  }

  result
}


#' Convert Date to Fortnight Position in Quarter
#'
#' @param date Date vector
#' @param quarter Integer quarter vector (1-4)
#' @return Integer fortnight position (1-6) within the quarter
#' @keywords internal
#' @noRd
date_to_fortnight_in_quarter <- function(date, quarter) {
  month <- fast_month(date)
  day <- fast_mday(date)

  # First and last month of quarter
  first_month <- quarter_first_month(quarter)
  last_month <- first_month + 2L

  # Check if date is within the quarter
  in_quarter <- month >= first_month & month <= last_month

  # Month position in quarter (1, 2, or 3)
  month_in_quarter <- month - first_month + 1L

  # Fortnight within month (1 if day 1-15, 2 if day 16+)
  fortnight_in_month <- fifelse(day <= 15L, 1L, 2L)

  # Position in quarter (1-6)
  pos <- (month_in_quarter - 1L) * 2L + fortnight_in_month

  # Return NA for dates outside the quarter
  fifelse(in_quarter, pos, NA_integer_)
}


#' Convert Date to YYYYFF Format
#'
#' @param date Date vector
#' @return Integer YYYYFF (e.g., 202301 for first fortnight of Jan 2023)
#' @keywords internal
#' @noRd
date_to_yyyyff <- function(date) {
  year <- fast_year(date)
  month <- fast_month(date)
  day <- fast_mday(date)

  # Fortnight number in year (1-24)
  ff <- (month - 1L) * 2L + fifelse(day <= 15L, 1L, 2L)

  year * 100L + ff
}


#' Convert Fortnight Position in Quarter to YYYYFF
#'
#' @param year Integer year
#' @param quarter Integer quarter (1-4)
#' @param pos Integer position in quarter (1-6)
#' @return Integer YYYYFF
#' @keywords internal
#' @noRd
fortnight_in_quarter_to_yyyyff <- function(year, quarter, pos) {
  # First fortnight of quarter
  first_ff <- (quarter - 1L) * 6L + 1L

  # Fortnight number in year (1-24)
  ff <- first_ff + pos - 1L

  year * 100L + ff
}


#' Convert YYYYFF to Date
#'
#' @param yyyyff Integer YYYYFF format
#' @return Date (1st or 16th of month)
#' @keywords internal
#' @noRd
yyyyff_to_date <- function(yyyyff) {
  year <- yyyyff %/% 100L
  ff <- yyyyff %% 100L

  # Month (1-12)
  month <- (ff - 1L) %/% 2L + 1L

  # Day (1 or 16)
  day <- fifelse(ff %% 2L == 1L, 1L, 16L)

  make_date(year, month, day)
}
