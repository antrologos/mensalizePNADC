#' Identify Reference Fortnight in PNADC Data
#'
#' Determines which IBGE fortnight (quinzena) within each quarter corresponds
#' to each survey observation based on IBGE's interview timing rules.
#'
#' @description
#' PNADC is a quarterly survey where each interview occurs during a specific
#' fortnight within the quarter. This function identifies which IBGE fortnight
#' (1-6 per quarter, 1-24 per year) that observation belongs to, enabling
#' bi-weekly time series analysis.
#'
#' The algorithm uses:
#' \itemize{
#'   \item IBGE's reference week timing rules (first Saturday with sufficient days in month)
#'   \item IBGE week boundaries (Sunday-Saturday, not calendar days)
#'   \item Respondent birthdates to constrain possible interview dates
#'   \item Household-level aggregation within quarter (all persons in same household interviewed on same day)
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
#'     \item \code{ref_fortnight_start}: Reference fortnight start (Sunday of first IBGE week)
#'     \item \code{ref_fortnight_end}: Reference fortnight end (Saturday of last IBGE week)
#'     \item \code{ref_fortnight_in_quarter}: Position in quarter (1-6) or NA
#'     \item \code{ref_fortnight_yyyyff}: Integer YYYYFF format (1-24 per year)
#'     \item \code{ref_fortnight_weeks}: Number of IBGE weeks in this fortnight (always 2)
#'   }
#'
#' @details
#' ## IBGE Fortnight Definition
#'
#' IBGE fortnights are based on IBGE reference weeks, NOT calendar days:
#' \itemize{
#'   \item Fortnight 1 of a month = IBGE weeks 1-2 of that month (14 days)
#'   \item Fortnight 2 of a month = IBGE weeks 3-4 of that month (14 days)
#' }
#'
#' Each quarter has 6 fortnights (2 per month). Each fortnight has exactly 2 weeks.
#'
#' ## Expected Determination Rate
#'
#' Fortnight determination rate is typically low (~6-8%) because:
#' \itemize{
#'   \item Unlike months, fortnights cannot aggregate across quarters
#'   \item Birthday constraints typically narrow the date range by only a few weeks
#'   \item An IBGE fortnight (2 weeks) is difficult to pinpoint without cross-quarter data
#' }
#'
#' The algorithm determines fortnight when the interview date range falls
#' entirely within a single IBGE fortnight (weeks 1-2 or weeks 3+).
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
#' # Note: Fortnight determination rate is typically low (~6-8%)
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

  # Calculate the last Saturday of the quarter using IBGE month end
  # (IBGE months always have exactly 4 reference weeks)
  dt[, quarter_end := ibge_month_end(Ano, month3, min_days = 4L)]

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

  dt[, birthday := NULL]

  update_pb(3)

  # ============================================================================
  # STEP 4: Convert date bounds to IBGE fortnight positions
  # ============================================================================

  # Calculate IBGE fortnight position within quarter (1-6) using IBGE weeks
  dt[, `:=`(
    fortnight_min_pos = ibge_fortnight_in_quarter(date_min, Trimestre, Ano, min_days = 4L),
    fortnight_max_pos = ibge_fortnight_in_quarter(date_max, Trimestre, Ano, min_days = 4L)
  )]

  # Calculate alternative bounds using EXCEPTION rule (min_days=3)
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

  # Convert alternative bounds to IBGE fortnight positions
  dt[, `:=`(
    alt_fortnight_min_pos = ibge_fortnight_in_quarter(alt_date_min, Trimestre, Ano, min_days = 3L),
    alt_fortnight_max_pos = ibge_fortnight_in_quarter(alt_date_max, Trimestre, Ano, min_days = 3L)
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
         "alt_fortnight_min_pos", "alt_fortnight_max_pos",
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
       !is.na(hh_fortnight_min) & hh_fortnight_min >= 1L & hh_fortnight_max <= 6L,
     ref_fortnight_in_quarter := hh_fortnight_min]

  # Calculate YYYYFF (1-24 per year) using IBGE format
  dt[, ref_fortnight_yyyyff := NA_integer_]
  dt[!is.na(ref_fortnight_in_quarter), `:=`(
    ref_fortnight_yyyyff = ibge_fortnight_in_quarter_to_yyyyff(
      Ano, Trimestre, ref_fortnight_in_quarter
    )
  )]

  # Calculate fortnight start/end dates (Sunday/Saturday of IBGE weeks)
  # First, derive month and fortnight within month from fortnight_in_quarter
  dt[!is.na(ref_fortnight_in_quarter), `:=`(
    temp_month_in_q = ((ref_fortnight_in_quarter - 1L) %/% 2L) + 1L,
    temp_fortnight_in_month = ((ref_fortnight_in_quarter - 1L) %% 2L) + 1L
  )]
  dt[!is.na(ref_fortnight_in_quarter), `:=`(
    temp_month = quarter_month_n(Trimestre, temp_month_in_q)
  )]

  # Get start (Sunday) and end (Saturday) dates
  dt[, `:=`(ref_fortnight_start = as.Date(NA), ref_fortnight_end = as.Date(NA))]
  dt[!is.na(ref_fortnight_in_quarter), `:=`(
    ref_fortnight_start = ibge_fortnight_start(Ano, temp_month, temp_fortnight_in_month, min_days = 4L),
    ref_fortnight_end = ibge_fortnight_end(Ano, temp_month, temp_fortnight_in_month, min_days = 4L)
  )]

  # IBGE fortnights always have exactly 2 weeks
  dt[, ref_fortnight_weeks := NA_integer_]
  dt[!is.na(ref_fortnight_in_quarter), ref_fortnight_weeks := 2L]

  # Clean up intermediate columns
  temp_cols <- c(
    "month1", "month2", "month3",
    "first_sat_m1", "first_sat_m2", "first_sat_m3",
    "alt_sat_m1", "alt_sat_m2", "alt_sat_m3",
    "first_sat_after_birthday", "visit_before_birthday",
    "date_min", "date_max", "quarter_end",
    "fortnight_min_pos", "fortnight_max_pos",
    "hh_fortnight_min", "hh_fortnight_max",
    "temp_month_in_q", "temp_fortnight_in_month", "temp_month"
  )
  dt[, (intersect(temp_cols, names(dt))) := NULL]
  # OPTIMIZATION: Removed explicit gc() call - R's garbage collector runs automatically

  # Select output columns - include quarter/household keys for within-quarter aggregation
  key_cols <- intersect(c("Ano", "Trimestre", "UPA", "V1008", "V1014"), names(dt))
  output_cols <- c(key_cols, "ref_fortnight_start", "ref_fortnight_end",
                   "ref_fortnight_in_quarter", "ref_fortnight_yyyyff", "ref_fortnight_weeks")

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

# NOTE: The helper functions for IBGE fortnight calculations (date_to_fortnight_in_quarter,
# ibge_fortnight_in_quarter, ibge_fortnight_start, ibge_fortnight_end, etc.) are now
# located in utils-dates.R as part of the IBGE Calendar Utilities section.
