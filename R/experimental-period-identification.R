#' Experimental Period Identification Strategies
#'
#' Provides experimental strategies for improving period identification rates
#' beyond the standard deterministic algorithm. All strategies respect the nested
#' identification hierarchy: weeks require fortnights, fortnights require months.
#'
#' @description
#' Three experimental strategies are available, all properly nested by period:
#' \itemize{
#'   \item \strong{probabilistic}: For narrow ranges (2 possible periods),
#'     classifies based on where most of the date interval falls. Assigns only
#'     when confidence exceeds threshold.
#'   \item \strong{upa_aggregation}: Extends strictly identified periods to other
#'     observations in the same UPA-V1014 within the quarter, if a sufficient
#'     proportion already have strict identification.
#'   \item \strong{both}: Combines both strategies - applies probabilistic rule
#'     at UPA-V1014 level when sufficient proportion have narrow ranges.
#' }
#'
#' @param crosswalk A crosswalk data.table from \code{pnadc_identify_periods()}
#' @param data The original PNADC data.table used to build the crosswalk
#'   (required for probabilistic strategy to access date bounds)
#' @param strategy Character specifying which strategy to apply.
#'   Options: "none", "probabilistic", "upa_aggregation", "both"
#' @param confidence_threshold Numeric (0-1). Minimum confidence required to
#'   assign a probabilistic period. Used by probabilistic and combined strategies.
#'   Default 0.9.
#' @param upa_proportion_threshold Numeric (0-1). Minimum proportion of UPA-V1014
#'   observations (within quarter) that must meet criteria for UPA-level assignment.
#'   Used by upa_aggregation and combined strategies. Default 0.9.
#' @param verbose Logical. If TRUE, print progress information.
#'
#' @return A modified crosswalk with additional columns:
#'   \itemize{
#'     \item \code{ref_month_exp}: Experimentally assigned month (NA if not assigned)
#'     \item \code{ref_month_exp_confidence}: Confidence of month assignment (0.5-1.0)
#'     \item \code{ref_fortnight_exp}: Experimentally assigned fortnight
#'     \item \code{ref_fortnight_exp_confidence}: Confidence of fortnight assignment
#'     \item \code{ref_week_exp}: Experimentally assigned week
#'     \item \code{ref_week_exp_confidence}: Confidence of week assignment
#'   }
#'
#' @details
#' ## Nesting Enforcement
#'
#' All strategies enforce proper nesting:
#' \itemize{
#'   \item Fortnights can only be assigned if month is identified (strictly OR experimentally)
#'   \item Weeks can only be assigned if fortnight is identified (strictly OR experimentally)
#' }
#'
#' ## Probabilistic Strategy
#'
#' For each period type (processed in order: months, then fortnights, then weeks):
#' \enumerate{
#'   \item Check that the required parent period is identified
#'   \item If bounds are narrowed to exactly 2 sequential periods, calculate
#'     which period contains most of the date interval
#'   \item Calculate confidence based on the proportion of interval in the likely period
#'   \item Only assign if confidence >= \code{confidence_threshold}
#' }
#'
#' For months: aggregates at UPA-V1014 level (but within-quarter, unlike strict algorithm)
#' For fortnights and weeks: works at household level
#'
#' ## UPA Aggregation Strategy
#'
#' Extends strictly identified periods to all observations in the same UPA-V1014
#' within the quarter:
#' \enumerate{
#'   \item Calculate proportion of UPA-V1014 observations with strictly identified period
#'   \item If proportion >= \code{upa_proportion_threshold}, extend to all observations
#'   \item Apply in nested order: months first, then fortnights, then weeks
#' }
#'
#' ## Combined Strategy ("both")
#'
#' Applies probabilistic rule at UPA-V1014 level when sufficient observations
#' have narrow ranges:
#' \enumerate{
#'   \item For each UPA-V1014 within quarter, check if proportion >= threshold
#'     have bounds narrowed to 2 sequential periods
#'   \item If so, apply probabilistic classification at UPA-V1014 level
#'   \item Process in nested order: months, then fortnights, then weeks
#' }
#'
#' @note
#' These strategies produce "experimental" assignments, not strict determinations.
#' The standard \code{pnadc_identify_periods()} function should be used for
#' rigorous analysis. Experimental outputs are useful for:
#' \itemize{
#'   \item Sensitivity analysis
#'   \item Robustness checks
#'   \item Research into identification algorithm improvements
#' }
#'
#' @seealso \code{\link{pnadc_identify_periods}} to build the crosswalk that
#'   this function modifies.
#'
#' @examples
#' \dontrun{
#' # Build standard crosswalk
#' crosswalk <- pnadc_identify_periods(pnadc_data)
#'
#' # Apply experimental strategies
#' crosswalk_exp <- pnadc_experimental_periods(
#'   crosswalk,
#'   pnadc_data,
#'   strategy = "probabilistic",
#'   confidence_threshold = 0.9
#' )
#'
#' # Check how many additional assignments we get
#' crosswalk_exp[, .(
#'   strict = sum(!is.na(ref_month_in_quarter)),
#'   experimental = sum(!is.na(ref_month_exp)),
#'   total = sum(!is.na(ref_month_in_quarter) | !is.na(ref_month_exp))
#' )]
#' }
#'
#' @export
pnadc_experimental_periods <- function(
    crosswalk,
    data = NULL,
    strategy = c("none", "probabilistic", "upa_aggregation", "both"),
    confidence_threshold = 0.9,
    upa_proportion_threshold = 0.9,
    verbose = TRUE
) {

  strategy <- match.arg(strategy)

  # Validate thresholds
  checkmate::assert_number(confidence_threshold, lower = 0, upper = 1)
  checkmate::assert_number(upa_proportion_threshold, lower = 0, upper = 1)

  if (strategy == "none") {
    if (verbose) cat("No experimental strategy applied.\n")
    return(crosswalk)
  }

  # Copy crosswalk to avoid modifying original
  result <- data.table::copy(crosswalk)

  # Initialize experimental columns
  result[, `:=`(
    ref_month_exp = NA_integer_,
    ref_month_exp_confidence = NA_real_,
    ref_fortnight_exp = NA_integer_,
    ref_fortnight_exp_confidence = NA_real_,
    ref_week_exp = NA_integer_,
    ref_week_exp_confidence = NA_real_
  )]

  # ==========================================================================
  # STRATEGY DISPATCH
  # ==========================================================================

  if (strategy == "probabilistic") {
    if (is.null(data)) {
      warning("Probabilistic strategy requires original data to compute date bounds. ",
              "Skipping.")
      return(result)
    }
    result <- apply_probabilistic_strategy_nested(
      result, data, confidence_threshold, verbose
    )
  } else if (strategy == "upa_aggregation") {
    result <- apply_upa_aggregation_strategy_nested(
      result, upa_proportion_threshold, verbose
    )
  } else if (strategy == "both") {
    if (is.null(data)) {
      warning("Combined strategy requires original data to compute date bounds. ",
              "Skipping.")
      return(result)
    }
    result <- apply_combined_strategy_nested(
      result, data, confidence_threshold, upa_proportion_threshold, verbose
    )
  }

  result
}


# =============================================================================
# PROBABILISTIC STRATEGY (Nested)
# =============================================================================

#' Apply Probabilistic Strategy with Proper Nesting
#'
#' @param crosswalk Crosswalk data.table
#' @param data Original PNADC data
#' @param confidence_threshold Minimum confidence to assign
#' @param verbose Print progress
#' @return Modified crosswalk with probabilistic columns
#' @keywords internal
#' @noRd
apply_probabilistic_strategy_nested <- function(crosswalk, data, confidence_threshold, verbose) {

  if (verbose) cat("Applying probabilistic strategy (nested)...\n")

  # ==========================================================================
  # STEP 1: Calculate date bounds from original data
  # ==========================================================================

  if (verbose) cat("  Calculating date bounds...\n")

  dt <- ensure_data_table(data, copy = TRUE)

  # Basic preprocessing
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

  # Calculate quarter dates
  unique_quarters <- unique(dt[, .(Ano, Trimestre)])
  unique_quarters[, `:=`(
    month1 = quarter_month_n(Trimestre, 1L),
    month2 = quarter_month_n(Trimestre, 2L),
    month3 = quarter_month_n(Trimestre, 3L)
  )]
  unique_quarters[, `:=`(
    first_sat_m1 = first_valid_saturday(Ano, month1, min_days = 4L),
    first_sat_m2 = first_valid_saturday(Ano, month2, min_days = 4L),
    first_sat_m3 = first_valid_saturday(Ano, month3, min_days = 4L)
  )]

  dt[unique_quarters, on = .(Ano, Trimestre), `:=`(
    month1 = i.month1, month2 = i.month2, month3 = i.month3,
    first_sat_m1 = i.first_sat_m1, first_sat_m2 = i.first_sat_m2, first_sat_m3 = i.first_sat_m3
  )]

  # Calculate birthday constraints
  dt[, birthday := make_birthday(V20081, V2008, Ano)]
  dt[, first_sat_after_birthday := first_saturday_on_or_after(birthday)]
  dt[, visit_before_birthday := NA_integer_]
  dt[!is.na(V20082), visit_before_birthday := as.integer((Ano - V20082) - V2009)]

  # Date bounds
  dt[, `:=`(
    date_min = make_date(Ano, month1, first_sat_m1),
    date_max = make_date(Ano, month3, first_sat_m3) + 21L
  )]

  # Apply birthday constraints
  dt[visit_before_birthday == 0L & !is.na(first_sat_after_birthday) &
       first_sat_after_birthday > date_min & first_sat_after_birthday <= date_max,
     date_min := first_sat_after_birthday]

  dt[visit_before_birthday == 1L & !is.na(first_sat_after_birthday) &
       (first_sat_after_birthday - 7L) < date_max & (first_sat_after_birthday - 7L) >= date_min,
     date_max := first_sat_after_birthday - 7L]

  # ==========================================================================
  # STEP 2: MONTH PROBABILISTIC (UPA-V1014 level, within-quarter)
  # ==========================================================================

  if (verbose) cat("  Phase 1: Month probabilistic identification...\n")

  # Aggregate at UPA-V1014 level within quarter
  dt[, `:=`(
    upa_date_min = fifelse(all(is.na(date_min)), as.Date(NA), max(date_min, na.rm = TRUE)),
    upa_date_max = fifelse(all(is.na(date_max)), as.Date(NA), min(date_max, na.rm = TRUE))
  ), by = .(Ano, Trimestre, UPA, V1014)]

  # Calculate month positions
  dt[, `:=`(
    upa_month_min_pos = month_in_quarter(upa_date_min),
    upa_month_max_pos = month_in_quarter(upa_date_max)
  )]

  # Get bounds at UPA-V1014 level
  upa_bounds <- unique(dt[, .(Ano, Trimestre, UPA, V1014,
                               upa_date_min, upa_date_max,
                               upa_month_min_pos, upa_month_max_pos)])

  # Calculate month range
  upa_bounds[, month_range := upa_month_max_pos - upa_month_min_pos + 1L]

  # Join to crosswalk
  crosswalk[upa_bounds, on = .(Ano, Trimestre, UPA, V1014), `:=`(
    upa_date_min = i.upa_date_min,
    upa_date_max = i.upa_date_max,
    upa_month_min_pos = i.upa_month_min_pos,
    upa_month_max_pos = i.upa_month_max_pos,
    month_range = i.month_range
  )]

  # For range == 2 and not strictly determined, calculate probabilistic month
  # Check if months are sequential (difference of 1)
  crosswalk[month_range == 2L &
              is.na(ref_month_in_quarter) &
              !is.na(upa_date_min) &
              !is.na(upa_date_max) &
              (upa_month_max_pos - upa_month_min_pos) == 1L, `:=`(
    date_midpoint = upa_date_min + as.integer((upa_date_max - upa_date_min) / 2)
  )]

  # Determine month from midpoint
  crosswalk[!is.na(date_midpoint), `:=`(
    ref_month_exp = month_in_quarter(date_midpoint)
  )]

  # Calculate confidence based on proportion of interval in likely month
  # Get month boundaries
  crosswalk[!is.na(date_midpoint), `:=`(
    month1_start = make_date(Ano, quarter_month_n(Trimestre, 1L), 1L),
    month2_start = make_date(Ano, quarter_month_n(Trimestre, 2L), 1L),
    month3_start = make_date(Ano, quarter_month_n(Trimestre, 3L), 1L)
  )]

  # Calculate days in each possible month within the interval
  crosswalk[!is.na(ref_month_exp) & month_range == 2L, `:=`(
    boundary_date = fifelse(upa_month_min_pos == 1L, month2_start,
                            fifelse(upa_month_min_pos == 2L, month3_start, as.Date(NA)))
  )]

  # Calculate days in each period - boundary_date is first day of second period
  # days_before_boundary = days in first period (before boundary)
  # days_after_boundary = days in second period (on or after boundary)
  crosswalk[!is.na(boundary_date), `:=`(
    total_days = as.integer(upa_date_max - upa_date_min) + 1L
  )]

  crosswalk[!is.na(boundary_date), `:=`(
    # Days before boundary: from upa_date_min to (boundary - 1), clamped to interval
    days_before_boundary = pmax(0L, as.integer(pmin(boundary_date - 1L, upa_date_max) - upa_date_min + 1L))
  )]

  crosswalk[!is.na(boundary_date), `:=`(
    # Days after boundary: remaining days
    days_after_boundary = total_days - days_before_boundary
  )]

  crosswalk[!is.na(total_days) & total_days > 0L, `:=`(
    ref_month_exp_confidence = fifelse(
      ref_month_exp == upa_month_min_pos,
      days_before_boundary / total_days,
      days_after_boundary / total_days
    )
  )]

  # Apply threshold - clear assignments below threshold
  crosswalk[!is.na(ref_month_exp_confidence) & ref_month_exp_confidence < confidence_threshold, `:=`(
    ref_month_exp = NA_integer_,
    ref_month_exp_confidence = NA_real_
  )]

  n_month_exp <- sum(!is.na(crosswalk$ref_month_exp))
  if (verbose) {
    cat(sprintf("    Assigned %s months (confidence >= %.0f%%)\n",
                format(n_month_exp, big.mark = ","), confidence_threshold * 100))
  }

  # ==========================================================================
  # STEP 3: FORTNIGHT PROBABILISTIC (Household level, NESTED)
  # ==========================================================================

  if (verbose) cat("  Phase 2: Fortnight probabilistic identification (nested)...\n")

  # Aggregate at household level
  dt[, `:=`(
    hh_date_min = fifelse(all(is.na(date_min)), as.Date(NA), max(date_min, na.rm = TRUE)),
    hh_date_max = fifelse(all(is.na(date_max)), as.Date(NA), min(date_max, na.rm = TRUE))
  ), by = .(Ano, Trimestre, UPA, V1008)]

  # Get bounds at household level
  hh_bounds <- unique(dt[, .(Ano, Trimestre, UPA, V1008, V1014,
                              hh_date_min, hh_date_max)])

  # Join to crosswalk
  crosswalk[hh_bounds, on = .(Ano, Trimestre, UPA, V1008, V1014), `:=`(
    hh_date_min = i.hh_date_min,
    hh_date_max = i.hh_date_max
  )]

  # NESTING: Only process observations with identified month (strictly OR experimentally)
  crosswalk[, month_identified := !is.na(ref_month_in_quarter) | !is.na(ref_month_exp)]

  # Get the effective month for nesting
  crosswalk[month_identified == TRUE, `:=`(
    effective_month = fifelse(!is.na(ref_month_in_quarter), ref_month_in_quarter, ref_month_exp)
  )]

  # Calculate fortnight bounds WITHIN the identified month
  # Month 1 -> fortnights 1-2, Month 2 -> fortnights 3-4, Month 3 -> fortnights 5-6
  crosswalk[month_identified == TRUE, `:=`(
    fortnight_lower = (effective_month - 1L) * 2L + 1L,
    fortnight_upper = effective_month * 2L
  )]

  # Calculate actual fortnight positions from dates
  crosswalk[month_identified == TRUE & !is.na(hh_date_min), `:=`(
    hh_fortnight_min = date_to_fortnight_in_quarter(hh_date_min, Trimestre),
    hh_fortnight_max = date_to_fortnight_in_quarter(hh_date_max, Trimestre)
  )]

  # Constrain to within the identified month
  crosswalk[month_identified == TRUE & !is.na(hh_fortnight_min), `:=`(
    hh_fortnight_min = pmax(hh_fortnight_min, fortnight_lower, na.rm = TRUE),
    hh_fortnight_max = pmin(hh_fortnight_max, fortnight_upper, na.rm = TRUE)
  )]

  crosswalk[month_identified == TRUE, `:=`(
    fortnight_range = hh_fortnight_max - hh_fortnight_min + 1L
  )]

  # For range == 2 and not strictly determined, check if sequential
  crosswalk[month_identified == TRUE &
              fortnight_range == 2L &
              is.na(ref_fortnight_in_quarter) &
              !is.na(hh_date_min) &
              (hh_fortnight_max - hh_fortnight_min) == 1L, `:=`(
    hh_date_midpoint = hh_date_min + as.integer((hh_date_max - hh_date_min) / 2)
  )]

  # Determine fortnight from midpoint
  crosswalk[!is.na(hh_date_midpoint), `:=`(
    ref_fortnight_exp = date_to_fortnight_in_quarter(hh_date_midpoint, Trimestre)
  )]

  # Calculate confidence: proportion of interval in the likely fortnight
  # Fortnight boundary is at day 15/16 of each month
  crosswalk[!is.na(ref_fortnight_exp) & fortnight_range == 2L, `:=`(
    fortnight_month_num = ((hh_fortnight_min - 1L) %/% 2L) + 1L
  )]

  crosswalk[!is.na(fortnight_month_num), `:=`(
    boundary_day_15 = make_date(Ano, quarter_month_n(Trimestre, fortnight_month_num), 16L)
  )]

  # Calculate days in each fortnight - boundary_day_15 is day 16 (first day of second fortnight)
  crosswalk[!is.na(boundary_day_15), `:=`(
    total_interval_days = as.integer(hh_date_max - hh_date_min) + 1L
  )]

  crosswalk[!is.na(boundary_day_15), `:=`(
    # Days in first fortnight: from hh_date_min to (boundary - 1), clamped to interval
    days_in_first_fortnight = pmax(0L, as.integer(pmin(boundary_day_15 - 1L, hh_date_max) - hh_date_min + 1L))
  )]

  crosswalk[!is.na(boundary_day_15), `:=`(
    # Days in second fortnight: remaining days
    days_in_second_fortnight = total_interval_days - days_in_first_fortnight
  )]

  crosswalk[!is.na(total_interval_days) & total_interval_days > 0L & !is.na(ref_fortnight_exp), `:=`(
    ref_fortnight_exp_confidence = fifelse(
      ref_fortnight_exp == hh_fortnight_min,
      days_in_first_fortnight / total_interval_days,
      days_in_second_fortnight / total_interval_days
    )
  )]

  # Apply threshold
  crosswalk[!is.na(ref_fortnight_exp_confidence) & ref_fortnight_exp_confidence < confidence_threshold, `:=`(
    ref_fortnight_exp = NA_integer_,
    ref_fortnight_exp_confidence = NA_real_
  )]

  n_fortnight_exp <- sum(!is.na(crosswalk$ref_fortnight_exp))
  if (verbose) {
    cat(sprintf("    Assigned %s fortnights (confidence >= %.0f%%)\n",
                format(n_fortnight_exp, big.mark = ","), confidence_threshold * 100))
  }

  # ==========================================================================
  # STEP 4: WEEK PROBABILISTIC (Household level, NESTED)
  # ==========================================================================

  if (verbose) cat("  Phase 3: Week probabilistic identification (nested)...\n")

  # NESTING: Only process observations with identified fortnight (strictly OR experimentally)
  crosswalk[, fortnight_identified := !is.na(ref_fortnight_in_quarter) | !is.na(ref_fortnight_exp)]

  # Get effective fortnight for nesting
  crosswalk[fortnight_identified == TRUE, `:=`(
    effective_fortnight = fifelse(!is.na(ref_fortnight_in_quarter),
                                  ref_fortnight_in_quarter, ref_fortnight_exp)
  )]

  # Calculate week bounds within the identified fortnight
  # Fortnight 1 = month 1, days 1-15
  # Fortnight 2 = month 1, days 16-end
  # etc.
  crosswalk[fortnight_identified == TRUE, `:=`(
    fortnight_month = ((effective_fortnight - 1L) %/% 2L) + 1L,
    fortnight_half = ((effective_fortnight - 1L) %% 2L) + 1L
  )]

  crosswalk[fortnight_identified == TRUE, `:=`(
    fortnight_start_day = fifelse(fortnight_half == 1L, 1L, 16L),
    fortnight_end_day = fifelse(fortnight_half == 1L, 15L,
                                days_in_month(Ano, quarter_month_n(Trimestre, fortnight_month)))
  )]

  crosswalk[fortnight_identified == TRUE, `:=`(
    fortnight_start_date = make_date(Ano, quarter_month_n(Trimestre, fortnight_month), fortnight_start_day),
    fortnight_end_date = make_date(Ano, quarter_month_n(Trimestre, fortnight_month), fortnight_end_day)
  )]

  # Calculate week bounds from dates
  crosswalk[fortnight_identified == TRUE & !is.na(hh_date_min), `:=`(
    hh_week_min = date_to_yyyyww(pmax(hh_date_min, fortnight_start_date)),
    hh_week_max = date_to_yyyyww(pmin(hh_date_max, fortnight_end_date))
  )]

  # Convert to sequential for range calculation
  crosswalk[fortnight_identified == TRUE & !is.na(hh_week_min), `:=`(
    hh_week_min_seq = (hh_week_min %/% 100L) * 53L + (hh_week_min %% 100L),
    hh_week_max_seq = (hh_week_max %/% 100L) * 53L + (hh_week_max %% 100L)
  )]

  crosswalk[fortnight_identified == TRUE & !is.na(hh_week_min_seq), `:=`(
    week_range = hh_week_max_seq - hh_week_min_seq + 1L
  )]

  # For range == 2 and not strictly determined, check if sequential
  crosswalk[fortnight_identified == TRUE &
              week_range == 2L &
              is.na(ref_week_in_quarter) &
              !is.na(hh_date_min) &
              (hh_week_max_seq - hh_week_min_seq) == 1L, `:=`(
    week_date_midpoint = hh_date_min + as.integer((hh_date_max - hh_date_min) / 2)
  )]

  # Determine week from midpoint
  crosswalk[!is.na(week_date_midpoint), `:=`(
    ref_week_exp = week_in_quarter(week_date_midpoint, Trimestre, Ano)
  )]

  # Calculate confidence: proportion of interval in the likely week
  crosswalk[!is.na(ref_week_exp) & week_range == 2L, `:=`(
    midpoint_week_yyyyww = date_to_yyyyww(week_date_midpoint)
  )]

  # Get week boundary date - Monday of the second week (hh_week_max)
  # For a 2-week range, boundary is the start of the second week
  crosswalk[!is.na(midpoint_week_yyyyww), `:=`(
    week_boundary_date = yyyyww_to_date(hh_week_max)
  )]

  # Calculate days in each week - week_boundary_date is Monday of second week
  crosswalk[!is.na(week_boundary_date) & !is.na(hh_date_min), `:=`(
    total_week_interval = as.integer(hh_date_max - hh_date_min) + 1L
  )]

  crosswalk[!is.na(week_boundary_date) & !is.na(hh_date_min), `:=`(
    # Days in first week: from hh_date_min to (boundary - 1), clamped to interval
    days_in_first_week = pmax(0L, as.integer(pmin(week_boundary_date - 1L, hh_date_max) - hh_date_min + 1L))
  )]

  crosswalk[!is.na(week_boundary_date) & !is.na(hh_date_min), `:=`(
    # Days in second week: remaining days
    days_in_second_week = total_week_interval - days_in_first_week
  )]

  crosswalk[!is.na(total_week_interval) & total_week_interval > 0L & !is.na(ref_week_exp), `:=`(
    ref_week_exp_confidence = fifelse(
      midpoint_week_yyyyww == hh_week_min,
      days_in_first_week / total_week_interval,
      days_in_second_week / total_week_interval
    )
  )]

  # Apply threshold
  crosswalk[!is.na(ref_week_exp_confidence) & ref_week_exp_confidence < confidence_threshold, `:=`(
    ref_week_exp = NA_integer_,
    ref_week_exp_confidence = NA_real_
  )]

  n_week_exp <- sum(!is.na(crosswalk$ref_week_exp))
  if (verbose) {
    cat(sprintf("    Assigned %s weeks (confidence >= %.0f%%)\n",
                format(n_week_exp, big.mark = ","), confidence_threshold * 100))
  }

  # ==========================================================================
  # CLEANUP
  # ==========================================================================

  temp_cols <- c("upa_date_min", "upa_date_max", "upa_month_min_pos", "upa_month_max_pos",
                 "month_range", "date_midpoint", "month1_start", "month2_start", "month3_start",
                 "boundary_date", "days_before_boundary", "days_after_boundary", "total_days",
                 "hh_date_min", "hh_date_max", "month_identified", "effective_month",
                 "fortnight_lower", "fortnight_upper", "hh_fortnight_min", "hh_fortnight_max",
                 "fortnight_range", "hh_date_midpoint", "fortnight_month_num", "boundary_day_15",
                 "days_in_first_fortnight", "days_in_second_fortnight", "total_interval_days",
                 "fortnight_identified", "effective_fortnight", "fortnight_month", "fortnight_half",
                 "fortnight_start_day", "fortnight_end_day", "fortnight_start_date", "fortnight_end_date",
                 "hh_week_min", "hh_week_max", "hh_week_min_seq", "hh_week_max_seq", "week_range",
                 "week_date_midpoint", "midpoint_week_yyyyww", "week_boundary_date",
                 "days_in_first_week", "days_in_second_week", "total_week_interval")
  crosswalk[, (intersect(temp_cols, names(crosswalk))) := NULL]

  if (verbose) {
    cat("  Probabilistic strategy complete.\n")
  }

  crosswalk
}


# =============================================================================
# UPA AGGREGATION STRATEGY (Nested)
# =============================================================================

#' Apply UPA Aggregation Strategy with Proper Nesting
#'
#' Extends strictly identified periods to all observations in the same UPA-V1014
#' within the quarter, if a sufficient proportion already have strict identification.
#'
#' @param crosswalk Crosswalk data.table
#' @param threshold Minimum proportion of UPA-V1014 observations with strict identification
#' @param verbose Print progress
#' @return Modified crosswalk with UPA-aggregated columns
#' @keywords internal
#' @noRd
apply_upa_aggregation_strategy_nested <- function(crosswalk, threshold, verbose) {

  if (verbose) cat("Applying UPA aggregation strategy (nested)...\n")

  # ==========================================================================
  # STEP 1: MONTH AGGREGATION
  # ==========================================================================

  if (verbose) cat("  Phase 1: Month UPA aggregation...\n")

  # Calculate proportion of UPA-V1014 within-quarter with strictly identified month
  upa_month_stats <- crosswalk[, .(
    n_total = .N,
    n_strict = sum(!is.na(ref_month_in_quarter)),
    consensus_month = fifelse(
      uniqueN(ref_month_in_quarter[!is.na(ref_month_in_quarter)]) == 1L,
      ref_month_in_quarter[!is.na(ref_month_in_quarter)][1L],
      NA_integer_
    )
  ), by = .(Ano, Trimestre, UPA, V1014)]

  upa_month_stats[, prop_strict := n_strict / n_total]

  # Only extend if proportion >= threshold AND there's a consensus
  upa_month_qualify <- upa_month_stats[prop_strict >= threshold & !is.na(consensus_month)]

  if (nrow(upa_month_qualify) > 0) {
    # Join and assign to observations without strict month
    crosswalk[upa_month_qualify, on = .(Ano, Trimestre, UPA, V1014), `:=`(
      ref_month_exp = fifelse(is.na(ref_month_in_quarter), i.consensus_month, ref_month_exp),
      ref_month_exp_confidence = fifelse(is.na(ref_month_in_quarter), 1.0, ref_month_exp_confidence)
    )]
  }

  n_month_upa <- sum(!is.na(crosswalk$ref_month_exp) & is.na(crosswalk$ref_month_in_quarter))
  if (verbose) {
    cat(sprintf("    Extended to %s observations via UPA aggregation\n",
                format(n_month_upa, big.mark = ",")))
  }

  # ==========================================================================
  # STEP 2: FORTNIGHT AGGREGATION (NESTED)
  # ==========================================================================

  if (verbose) cat("  Phase 2: Fortnight UPA aggregation (nested)...\n")

  # Calculate proportion of UPA-V1014 within-quarter with strictly identified fortnight
  upa_fortnight_stats <- crosswalk[, .(
    n_total = .N,
    n_strict = sum(!is.na(ref_fortnight_in_quarter)),
    consensus_fortnight = fifelse(
      uniqueN(ref_fortnight_in_quarter[!is.na(ref_fortnight_in_quarter)]) == 1L,
      ref_fortnight_in_quarter[!is.na(ref_fortnight_in_quarter)][1L],
      NA_integer_
    )
  ), by = .(Ano, Trimestre, UPA, V1014)]

  upa_fortnight_stats[, prop_strict := n_strict / n_total]

  # Only extend if proportion >= threshold AND there's a consensus
  upa_fortnight_qualify <- upa_fortnight_stats[prop_strict >= threshold & !is.na(consensus_fortnight)]

  if (nrow(upa_fortnight_qualify) > 0) {
    # NESTING: Only assign to observations with identified month
    # AND verify consensus_fortnight falls within that month's bounds
    crosswalk[, month_identified := !is.na(ref_month_in_quarter) | !is.na(ref_month_exp)]
    crosswalk[month_identified == TRUE, `:=`(
      effective_month = fifelse(!is.na(ref_month_in_quarter), ref_month_in_quarter, ref_month_exp),
      fortnight_lower = (fifelse(!is.na(ref_month_in_quarter), ref_month_in_quarter, ref_month_exp) - 1L) * 2L + 1L,
      fortnight_upper = fifelse(!is.na(ref_month_in_quarter), ref_month_in_quarter, ref_month_exp) * 2L
    )]

    # Join consensus_fortnight and validate it's within the month's fortnight bounds
    crosswalk[upa_fortnight_qualify, on = .(Ano, Trimestre, UPA, V1014), `:=`(
      ref_fortnight_exp = fifelse(
        is.na(ref_fortnight_in_quarter) & month_identified &
          i.consensus_fortnight >= fortnight_lower &
          i.consensus_fortnight <= fortnight_upper,
        i.consensus_fortnight,
        ref_fortnight_exp
      ),
      ref_fortnight_exp_confidence = fifelse(
        is.na(ref_fortnight_in_quarter) & month_identified &
          i.consensus_fortnight >= fortnight_lower &
          i.consensus_fortnight <= fortnight_upper,
        1.0,
        ref_fortnight_exp_confidence
      )
    )]

    crosswalk[, c("month_identified", "effective_month", "fortnight_lower", "fortnight_upper") := NULL]
  }

  n_fortnight_upa <- sum(!is.na(crosswalk$ref_fortnight_exp) & is.na(crosswalk$ref_fortnight_in_quarter))
  if (verbose) {
    cat(sprintf("    Extended to %s observations via UPA aggregation\n",
                format(n_fortnight_upa, big.mark = ",")))
  }

  # ==========================================================================
  # STEP 3: WEEK AGGREGATION (NESTED)
  # ==========================================================================

  if (verbose) cat("  Phase 3: Week UPA aggregation (nested)...\n")

  # Calculate proportion of UPA-V1014 within-quarter with strictly identified week
  upa_week_stats <- crosswalk[, .(
    n_total = .N,
    n_strict = sum(!is.na(ref_week_in_quarter)),
    consensus_week = fifelse(
      uniqueN(ref_week_in_quarter[!is.na(ref_week_in_quarter)]) == 1L,
      ref_week_in_quarter[!is.na(ref_week_in_quarter)][1L],
      NA_integer_
    )
  ), by = .(Ano, Trimestre, UPA, V1014)]

  upa_week_stats[, prop_strict := n_strict / n_total]

  # Only extend if proportion >= threshold AND there's a consensus
  upa_week_qualify <- upa_week_stats[prop_strict >= threshold & !is.na(consensus_week)]

  if (nrow(upa_week_qualify) > 0) {
    # NESTING: Only assign to observations with identified fortnight
    # Note: Full week-within-fortnight validation would require complex date calculations.
    # Since consensus_week comes from strictly identified weeks (which were already
    # constrained to their fortnight bounds during strict identification), and we require
    # fortnight_identified, the assignment should be valid. We rely on the strict
    # algorithm's nesting enforcement for the source consensus values.
    crosswalk[, fortnight_identified := !is.na(ref_fortnight_in_quarter) | !is.na(ref_fortnight_exp)]

    crosswalk[upa_week_qualify, on = .(Ano, Trimestre, UPA, V1014), `:=`(
      ref_week_exp = fifelse(
        is.na(ref_week_in_quarter) & fortnight_identified,
        i.consensus_week,
        ref_week_exp
      ),
      ref_week_exp_confidence = fifelse(
        is.na(ref_week_in_quarter) & fortnight_identified,
        1.0,
        ref_week_exp_confidence
      )
    )]

    crosswalk[, fortnight_identified := NULL]
  }

  n_week_upa <- sum(!is.na(crosswalk$ref_week_exp) & is.na(crosswalk$ref_week_in_quarter))
  if (verbose) {
    cat(sprintf("    Extended to %s observations via UPA aggregation\n",
                format(n_week_upa, big.mark = ",")))
  }

  if (verbose) {
    cat("  UPA aggregation strategy complete.\n")
  }

  crosswalk
}


# =============================================================================
# COMBINED STRATEGY (Nested)
# =============================================================================

#' Apply Combined Strategy (Probabilistic + UPA Aggregation) with Proper Nesting
#'
#' Applies probabilistic rule at UPA-V1014 level when sufficient proportion
#' have narrow ranges.
#'
#' @param crosswalk Crosswalk data.table
#' @param data Original PNADC data
#' @param confidence_threshold Minimum confidence to assign
#' @param upa_threshold Minimum proportion of UPA with narrow ranges
#' @param verbose Print progress
#' @return Modified crosswalk
#' @keywords internal
#' @noRd
apply_combined_strategy_nested <- function(crosswalk, data, confidence_threshold, upa_threshold, verbose) {

  if (verbose) cat("Applying combined strategy (probabilistic + UPA aggregation, nested)...\n")

  # ==========================================================================
  # STEP 1: Calculate date bounds from original data
  # ==========================================================================

  if (verbose) cat("  Calculating date bounds...\n")

  dt <- ensure_data_table(data, copy = TRUE)

  # Basic preprocessing
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

  # Calculate quarter dates
  unique_quarters <- unique(dt[, .(Ano, Trimestre)])
  unique_quarters[, `:=`(
    month1 = quarter_month_n(Trimestre, 1L),
    month2 = quarter_month_n(Trimestre, 2L),
    month3 = quarter_month_n(Trimestre, 3L)
  )]
  unique_quarters[, `:=`(
    first_sat_m1 = first_valid_saturday(Ano, month1, min_days = 4L),
    first_sat_m2 = first_valid_saturday(Ano, month2, min_days = 4L),
    first_sat_m3 = first_valid_saturday(Ano, month3, min_days = 4L)
  )]

  dt[unique_quarters, on = .(Ano, Trimestre), `:=`(
    month1 = i.month1, month2 = i.month2, month3 = i.month3,
    first_sat_m1 = i.first_sat_m1, first_sat_m2 = i.first_sat_m2, first_sat_m3 = i.first_sat_m3
  )]

  # Calculate birthday constraints
  dt[, birthday := make_birthday(V20081, V2008, Ano)]
  dt[, first_sat_after_birthday := first_saturday_on_or_after(birthday)]
  dt[, visit_before_birthday := NA_integer_]
  dt[!is.na(V20082), visit_before_birthday := as.integer((Ano - V20082) - V2009)]

  # Date bounds
  dt[, `:=`(
    date_min = make_date(Ano, month1, first_sat_m1),
    date_max = make_date(Ano, month3, first_sat_m3) + 21L
  )]

  # Apply birthday constraints
  dt[visit_before_birthday == 0L & !is.na(first_sat_after_birthday) &
       first_sat_after_birthday > date_min & first_sat_after_birthday <= date_max,
     date_min := first_sat_after_birthday]

  dt[visit_before_birthday == 1L & !is.na(first_sat_after_birthday) &
       (first_sat_after_birthday - 7L) < date_max & (first_sat_after_birthday - 7L) >= date_min,
     date_max := first_sat_after_birthday - 7L]

  # ==========================================================================
  # STEP 2: MONTH - Combined probabilistic at UPA-V1014 level
  # ==========================================================================

  if (verbose) cat("  Phase 1: Month combined strategy...\n")

  # Calculate month positions for each observation
  dt[, `:=`(
    month_min_pos = month_in_quarter(date_min),
    month_max_pos = month_in_quarter(date_max)
  )]

  dt[, month_range := month_max_pos - month_min_pos + 1L]
  dt[, has_narrow_month_range := month_range == 2L & (month_max_pos - month_min_pos) == 1L]

  # Aggregate at UPA-V1014 within-quarter level
  upa_month_combined <- dt[, .(
    n_total = .N,
    n_narrow = sum(has_narrow_month_range, na.rm = TRUE),
    # Aggregate date bounds at UPA level
    upa_date_min = fifelse(all(is.na(date_min)), as.Date(NA), max(date_min, na.rm = TRUE)),
    upa_date_max = fifelse(all(is.na(date_max)), as.Date(NA), min(date_max, na.rm = TRUE))
  ), by = .(Ano, Trimestre, UPA, V1014)]

  upa_month_combined[, prop_narrow := n_narrow / n_total]

  # Calculate UPA-level month bounds
  upa_month_combined[, `:=`(
    upa_month_min = month_in_quarter(upa_date_min),
    upa_month_max = month_in_quarter(upa_date_max)
  )]

  upa_month_combined[, upa_month_range := upa_month_max - upa_month_min + 1L]

  # Qualify: proportion >= threshold AND UPA-level range == 2 sequential
  upa_month_qualify <- upa_month_combined[
    prop_narrow >= upa_threshold &
      upa_month_range == 2L &
      (upa_month_max - upa_month_min) == 1L &
      !is.na(upa_date_min) &
      !is.na(upa_date_max)
  ]

  if (nrow(upa_month_qualify) > 0) {
    # Calculate probabilistic assignment
    upa_month_qualify[, `:=`(
      date_midpoint = upa_date_min + as.integer((upa_date_max - upa_date_min) / 2)
    )]

    upa_month_qualify[, likely_month := month_in_quarter(date_midpoint)]

    # Calculate confidence
    upa_month_qualify[, `:=`(
      month1_start = make_date(Ano, quarter_month_n(Trimestre, 1L), 1L),
      month2_start = make_date(Ano, quarter_month_n(Trimestre, 2L), 1L),
      month3_start = make_date(Ano, quarter_month_n(Trimestre, 3L), 1L)
    )]

    upa_month_qualify[, boundary_date := fifelse(
      upa_month_min == 1L, month2_start,
      fifelse(upa_month_min == 2L, month3_start, as.Date(NA))
    )]

    # Calculate days in each period - boundary_date is first day of second month
    upa_month_qualify[!is.na(boundary_date), `:=`(
      total_days = as.integer(upa_date_max - upa_date_min) + 1L
    )]

    upa_month_qualify[!is.na(boundary_date), `:=`(
      days_before = pmax(0L, as.integer(pmin(boundary_date - 1L, upa_date_max) - upa_date_min + 1L))
    )]

    upa_month_qualify[!is.na(boundary_date), `:=`(
      days_after = total_days - days_before
    )]

    upa_month_qualify[total_days > 0L, `:=`(
      confidence = fifelse(likely_month == upa_month_min, days_before / total_days, days_after / total_days)
    )]

    # Apply threshold
    upa_month_qualify <- upa_month_qualify[confidence >= confidence_threshold]

    if (nrow(upa_month_qualify) > 0) {
      # Join to crosswalk
      crosswalk[upa_month_qualify, on = .(Ano, Trimestre, UPA, V1014), `:=`(
        ref_month_exp = fifelse(is.na(ref_month_in_quarter), i.likely_month, ref_month_exp),
        ref_month_exp_confidence = fifelse(is.na(ref_month_in_quarter), i.confidence, ref_month_exp_confidence)
      )]
    }
  }

  n_month_combined <- sum(!is.na(crosswalk$ref_month_exp) & is.na(crosswalk$ref_month_in_quarter))
  if (verbose) {
    cat(sprintf("    Assigned %s months via combined strategy\n",
                format(n_month_combined, big.mark = ",")))
  }

  # ==========================================================================
  # STEP 3: FORTNIGHT - Combined probabilistic at UPA-V1014 level (NESTED)
  # ==========================================================================

  if (verbose) cat("  Phase 2: Fortnight combined strategy (nested)...\n")

  # NESTING: Only process observations with identified month (strictly OR experimentally)
  crosswalk[, month_identified := !is.na(ref_month_in_quarter) | !is.na(ref_month_exp)]
  crosswalk[month_identified == TRUE, effective_month := fifelse(
    !is.na(ref_month_in_quarter), ref_month_in_quarter, ref_month_exp
  )]

  # Calculate fortnight positions for each observation (constrained to identified month)
  dt[, `:=`(
    fortnight_min_pos = date_to_fortnight_in_quarter(date_min, Trimestre),
    fortnight_max_pos = date_to_fortnight_in_quarter(date_max, Trimestre)
  )]

  # Join month identification status to dt
  month_status <- unique(crosswalk[, .(Ano, Trimestre, UPA, V1008, V1014, month_identified, effective_month)])
  dt[month_status, on = .(Ano, Trimestre, UPA, V1008, V1014), `:=`(
    month_identified = i.month_identified,
    effective_month = i.effective_month
  )]

  # Constrain fortnight bounds to identified month
  dt[month_identified == TRUE, `:=`(
    fortnight_lower = (effective_month - 1L) * 2L + 1L,
    fortnight_upper = effective_month * 2L
  )]

  dt[month_identified == TRUE, `:=`(
    fortnight_min_pos = pmax(fortnight_min_pos, fortnight_lower, na.rm = TRUE),
    fortnight_max_pos = pmin(fortnight_max_pos, fortnight_upper, na.rm = TRUE)
  )]

  dt[, fortnight_range := fortnight_max_pos - fortnight_min_pos + 1L]
  dt[, has_narrow_fortnight_range := month_identified == TRUE &
       fortnight_range == 2L &
       (fortnight_max_pos - fortnight_min_pos) == 1L]

  # Aggregate at UPA-V1014 within-quarter level
  upa_fortnight_combined <- dt[month_identified == TRUE, .(
    n_total = .N,
    n_narrow = sum(has_narrow_fortnight_range, na.rm = TRUE),
    # Aggregate date bounds at UPA level (for households with identified month)
    upa_date_min = fifelse(all(is.na(date_min)), as.Date(NA), max(date_min, na.rm = TRUE)),
    upa_date_max = fifelse(all(is.na(date_max)), as.Date(NA), min(date_max, na.rm = TRUE)),
    # Only use effective_month if all observations in the group have the same value
    effective_month = fifelse(
      uniqueN(effective_month[!is.na(effective_month)]) == 1L,
      effective_month[!is.na(effective_month)][1L],
      NA_integer_
    )
  ), by = .(Ano, Trimestre, UPA, V1014)]

  upa_fortnight_combined[, prop_narrow := n_narrow / n_total]

  # Calculate UPA-level fortnight bounds (constrained)
  upa_fortnight_combined[, `:=`(
    fortnight_lower = (effective_month - 1L) * 2L + 1L,
    fortnight_upper = effective_month * 2L
  )]

  upa_fortnight_combined[, `:=`(
    upa_fortnight_min = pmax(date_to_fortnight_in_quarter(upa_date_min, Trimestre), fortnight_lower, na.rm = TRUE),
    upa_fortnight_max = pmin(date_to_fortnight_in_quarter(upa_date_max, Trimestre), fortnight_upper, na.rm = TRUE)
  )]

  upa_fortnight_combined[, upa_fortnight_range := upa_fortnight_max - upa_fortnight_min + 1L]

  # Qualify: proportion >= threshold AND UPA-level range == 2 sequential
  upa_fortnight_qualify <- upa_fortnight_combined[
    prop_narrow >= upa_threshold &
      upa_fortnight_range == 2L &
      (upa_fortnight_max - upa_fortnight_min) == 1L &
      !is.na(upa_date_min) &
      !is.na(upa_date_max)
  ]

  if (nrow(upa_fortnight_qualify) > 0) {
    # Calculate probabilistic assignment
    upa_fortnight_qualify[, `:=`(
      date_midpoint = upa_date_min + as.integer((upa_date_max - upa_date_min) / 2)
    )]

    upa_fortnight_qualify[, likely_fortnight := date_to_fortnight_in_quarter(date_midpoint, Trimestre)]

    # Calculate confidence (boundary at day 15/16)
    upa_fortnight_qualify[, fortnight_month := ((upa_fortnight_min - 1L) %/% 2L) + 1L]
    upa_fortnight_qualify[, boundary_day_15 := make_date(Ano, quarter_month_n(Trimestre, fortnight_month), 16L)]

    # Calculate days in each fortnight - boundary_day_15 is day 16 (first day of second fortnight)
    upa_fortnight_qualify[!is.na(boundary_day_15), `:=`(
      total_days = as.integer(upa_date_max - upa_date_min) + 1L
    )]

    upa_fortnight_qualify[!is.na(boundary_day_15), `:=`(
      days_before = pmax(0L, as.integer(pmin(boundary_day_15 - 1L, upa_date_max) - upa_date_min + 1L))
    )]

    upa_fortnight_qualify[!is.na(boundary_day_15), `:=`(
      days_after = total_days - days_before
    )]

    upa_fortnight_qualify[total_days > 0L, `:=`(
      confidence = fifelse(likely_fortnight == upa_fortnight_min,
                           days_before / total_days, days_after / total_days)
    )]

    # Apply threshold
    upa_fortnight_qualify <- upa_fortnight_qualify[confidence >= confidence_threshold]

    if (nrow(upa_fortnight_qualify) > 0) {
      # Join to crosswalk (only for month-identified observations)
      crosswalk[upa_fortnight_qualify, on = .(Ano, Trimestre, UPA, V1014), `:=`(
        ref_fortnight_exp = fifelse(
          is.na(ref_fortnight_in_quarter) & month_identified,
          i.likely_fortnight,
          ref_fortnight_exp
        ),
        ref_fortnight_exp_confidence = fifelse(
          is.na(ref_fortnight_in_quarter) & month_identified,
          i.confidence,
          ref_fortnight_exp_confidence
        )
      )]
    }
  }

  n_fortnight_combined <- sum(!is.na(crosswalk$ref_fortnight_exp) & is.na(crosswalk$ref_fortnight_in_quarter))
  if (verbose) {
    cat(sprintf("    Assigned %s fortnights via combined strategy\n",
                format(n_fortnight_combined, big.mark = ",")))
  }

  # ==========================================================================
  # STEP 4: WEEK - Combined probabilistic at UPA-V1014 level (NESTED)
  # ==========================================================================

  if (verbose) cat("  Phase 3: Week combined strategy (nested)...\n")

  # NESTING: Only process observations with identified fortnight (strictly OR experimentally)
  crosswalk[, fortnight_identified := !is.na(ref_fortnight_in_quarter) | !is.na(ref_fortnight_exp)]
  crosswalk[fortnight_identified == TRUE, effective_fortnight := fifelse(
    !is.na(ref_fortnight_in_quarter), ref_fortnight_in_quarter, ref_fortnight_exp
  )]

  # Join fortnight identification status to dt
  fortnight_status <- unique(crosswalk[, .(Ano, Trimestre, UPA, V1008, V1014,
                                            fortnight_identified, effective_fortnight)])
  dt[fortnight_status, on = .(Ano, Trimestre, UPA, V1008, V1014), `:=`(
    fortnight_identified = i.fortnight_identified,
    effective_fortnight = i.effective_fortnight
  )]

  # Calculate week bounds constrained to identified fortnight
  dt[fortnight_identified == TRUE, `:=`(
    fortnight_month = ((effective_fortnight - 1L) %/% 2L) + 1L,
    fortnight_half = ((effective_fortnight - 1L) %% 2L) + 1L
  )]

  dt[fortnight_identified == TRUE, `:=`(
    fortnight_start_day = fifelse(fortnight_half == 1L, 1L, 16L),
    fortnight_end_day = fifelse(fortnight_half == 1L, 15L,
                                days_in_month(Ano, quarter_month_n(Trimestre, fortnight_month)))
  )]

  dt[fortnight_identified == TRUE, `:=`(
    fortnight_start_date = make_date(Ano, quarter_month_n(Trimestre, fortnight_month), fortnight_start_day),
    fortnight_end_date = make_date(Ano, quarter_month_n(Trimestre, fortnight_month), fortnight_end_day)
  )]

  dt[fortnight_identified == TRUE, `:=`(
    week_min = date_to_yyyyww(pmax(date_min, fortnight_start_date)),
    week_max = date_to_yyyyww(pmin(date_max, fortnight_end_date))
  )]

  dt[fortnight_identified == TRUE, `:=`(
    week_min_seq = (week_min %/% 100L) * 53L + (week_min %% 100L),
    week_max_seq = (week_max %/% 100L) * 53L + (week_max %% 100L)
  )]

  dt[, week_range := week_max_seq - week_min_seq + 1L]
  dt[, has_narrow_week_range := fortnight_identified == TRUE &
       week_range == 2L &
       (week_max_seq - week_min_seq) == 1L]

  # Aggregate at UPA-V1014 within-quarter level
  upa_week_combined <- dt[fortnight_identified == TRUE, .(
    n_total = .N,
    n_narrow = sum(has_narrow_week_range, na.rm = TRUE),
    # Aggregate date bounds at UPA level
    upa_date_min = fifelse(all(is.na(date_min)), as.Date(NA), max(date_min, na.rm = TRUE)),
    upa_date_max = fifelse(all(is.na(date_max)), as.Date(NA), min(date_max, na.rm = TRUE)),
    # Use effective_fortnight only if all values within UPA-V1014 agree
    effective_fortnight = fifelse(
      uniqueN(effective_fortnight[!is.na(effective_fortnight)]) == 1L,
      effective_fortnight[!is.na(effective_fortnight)][1L],
      NA_integer_
    )
  ), by = .(Ano, Trimestre, UPA, V1014)]

  upa_week_combined[, prop_narrow := n_narrow / n_total]

  # Calculate UPA-level week bounds (constrained)
  upa_week_combined[, `:=`(
    fortnight_month = ((effective_fortnight - 1L) %/% 2L) + 1L,
    fortnight_half = ((effective_fortnight - 1L) %% 2L) + 1L
  )]

  upa_week_combined[, `:=`(
    fortnight_start_day = fifelse(fortnight_half == 1L, 1L, 16L),
    fortnight_end_day = fifelse(fortnight_half == 1L, 15L,
                                days_in_month(Ano, quarter_month_n(Trimestre, fortnight_month)))
  )]

  upa_week_combined[, `:=`(
    fortnight_start_date = make_date(Ano, quarter_month_n(Trimestre, fortnight_month), fortnight_start_day),
    fortnight_end_date = make_date(Ano, quarter_month_n(Trimestre, fortnight_month), fortnight_end_day)
  )]

  upa_week_combined[, `:=`(
    upa_week_min = date_to_yyyyww(pmax(upa_date_min, fortnight_start_date)),
    upa_week_max = date_to_yyyyww(pmin(upa_date_max, fortnight_end_date))
  )]

  upa_week_combined[, `:=`(
    upa_week_min_seq = (upa_week_min %/% 100L) * 53L + (upa_week_min %% 100L),
    upa_week_max_seq = (upa_week_max %/% 100L) * 53L + (upa_week_max %% 100L)
  )]

  upa_week_combined[, upa_week_range := upa_week_max_seq - upa_week_min_seq + 1L]

  # Qualify: proportion >= threshold AND UPA-level range == 2 sequential
  upa_week_qualify <- upa_week_combined[
    prop_narrow >= upa_threshold &
      upa_week_range == 2L &
      (upa_week_max_seq - upa_week_min_seq) == 1L &
      !is.na(upa_date_min) &
      !is.na(upa_date_max)
  ]

  if (nrow(upa_week_qualify) > 0) {
    # Calculate probabilistic assignment
    upa_week_qualify[, `:=`(
      date_midpoint = upa_date_min + as.integer((upa_date_max - upa_date_min) / 2)
    )]

    upa_week_qualify[, likely_week := week_in_quarter(date_midpoint, Trimestre, Ano)]

    # Calculate confidence - week_boundary_date is Monday of second week
    upa_week_qualify[, week_boundary_date := yyyyww_to_date(upa_week_max)]

    upa_week_qualify[!is.na(week_boundary_date), `:=`(
      total_days = as.integer(upa_date_max - upa_date_min) + 1L
    )]

    upa_week_qualify[!is.na(week_boundary_date), `:=`(
      days_before = pmax(0L, as.integer(pmin(week_boundary_date - 1L, upa_date_max) - upa_date_min + 1L))
    )]

    upa_week_qualify[!is.na(week_boundary_date), `:=`(
      days_after = total_days - days_before
    )]

    upa_week_qualify[, midpoint_week_seq := (date_to_yyyyww(date_midpoint) %/% 100L) * 53L +
                       (date_to_yyyyww(date_midpoint) %% 100L)]

    upa_week_qualify[total_days > 0L, `:=`(
      confidence = fifelse(midpoint_week_seq == upa_week_min_seq,
                           days_before / total_days, days_after / total_days)
    )]

    # Apply threshold
    upa_week_qualify <- upa_week_qualify[confidence >= confidence_threshold]

    if (nrow(upa_week_qualify) > 0) {
      # Join to crosswalk (only for fortnight-identified observations)
      crosswalk[upa_week_qualify, on = .(Ano, Trimestre, UPA, V1014), `:=`(
        ref_week_exp = fifelse(
          is.na(ref_week_in_quarter) & fortnight_identified,
          i.likely_week,
          ref_week_exp
        ),
        ref_week_exp_confidence = fifelse(
          is.na(ref_week_in_quarter) & fortnight_identified,
          i.confidence,
          ref_week_exp_confidence
        )
      )]
    }
  }

  n_week_combined <- sum(!is.na(crosswalk$ref_week_exp) & is.na(crosswalk$ref_week_in_quarter))
  if (verbose) {
    cat(sprintf("    Assigned %s weeks via combined strategy\n",
                format(n_week_combined, big.mark = ",")))
  }

  # ==========================================================================
  # CLEANUP
  # ==========================================================================

  temp_cols <- c("month_identified", "effective_month", "fortnight_identified", "effective_fortnight")
  crosswalk[, (intersect(temp_cols, names(crosswalk))) := NULL]

  if (verbose) {
    cat("  Combined strategy complete.\n")
  }

  crosswalk
}
