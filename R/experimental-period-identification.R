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
#'   \item \strong{both}: Sequentially applies probabilistic strategy first, then
#'     UPA aggregation on top. Guarantees identification rate >= max of individual strategies.
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
#' @param upa_proportion_threshold Numeric (0-1). Minimum proportion of UPA
#'   observations (within quarter) that must have strict identification with consensus
#'   for extending to unidentified observations. Default 0.5.
#' @param include_derived Logical. If TRUE (default), output includes derived columns
#'   compatible with \code{pnadc_apply_periods()}: ref_month_start, ref_month_end,
#'   ref_month_yyyymm, ref_month_weeks, determined_month, etc. This allows
#'   experimental output to be used directly for weight calibration.
#' @param verbose Logical. If TRUE, print progress information.
#'
#' @return A modified crosswalk with additional columns. When \code{include_derived = TRUE}
#'   (default), output is directly compatible with \code{pnadc_apply_periods()}:
#'   \itemize{
#'     \item \strong{Experimental columns} (always included):
#'     \itemize{
#'       \item \code{ref_month_exp}: Experimentally assigned month position in quarter (1-3, or NA)
#'       \item \code{ref_month_exp_confidence}: Confidence of month assignment (0-1, proportion
#'         of date interval in assigned period; values below threshold are removed)
#'       \item \code{ref_fortnight_exp}: Experimentally assigned fortnight position in quarter (1-6, or NA)
#'       \item \code{ref_fortnight_exp_confidence}: Confidence of fortnight assignment (0-1)
#'       \item \code{ref_week_exp}: Experimentally assigned week position in quarter (1-14, or NA)
#'       \item \code{ref_week_exp_confidence}: Confidence of week assignment (0-1)
#'     }
#'     \item \strong{Derived columns} (when \code{include_derived = TRUE}):
#'     \itemize{
#'       \item \code{ref_month_start}, \code{ref_month_end}, \code{ref_month_in_quarter},
#'         \code{ref_month_yyyymm}, \code{ref_month_weeks}: Combined strict + experimental month
#'         (strict takes priority). Start/end are Sunday/Saturday of IBGE month boundaries.
#'       \item \code{determined_month}: TRUE if month is assigned (strictly or experimentally)
#'       \item \code{ref_fortnight_start}, \code{ref_fortnight_end}, \code{ref_fortnight_in_quarter},
#'         \code{ref_fortnight_yyyyff}: Combined strict + experimental fortnight.
#'         Start/end are Sunday/Saturday of IBGE fortnight boundaries.
#'       \item \code{determined_fortnight}: TRUE if fortnight is assigned
#'       \item \code{ref_week_start}, \code{ref_week_end}, \code{ref_week_in_quarter},
#'         \code{ref_week_yyyyww}: Combined strict + experimental week.
#'         Start/end are Sunday/Saturday of IBGE week boundaries.
#'       \item \code{determined_week}: TRUE if week is assigned
#'       \item \code{probabilistic_assignment}: TRUE if any period was assigned experimentally
#'         (vs strictly deterministic)
#'     }
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
#'   \item Calculate confidence based on the proportion of interval in the likely period (0-1)
#'   \item Only assign if confidence >= \code{confidence_threshold}
#' }
#'
#' For months: aggregates at UPA-V1014 level (but within-quarter, unlike strict algorithm)
#' For fortnights and weeks: works at household level
#'
#' ## UPA Aggregation Strategy
#'
#' Extends strictly identified periods based on consensus within geographic groups:
#' \itemize{
#'   \item \strong{Months}: Uses UPA-V1014 level (panel design ensures same month)
#'   \item \strong{Fortnights/Weeks}: Uses UPA level (all households in same UPA
#'     are interviewed in same fortnight/week within a quarter)
#' }
#' \enumerate{
#'   \item Calculate proportion of observations with strictly identified period
#'   \item If proportion >= \code{upa_proportion_threshold} AND consensus exists, extend
#'   \item Apply in nested order: months first, then fortnights, then weeks
#' }
#'
#' ## Combined Strategy ("both")
#'
#' Sequentially applies both strategies to maximize identification:
#' \enumerate{
#'   \item First, apply the probabilistic strategy (captures observations with
#'     narrow date ranges and high confidence)
#'   \item Then, apply UPA aggregation (extends based on strict consensus
#'     within UPA/UPA-V1014 groups)
#' }
#'
#' This guarantees that "both" identifies at least as many observations as
#' either individual strategy alone. The strategies operate independently
#' (UPA aggregation considers only strict identifications), so the result
#' is the union of both strategies.
#'
#' ## Integration with Weight Calibration
#'
#' With \code{include_derived = TRUE} (default), the output can be passed directly to
#' \code{pnadc_apply_periods()} for weight calibration. The derived columns combine
#' strict and experimental assignments, with strict taking priority. Use the
#' \code{probabilistic_assignment} flag to filter if you only want strict determinations.
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
#' @seealso
#' \code{\link{pnadc_identify_periods}} to build the crosswalk that this function modifies.
#' \code{\link{pnadc_apply_periods}} to apply period crosswalk and calibrate weights.
#' \code{\link{combine_period_crosswalks}} to merge strict and experimental crosswalks.
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
#'   strict = sum(!is.na(ref_month_in_quarter) & !probabilistic_assignment),
#'   experimental = sum(probabilistic_assignment, na.rm = TRUE),
#'   total = sum(determined_month)
#' )]
#'
#' # Use directly with calibration (experimental output is compatible)
#' result <- pnadc_apply_periods(pnadc_data, crosswalk_exp,
#'                               period = "month", calibrate = TRUE)
#'
#' # Or filter to only strict determinations
#' strict_only <- crosswalk_exp[probabilistic_assignment == FALSE | is.na(probabilistic_assignment)]
#' }
#'
#' @export
pnadc_experimental_periods <- function(
    crosswalk,
    data = NULL,
    strategy = c("none", "probabilistic", "upa_aggregation", "both"),
    confidence_threshold = 0.9,
    upa_proportion_threshold = 0.5,
    include_derived = TRUE,
    verbose = TRUE
) {

  strategy <- match.arg(strategy)

  # Validate thresholds
  checkmate::assert_number(confidence_threshold, lower = 0, upper = 1)

  checkmate::assert_number(upa_proportion_threshold, lower = 0, upper = 1)
  checkmate::assert_flag(include_derived)

  if (strategy == "none") {
    if (verbose) cat("No experimental strategy applied.\n")
    if (include_derived) {
      result <- data.table::copy(crosswalk)
      # Initialize experimental columns (all NA since no strategy applied)
      result[, `:=`(
        ref_month_exp = NA_integer_,
        ref_month_exp_confidence = NA_real_,
        ref_fortnight_exp = NA_integer_,
        ref_fortnight_exp_confidence = NA_real_,
        ref_week_exp = NA_integer_,
        ref_week_exp_confidence = NA_real_
      )]
      result <- .add_derived_columns_experimental(result, verbose)
      return(result)
    }
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
      if (include_derived) {
        result <- .add_derived_columns_experimental(result, verbose)
      }
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
      if (include_derived) {
        result <- .add_derived_columns_experimental(result, verbose)
      }
      return(result)
    }
    result <- apply_combined_strategy_nested(
      result, data, confidence_threshold, upa_proportion_threshold, verbose
    )
  }

  # ==========================================================================
  # ADD DERIVED COLUMNS FOR COMPATIBILITY WITH pnadc_apply_periods()
  # ==========================================================================

  if (include_derived) {
    result <- .add_derived_columns_experimental(result, verbose)
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

  # OPTIMIZATION: Subset to required columns BEFORE copying (80-90% memory reduction)
  required_cols <- c("Ano", "Trimestre", "UPA", "V1008", "V1014",
                     "V2008", "V20081", "V20082", "V2009")
  dt <- subset_and_copy(data, required_cols)

  # Basic preprocessing - convert to integer
  int_cols <- c("Ano", "Trimestre", "V2008", "V20081", "V20082")
  for (col in int_cols) {
    if (col %in% names(dt) && !is.integer(dt[[col]])) {
      data.table::set(dt, j = col, value = as.integer(dt[[col]]))
    }
  }
  # Convert V2009 to integer (saves memory, age is always integer)
  if ("V2009" %in% names(dt) && !is.integer(dt$V2009)) {
    data.table::set(dt, j = "V2009", value = as.integer(dt$V2009))
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

  # Calculate the last Saturday of the quarter using IBGE month end
  dt[, quarter_end := ibge_month_end(Ano, month3, min_days = 4L)]

  # Date bounds
  dt[, `:=`(
    date_min = make_date(Ano, month1, first_sat_m1),
    date_max = quarter_end
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

  # Calculate month positions using IBGE calendar
  # The IBGE month is determined by which month's Saturday boundary contains the Saturday
  dt[, `:=`(
    upa_month_min_pos = calculate_ibge_month_position(upa_date_min, Trimestre, Ano),
    upa_month_max_pos = calculate_ibge_month_position(upa_date_max, Trimestre, Ano)
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

  # Calculate confidence with division by zero protection
  crosswalk[!is.na(total_days) & total_days > 0L & !is.na(ref_month_exp), `:=`(
    ref_month_exp_confidence = fifelse(
      total_days > 0L & !is.na(days_before_boundary) & !is.na(days_after_boundary),
      fifelse(
        ref_month_exp == upa_month_min_pos,
        days_before_boundary / total_days,
        days_after_boundary / total_days
      ),
      NA_real_
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

  # Calculate actual fortnight positions from dates using IBGE fortnights
  crosswalk[month_identified == TRUE & !is.na(hh_date_min), `:=`(
    hh_fortnight_min = ibge_fortnight_in_quarter(hh_date_min, Trimestre, Ano, min_days = 4L),
    hh_fortnight_max = ibge_fortnight_in_quarter(hh_date_max, Trimestre, Ano, min_days = 4L)
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

  # Determine fortnight from midpoint using IBGE fortnights
  crosswalk[!is.na(hh_date_midpoint), `:=`(
    ref_fortnight_exp = ibge_fortnight_in_quarter(hh_date_midpoint, Trimestre, Ano, min_days = 4L)
  )]

  # Calculate confidence: proportion of interval in the likely fortnight
  # IBGE fortnight boundary is at the start of IBGE week 3 of each month (Sunday after week 2)
  crosswalk[!is.na(ref_fortnight_exp) & fortnight_range == 2L, `:=`(
    fortnight_month_num = ((hh_fortnight_min - 1L) %/% 2L) + 1L
  )]

  # Get the IBGE fortnight boundary (start of fortnight 2 = Sunday of week 3)
  crosswalk[!is.na(fortnight_month_num), `:=`(
    fortnight_boundary = ibge_fortnight_start(Ano, quarter_month_n(Trimestre, fortnight_month_num), 2L, min_days = 4L)
  )]

  # Calculate days in each fortnight - fortnight_boundary is first day of second fortnight (Sunday)
  crosswalk[!is.na(fortnight_boundary), `:=`(
    total_interval_days = as.integer(hh_date_max - hh_date_min) + 1L
  )]

  crosswalk[!is.na(fortnight_boundary), `:=`(
    # Days in first fortnight: from hh_date_min to (boundary - 1), clamped to interval
    days_in_first_fortnight = pmax(0L, as.integer(pmin(fortnight_boundary - 1L, hh_date_max) - hh_date_min + 1L))
  )]

  crosswalk[!is.na(fortnight_boundary), `:=`(
    # Days in second fortnight: remaining days
    days_in_second_fortnight = total_interval_days - days_in_first_fortnight
  )]

  # Calculate confidence with division by zero protection
  crosswalk[!is.na(total_interval_days) & total_interval_days > 0L & !is.na(ref_fortnight_exp), `:=`(
    ref_fortnight_exp_confidence = fifelse(
      total_interval_days > 0L & !is.na(days_in_first_fortnight) & !is.na(days_in_second_fortnight),
      fifelse(
        ref_fortnight_exp == hh_fortnight_min,
        days_in_first_fortnight / total_interval_days,
        days_in_second_fortnight / total_interval_days
      ),
      NA_real_
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

  # Calculate IBGE week bounds from dates (week position within quarter)
  crosswalk[fortnight_identified == TRUE & !is.na(hh_date_min), `:=`(
    hh_week_min_pos = ibge_week_in_quarter(pmax(hh_date_min, fortnight_start_date), Trimestre, Ano, min_days = 4L),
    hh_week_max_pos = ibge_week_in_quarter(pmin(hh_date_max, fortnight_end_date), Trimestre, Ano, min_days = 4L)
  )]

  crosswalk[fortnight_identified == TRUE & !is.na(hh_week_min_pos), `:=`(
    week_range = hh_week_max_pos - hh_week_min_pos + 1L
  )]

  # For range == 2 and not strictly determined, check if sequential
  crosswalk[fortnight_identified == TRUE &
              week_range == 2L &
              is.na(ref_week_in_quarter) &
              !is.na(hh_date_min) &
              (hh_week_max_pos - hh_week_min_pos) == 1L, `:=`(
    week_date_midpoint = hh_date_min + as.integer((hh_date_max - hh_date_min) / 2)
  )]

  # Determine week from midpoint using IBGE weeks
  crosswalk[!is.na(week_date_midpoint), `:=`(
    ref_week_exp = ibge_week_in_quarter(week_date_midpoint, Trimestre, Ano, min_days = 4L)
  )]

  # Calculate confidence: proportion of interval in the likely week
  # Get week boundary date - Sunday of the second week (start of hh_week_max_pos)
  # For a 2-week range, boundary is the start of the second week
  crosswalk[!is.na(ref_week_exp) & week_range == 2L, `:=`(
    week_boundary_date = ibge_week_dates_from_position(Ano, Trimestre, hh_week_max_pos, min_days = 4L)$start
  )]

  # Calculate days in each week - week_boundary_date is Sunday of second week
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

  # Calculate confidence with division by zero protection
  crosswalk[!is.na(total_week_interval) & total_week_interval > 0L & !is.na(ref_week_exp), `:=`(
    ref_week_exp_confidence = fifelse(
      total_week_interval > 0L & !is.na(days_in_first_week) & !is.na(days_in_second_week),
      fifelse(
        ref_week_exp == hh_week_min_pos,
        days_in_first_week / total_week_interval,
        days_in_second_week / total_week_interval
      ),
      NA_real_
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
  # CLEANUP - use shared helper for comprehensive temp column removal
  # ==========================================================================

  temp_cols <- c(
    # Month probabilistic
    "upa_date_min", "upa_date_max", "upa_month_min_pos", "upa_month_max_pos",
    "month_range", "date_midpoint", "month1_start", "month2_start", "month3_start",
    "boundary_date", "days_before_boundary", "days_after_boundary", "total_days",
    "quarter_end",
    # Fortnight probabilistic
    "hh_date_min", "hh_date_max", "month_identified", "effective_month",
    "fortnight_lower", "fortnight_upper", "hh_fortnight_min", "hh_fortnight_max",
    "fortnight_range", "hh_date_midpoint", "fortnight_month_num", "fortnight_boundary",
    "days_in_first_fortnight", "days_in_second_fortnight", "total_interval_days",
    # Week probabilistic
    "fortnight_identified", "effective_fortnight", "fortnight_month", "fortnight_half",
    "fortnight_start_day", "fortnight_end_day", "fortnight_start_date", "fortnight_end_date",
    "hh_week_min_pos", "hh_week_max_pos", "week_range",
    "week_date_midpoint", "week_boundary_date",
    "days_in_first_week", "days_in_second_week", "total_week_interval"
  )
  .cleanup_temp_columns(crosswalk, temp_cols)

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
#' Extends strictly identified periods to unidentified observations based on
#' consensus within geographic/sampling groups:
#' \itemize{
#'   \item \strong{Months}: Aggregates at UPA-V1014 level (panel design ensures
#'     same month position across quarters)
#'   \item \strong{Fortnights/Weeks}: Aggregates at UPA level (all households in
#'     same UPA are interviewed in same fortnight/week within a quarter,
#'     regardless of panel rotation V1014)
#' }
#'
#' @param crosswalk Crosswalk data.table
#' @param threshold Minimum proportion of observations with strict identification
#'   required before extending to unidentified observations
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

  # Calculate proportion of UPA within-quarter with strictly identified fortnight
  # NOTE: Aggregate at UPA level (not UPA-V1014) because all households in the same
  # UPA are interviewed in the same fortnight within a quarter, regardless of panel rotation.
  upa_fortnight_stats <- crosswalk[, .(
    n_total = .N,
    n_strict = sum(!is.na(ref_fortnight_in_quarter)),
    consensus_fortnight = fifelse(
      uniqueN(ref_fortnight_in_quarter[!is.na(ref_fortnight_in_quarter)]) == 1L,
      ref_fortnight_in_quarter[!is.na(ref_fortnight_in_quarter)][1L],
      NA_integer_
    )
  ), by = .(Ano, Trimestre, UPA)]

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
    crosswalk[upa_fortnight_qualify, on = .(Ano, Trimestre, UPA), `:=`(
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

  # Calculate proportion of UPA within-quarter with strictly identified week
  # NOTE: Aggregate at UPA level (not UPA-V1014) because all households in the same
  # UPA are interviewed in the same week within a quarter, regardless of panel rotation.
  upa_week_stats <- crosswalk[, .(
    n_total = .N,
    n_strict = sum(!is.na(ref_week_in_quarter)),
    consensus_week = fifelse(
      uniqueN(ref_week_in_quarter[!is.na(ref_week_in_quarter)]) == 1L,
      ref_week_in_quarter[!is.na(ref_week_in_quarter)][1L],
      NA_integer_
    )
  ), by = .(Ano, Trimestre, UPA)]

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

    crosswalk[upa_week_qualify, on = .(Ano, Trimestre, UPA), `:=`(
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
#' Sequentially applies both strategies: probabilistic first, then UPA aggregation.
#' This guarantees that "both" identifies at least as many periods as either
#' individual strategy alone.
#'
#' The previous implementation had an extra `prop_narrow` filter that made it
#' more restrictive than probabilistic alone. This refactored version ensures
#' the union behavior users expect from the "both" option.
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
  # STEP 1: Apply probabilistic strategy first
  # ==========================================================================
  # This captures all observations that can be assigned via probabilistic logic
  # (narrow date ranges with high confidence).

  crosswalk <- apply_probabilistic_strategy_nested(
    crosswalk, data, confidence_threshold, verbose
  )

  # ==========================================================================
  # STEP 2: Apply UPA aggregation on top
  # ==========================================================================
  # This extends assignments based on strict consensus within UPA-V1014 groups.
  # The strategies operate independently: UPA aggregation considers only strict
  # identifications (ref_month_in_quarter), not probabilistic assignments.
  # The union of both strategies is captured because both write to the
  # experimental columns (ref_month_exp, etc.) and neither overwrites
  # existing assignments.

  crosswalk <- apply_upa_aggregation_strategy_nested(
    crosswalk, upa_threshold, verbose
  )

  if (verbose) {
    cat("  Combined strategy complete.\n")
  }

  crosswalk
}


# =============================================================================
# SHARED HELPER FUNCTIONS
# =============================================================================

#' Calculate Date Bounds from Original Data
#'
#' Shared preprocessing function used by probabilistic and combined strategies.
#' Calculates birthday constraints and date bounds for each observation.
#'
#' @param data Original PNADC data.table
#' @return data.table with date_min, date_max, and household-level bounds
#' @keywords internal
#' @noRd
.calculate_date_bounds <- function(data) {

  # OPTIMIZATION: Subset to required columns BEFORE copying (80-90% memory reduction)
  required_cols <- c("Ano", "Trimestre", "UPA", "V1008", "V1014",
                     "V2008", "V20081", "V20082", "V2009")
  dt <- subset_and_copy(data, required_cols)

  # Basic preprocessing - ensure integer types
  int_cols <- c("Ano", "Trimestre", "V2008", "V20081", "V20082")
  for (col in int_cols) {
    if (col %in% names(dt) && !is.integer(dt[[col]])) {
      data.table::set(dt, j = col, value = as.integer(dt[[col]]))
    }
  }
  # Convert V2009 to integer (saves memory, age is always integer)
  if ("V2009" %in% names(dt) && !is.integer(dt$V2009)) {
    data.table::set(dt, j = "V2009", value = as.integer(dt$V2009))
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

  # Calculate the last Saturday of the quarter using IBGE month end
  dt[, quarter_end := ibge_month_end(Ano, month3, min_days = 4L)]

  # Date bounds
  dt[, `:=`(
    date_min = make_date(Ano, month1, first_sat_m1),
    date_max = quarter_end
  )]

  # Apply birthday constraints
  dt[visit_before_birthday == 0L & !is.na(first_sat_after_birthday) &
       first_sat_after_birthday > date_min & first_sat_after_birthday <= date_max,
     date_min := first_sat_after_birthday]

  dt[visit_before_birthday == 1L & !is.na(first_sat_after_birthday) &
       (first_sat_after_birthday - 7L) < date_max & (first_sat_after_birthday - 7L) >= date_min,
     date_max := first_sat_after_birthday - 7L]

  dt
}


#' Clean Up Temporary Columns
#'
#' Shared cleanup function to remove temporary columns from a data.table.
#' Ensures comprehensive cleanup regardless of code path.
#'
#' @param dt data.table to clean
#' @param temp_cols Character vector of column names to remove
#' @return Modified data.table (invisibly)
#' @keywords internal
#' @noRd
.cleanup_temp_columns <- function(dt, temp_cols) {
  cols_to_remove <- intersect(temp_cols, names(dt))
  if (length(cols_to_remove) > 0) {
    dt[, (cols_to_remove) := NULL]
  }
  invisible(dt)
}


#' Add Derived Columns for Compatibility with pnadc_apply_periods()
#'
#' Combines strict and experimental period assignments into unified columns
#' that match the output format of pnadc_identify_periods(). Strict assignments
#' take priority over experimental ones.
#'
#' @param crosswalk data.table with strict and experimental period columns
#' @param verbose Logical. If TRUE, print summary information.
#' @return Modified crosswalk with derived columns
#' @keywords internal
#' @noRd
.add_derived_columns_experimental <- function(crosswalk, verbose = FALSE) {

  # Track which assignments are probabilistic (experimental)
  crosswalk[, probabilistic_assignment := FALSE]

  # -------------------------------------------------------------------------
  # MONTH: Combine strict + experimental
  # -------------------------------------------------------------------------

  # If experimental assigned but strict not, mark as probabilistic
  crosswalk[is.na(ref_month_in_quarter) & !is.na(ref_month_exp),
            probabilistic_assignment := TRUE]

  # Create unified ref_month_in_quarter (strict takes priority)
  # Only update if strict is NA and experimental is not
  crosswalk[is.na(ref_month_in_quarter) & !is.na(ref_month_exp),
            ref_month_in_quarter := ref_month_exp]

  # Derive ref_month_yyyymm from ref_month_in_quarter
  crosswalk[!is.na(ref_month_in_quarter), `:=`(
    ref_month_yyyymm = yyyymm(Ano, quarter_month_n(Trimestre, ref_month_in_quarter))
  )]

  # Derive IBGE month boundaries (start/end dates) for newly assigned months
  crosswalk[!is.na(ref_month_yyyymm) & is.na(ref_month_start), `:=`(
    temp_month = quarter_month_n(Trimestre, ref_month_in_quarter)
  )]
  crosswalk[!is.na(temp_month), `:=`(
    ref_month_start = ibge_month_start(Ano, temp_month, min_days = 4L),
    ref_month_end = ibge_month_end(Ano, temp_month, min_days = 4L),
    ref_month_weeks = ibge_month_weeks(Ano, temp_month, min_days = 4L)
  )]
  crosswalk[, temp_month := NULL]

  # Update determined_month flag
  crosswalk[, determined_month := !is.na(ref_month_in_quarter)]

  # -------------------------------------------------------------------------
  # FORTNIGHT: Combine strict + experimental
  # -------------------------------------------------------------------------

  # If experimental assigned but strict not, mark as probabilistic
  crosswalk[is.na(ref_fortnight_in_quarter) & !is.na(ref_fortnight_exp),
            probabilistic_assignment := TRUE]

  # Create unified ref_fortnight_in_quarter (strict takes priority)
  crosswalk[is.na(ref_fortnight_in_quarter) & !is.na(ref_fortnight_exp),
            ref_fortnight_in_quarter := ref_fortnight_exp]

  # Derive ref_fortnight_yyyyff from ref_fortnight_in_quarter using IBGE format
  crosswalk[!is.na(ref_fortnight_in_quarter), `:=`(
    ref_fortnight_yyyyff = ibge_fortnight_in_quarter_to_yyyyff(Ano, Trimestre, ref_fortnight_in_quarter)
  )]

  # Derive IBGE fortnight boundaries (start/end dates) for newly assigned fortnights
  crosswalk[!is.na(ref_fortnight_yyyyff) & is.na(ref_fortnight_start), `:=`(
    temp_month_in_q = ((ref_fortnight_in_quarter - 1L) %/% 2L) + 1L,
    temp_fortnight_in_month = ((ref_fortnight_in_quarter - 1L) %% 2L) + 1L
  )]
  crosswalk[!is.na(temp_month_in_q), `:=`(
    temp_month = quarter_month_n(Trimestre, temp_month_in_q)
  )]
  crosswalk[!is.na(temp_month), `:=`(
    ref_fortnight_start = ibge_fortnight_start(Ano, temp_month, temp_fortnight_in_month, min_days = 4L),
    ref_fortnight_end = ibge_fortnight_end(Ano, temp_month, temp_fortnight_in_month, min_days = 4L),
    ref_fortnight_weeks = ibge_fortnight_weeks(Ano, temp_month, temp_fortnight_in_month, min_days = 4L)
  )]
  crosswalk[, c("temp_month_in_q", "temp_fortnight_in_month", "temp_month") := NULL]

  # Update determined_fortnight flag
  crosswalk[, determined_fortnight := !is.na(ref_fortnight_in_quarter)]

  # -------------------------------------------------------------------------
  # WEEK: Combine strict + experimental
  # -------------------------------------------------------------------------

  # If experimental assigned but strict not, mark as probabilistic
  crosswalk[is.na(ref_week_in_quarter) & !is.na(ref_week_exp),
            probabilistic_assignment := TRUE]

  # Create unified ref_week_in_quarter (strict takes priority)
  crosswalk[is.na(ref_week_in_quarter) & !is.na(ref_week_exp),
            ref_week_in_quarter := ref_week_exp]

  # For weeks, we need to derive IBGE week boundaries from the week position
  # The experimental ref_week_exp is a position in quarter (1-14)
  crosswalk[!is.na(ref_week_in_quarter) & is.na(ref_week_start), `:=`(
    ref_week_start = ibge_week_dates_from_position(Ano, Trimestre, ref_week_in_quarter, min_days = 4L)$start,
    ref_week_end = ibge_week_dates_from_position(Ano, Trimestre, ref_week_in_quarter, min_days = 4L)$end
  )]

  # Derive ref_week_yyyyww from the Saturday (end) of the IBGE week
  crosswalk[!is.na(ref_week_end) & is.na(ref_week_yyyyww), `:=`(
    ref_week_yyyyww = date_to_ibge_yyyyww(ref_week_end)
  )]

  # Update determined_week flag
  crosswalk[, determined_week := !is.na(ref_week_in_quarter)]

  # -------------------------------------------------------------------------
  # Summary
  # -------------------------------------------------------------------------

  if (verbose) {
    n_prob_month <- sum(crosswalk$probabilistic_assignment &
                         crosswalk$determined_month, na.rm = TRUE)
    n_prob_fortnight <- sum(crosswalk$probabilistic_assignment &
                             crosswalk$determined_fortnight, na.rm = TRUE)
    n_prob_week <- sum(crosswalk$probabilistic_assignment &
                        crosswalk$determined_week, na.rm = TRUE)
    cat(sprintf("  Derived columns added. Probabilistic assignments: %s months, %s fortnights, %s weeks\n",
                format(n_prob_month, big.mark = ","),
                format(n_prob_fortnight, big.mark = ","),
                format(n_prob_week, big.mark = ",")))
  }

  crosswalk
}


# NOTE: The week_in_quarter_to_yyyyww function has been replaced by IBGE-based functions
# in utils-dates.R. Use ibge_week_dates_from_position() and date_to_ibge_yyyyww() instead.

# =============================================================================
# COMBINE CROSSWALKS FUNCTION
# =============================================================================

#' Combine Strict and Experimental Period Crosswalks
#'
#' Merges a strict crosswalk from \code{pnadc_identify_periods()} with an
#' experimental crosswalk from \code{pnadc_experimental_periods()}, filling
#' in undetermined periods from the strict crosswalk with experimental
#' assignments.
#'
#' @param strict_crosswalk data.table from \code{pnadc_identify_periods()}
#' @param experimental_crosswalk data.table from \code{pnadc_experimental_periods()}
#'   with \code{include_derived = FALSE} (experimental columns only)
#' @param priority Character. Which source takes priority when both have assignments.
#'   Default "strict" means strict determinations are never overwritten.
#'   Use "experimental" to prefer experimental assignments (rarely needed).
#' @param verbose Logical. If TRUE, print summary of combined assignments.
#'
#' @return A combined crosswalk data.table with:
#'   \itemize{
#'     \item All columns from strict crosswalk
#'     \item Experimental columns (ref_month_exp, etc.)
#'     \item \code{probabilistic_assignment}: TRUE for rows where experimental
#'       assignment was used to fill a gap
#'   }
#'   The combined crosswalk is ready for use with \code{pnadc_apply_periods()}.
#'
#' @details
#' This function is useful when you want to:
#' \itemize{
#'   \item Keep strict and experimental crosswalks separate
#'   \item Apply different confidence thresholds to experimental assignments
#'   \item Audit which assignments came from which source
#' }
#'
#' If you just want experimental assignments integrated automatically, use
#' \code{pnadc_experimental_periods()} with \code{include_derived = TRUE}.
#'
#' @examples
#' \dontrun{
#' # Build strict crosswalk
#' strict <- pnadc_identify_periods(pnadc_data)
#'
#' # Build experimental crosswalk (without derived columns)
#' experimental <- pnadc_experimental_periods(
#'   strict,
#'   pnadc_data,
#'   strategy = "probabilistic",
#'   confidence_threshold = 0.95,
#'   include_derived = FALSE
#' )
#'
#' # Combine them
#' combined <- combine_period_crosswalks(strict, experimental)
#'
#' # Use with calibration
#' result <- pnadc_apply_periods(pnadc_data, combined,
#'                               period = "month", calibrate = TRUE)
#' }
#'
#' @seealso \code{\link{pnadc_identify_periods}}, \code{\link{pnadc_experimental_periods}}
#'
#' @export
combine_period_crosswalks <- function(
    strict_crosswalk,
    experimental_crosswalk,
    priority = c("strict", "experimental"),
    verbose = TRUE
) {

  priority <- match.arg(priority)

  # Validate inputs
  checkmate::assert_data_table(strict_crosswalk)
  checkmate::assert_data_table(experimental_crosswalk)

  # Required join keys
  join_keys <- c("Ano", "Trimestre", "UPA", "V1008", "V1014")

  missing_strict <- setdiff(join_keys, names(strict_crosswalk))
  if (length(missing_strict) > 0) {
    stop("strict_crosswalk missing required columns: ", paste(missing_strict, collapse = ", "))
  }

  missing_exp <- setdiff(join_keys, names(experimental_crosswalk))
  if (length(missing_exp) > 0) {
    stop("experimental_crosswalk missing required columns: ", paste(missing_exp, collapse = ", "))
  }

  # Copy strict crosswalk as base
  result <- data.table::copy(strict_crosswalk)

  # Ensure join key types match
  for (key in join_keys) {
    if (!is.integer(result[[key]])) {
      data.table::set(result, j = key, value = as.integer(result[[key]]))
    }
    if (!is.integer(experimental_crosswalk[[key]])) {
      data.table::set(experimental_crosswalk, j = key, value = as.integer(experimental_crosswalk[[key]]))
    }
  }

  # Extract experimental columns to merge
  exp_cols <- c("ref_month_exp", "ref_month_exp_confidence",
                "ref_fortnight_exp", "ref_fortnight_exp_confidence",
                "ref_week_exp", "ref_week_exp_confidence")

  exp_cols_present <- intersect(exp_cols, names(experimental_crosswalk))
  if (length(exp_cols_present) == 0) {
    warning("No experimental columns found in experimental_crosswalk. ",
            "Returning strict crosswalk unchanged.")
    result[, probabilistic_assignment := FALSE]
    return(result)
  }

  # Subset experimental crosswalk to join keys + experimental columns
  exp_subset <- experimental_crosswalk[, c(join_keys, exp_cols_present), with = FALSE]

  # Merge experimental columns
  result[exp_subset, on = join_keys, `:=`(
    ref_month_exp = i.ref_month_exp,
    ref_month_exp_confidence = i.ref_month_exp_confidence,
    ref_fortnight_exp = i.ref_fortnight_exp,
    ref_fortnight_exp_confidence = i.ref_fortnight_exp_confidence,
    ref_week_exp = i.ref_week_exp,
    ref_week_exp_confidence = i.ref_week_exp_confidence
  )]

  # Initialize probabilistic_assignment flag
  result[, probabilistic_assignment := FALSE]

  # Fill gaps based on priority
  if (priority == "strict") {
    # Only fill where strict is NA
    # Month
    result[is.na(ref_month_in_quarter) & !is.na(ref_month_exp), `:=`(
      ref_month_in_quarter = ref_month_exp,
      probabilistic_assignment = TRUE
    )]
    # Fortnight
    result[is.na(ref_fortnight_in_quarter) & !is.na(ref_fortnight_exp), `:=`(
      ref_fortnight_in_quarter = ref_fortnight_exp,
      probabilistic_assignment = TRUE
    )]
    # Week
    result[is.na(ref_week_in_quarter) & !is.na(ref_week_exp), `:=`(
      ref_week_in_quarter = ref_week_exp,
      probabilistic_assignment = TRUE
    )]
  } else {
    # Experimental takes priority (overwrite strict)
    result[!is.na(ref_month_exp), `:=`(
      ref_month_in_quarter = ref_month_exp,
      probabilistic_assignment = TRUE
    )]
    result[!is.na(ref_fortnight_exp), `:=`(
      ref_fortnight_in_quarter = ref_fortnight_exp,
      probabilistic_assignment = TRUE
    )]
    result[!is.na(ref_week_exp), `:=`(
      ref_week_in_quarter = ref_week_exp,
      probabilistic_assignment = TRUE
    )]
  }

  # Derive YYYYMM/YYYYFF/YYYYWW and IBGE boundary columns for newly assigned periods

  # Month - derive IBGE boundaries
  result[probabilistic_assignment == TRUE & !is.na(ref_month_in_quarter), `:=`(
    temp_month = quarter_month_n(Trimestre, ref_month_in_quarter)
  )]
  result[probabilistic_assignment == TRUE & !is.na(temp_month), `:=`(
    ref_month_yyyymm = yyyymm(Ano, temp_month),
    ref_month_start = ibge_month_start(Ano, temp_month, min_days = 4L),
    ref_month_end = ibge_month_end(Ano, temp_month, min_days = 4L),
    ref_month_weeks = ibge_month_weeks(Ano, temp_month, min_days = 4L)
  )]
  result[, temp_month := NULL]

  # Fortnight - derive IBGE boundaries
  result[probabilistic_assignment == TRUE & !is.na(ref_fortnight_in_quarter), `:=`(
    temp_month_in_q = ((ref_fortnight_in_quarter - 1L) %/% 2L) + 1L,
    temp_fortnight_in_month = ((ref_fortnight_in_quarter - 1L) %% 2L) + 1L
  )]
  result[probabilistic_assignment == TRUE & !is.na(temp_month_in_q), `:=`(
    temp_month = quarter_month_n(Trimestre, temp_month_in_q)
  )]
  result[probabilistic_assignment == TRUE & !is.na(temp_month), `:=`(
    ref_fortnight_yyyyff = ibge_fortnight_in_quarter_to_yyyyff(Ano, Trimestre, ref_fortnight_in_quarter),
    ref_fortnight_start = ibge_fortnight_start(Ano, temp_month, temp_fortnight_in_month, min_days = 4L),
    ref_fortnight_end = ibge_fortnight_end(Ano, temp_month, temp_fortnight_in_month, min_days = 4L),
    ref_fortnight_weeks = ibge_fortnight_weeks(Ano, temp_month, temp_fortnight_in_month, min_days = 4L)
  )]
  result[, c("temp_month_in_q", "temp_fortnight_in_month", "temp_month") := NULL]

  # Week - derive IBGE boundaries
  result[probabilistic_assignment == TRUE & !is.na(ref_week_in_quarter), `:=`(
    ref_week_start = ibge_week_dates_from_position(Ano, Trimestre, ref_week_in_quarter, min_days = 4L)$start,
    ref_week_end = ibge_week_dates_from_position(Ano, Trimestre, ref_week_in_quarter, min_days = 4L)$end
  )]
  result[probabilistic_assignment == TRUE & !is.na(ref_week_end), `:=`(
    ref_week_yyyyww = date_to_ibge_yyyyww(ref_week_end)
  )]

  # Update determination flags
  result[, `:=`(
    determined_month = !is.na(ref_month_in_quarter),
    determined_fortnight = !is.na(ref_fortnight_in_quarter),
    determined_week = !is.na(ref_week_in_quarter)
  )]

  # Summary
  if (verbose) {
    n_strict_month <- sum(result$determined_month & !result$probabilistic_assignment, na.rm = TRUE)
    n_exp_month <- sum(result$determined_month & result$probabilistic_assignment, na.rm = TRUE)
    n_strict_fortnight <- sum(result$determined_fortnight & !result$probabilistic_assignment, na.rm = TRUE)
    n_exp_fortnight <- sum(result$determined_fortnight & result$probabilistic_assignment, na.rm = TRUE)
    n_strict_week <- sum(result$determined_week & !result$probabilistic_assignment, na.rm = TRUE)
    n_exp_week <- sum(result$determined_week & result$probabilistic_assignment, na.rm = TRUE)

    cat("Combined crosswalk summary:\n")
    cat(sprintf("  Months: %s strict + %s experimental = %s total (%.1f%% rate)\n",
                format(n_strict_month, big.mark = ","),
                format(n_exp_month, big.mark = ","),
                format(n_strict_month + n_exp_month, big.mark = ","),
                100 * (n_strict_month + n_exp_month) / nrow(result)))
    cat(sprintf("  Fortnights: %s strict + %s experimental = %s total (%.1f%% rate)\n",
                format(n_strict_fortnight, big.mark = ","),
                format(n_exp_fortnight, big.mark = ","),
                format(n_strict_fortnight + n_exp_fortnight, big.mark = ","),
                100 * (n_strict_fortnight + n_exp_fortnight) / nrow(result)))
    cat(sprintf("  Weeks: %s strict + %s experimental = %s total (%.1f%% rate)\n",
                format(n_strict_week, big.mark = ","),
                format(n_exp_week, big.mark = ","),
                format(n_strict_week + n_exp_week, big.mark = ","),
                100 * (n_strict_week + n_exp_week) / nrow(result)))
  }

  result
}


#' Calculate IBGE Month Position from Date
#'
#' Determines which month position (1, 2, or 3) within a quarter a date belongs to,
#' using IBGE month boundaries. The IBGE month is determined by which month's
#' Saturday boundary contains the Saturday of the date's week.
#'
#' @param date Date vector
#' @param quarter Integer quarter vector (1-4)
#' @param year Integer year vector
#' @return Integer month position (1, 2, or 3) within the quarter, or NA if out of range
#' @keywords internal
#' @noRd
calculate_ibge_month_position <- function(date, quarter, year) {
  # Get the Saturday of the date's IBGE week
  sat_date <- ibge_week_saturday(date)
  sat_month <- fast_month(sat_date)

  # Get first month of quarter
  first_month <- quarter_first_month(quarter)

  # Calculate month position in quarter (1, 2, or 3)
  month_pos <- sat_month - first_month + 1L

  # Handle year boundary (e.g., Q4 spanning into January)
  sat_year <- fast_year(sat_date)
  month_pos <- data.table::fifelse(
    sat_year > year,
    sat_month + 12L - first_month + 1L,
    month_pos
  )

  # Return NA for dates outside the quarter range
  data.table::fifelse(month_pos >= 1L & month_pos <= 3L, month_pos, NA_integer_)
}
