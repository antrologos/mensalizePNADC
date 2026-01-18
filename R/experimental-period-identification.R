#' Experimental Period Identification Strategies
#'
#' Provides experimental strategies for improving fortnight and week
#' identification rates beyond the standard algorithm. These strategies
#' are for research and testing purposes and should NOT replace the main
#' identification logic.
#'
#' @description
#' Two experimental strategies are available:
#' \itemize{
#'   \item \strong{probabilistic}: For narrow ranges (2 possible fortnights/weeks),
#'     calculates the probability-weighted best guess based on date position
#'   \item \strong{upa_aggregation}: Aggregates constraints at UPA level
#'     (experimental - IBGE does NOT interview UPAs together)
#' }
#'
#' @param crosswalk A crosswalk data.table from \code{pnadc_identify_periods()}
#' @param data The original PNADC data.table used to build the crosswalk
#'   (required for probabilistic strategy to access date bounds)
#' @param strategy Character vector specifying which strategies to apply.
#'   Options: "none", "probabilistic", "upa_aggregation", "both"
#' @param upa_homogeneity_threshold Numeric. Only apply UPA aggregation if
#'   the measured UPA homogeneity rate exceeds this threshold. Default 0.80.
#' @param verbose Logical. If TRUE, print progress information.
#'
#' @return A modified crosswalk with additional columns:
#'   \itemize{
#'     \item \code{ref_fortnight_likely}: Probabilistic best guess (NA if range > 2)
#'     \item \code{ref_fortnight_confidence}: Probability of being correct (0.5-1.0)
#'     \item \code{ref_week_likely}: Probabilistic best guess (NA if range > 2)
#'     \item \code{ref_week_confidence}: Probability of being correct (0.5-1.0)
#'     \item \code{ref_fortnight_upa}: UPA-aggregated fortnight (if strategy enabled)
#'     \item \code{ref_week_upa}: UPA-aggregated week (if strategy enabled)
#'   }
#'
#' @details
#' ## Probabilistic Strategy
#'
#' When the interview date range spans exactly 2 fortnights or weeks, we can
#' calculate the probability that the interview occurred in the earlier vs later
#' period based on the relative position of the date range boundaries.
#'
#' For example, if date_min is day 10 and date_max is day 20, and the fortnight
#' boundary is day 15, then:
#' - Days 10-15 (6 days) fall in fortnight 1
#' - Days 16-20 (5 days) fall in fortnight 2
#' - Probability of fortnight 1: 6/11 ≈ 55%
#'
#' ## UPA Aggregation Strategy
#'
#' This strategy tests whether aggregating constraints at UPA level (across all
#' households in the same UPA) improves determination rates. This is EXPERIMENTAL
#' because IBGE does NOT interview entire UPAs on the same day - interviews are
#' spread across the quarter.
#'
#' The UPA homogeneity rate (percentage of UPAs where all determined households
#' have the same fortnight/week) is calculated first. Only if this exceeds the
#' threshold is UPA aggregation applied.
#'
#' @note
#' These strategies produce "likely" assignments, not definitive determinations.
#' The standard \code{pnadc_identify_periods()} function should be used for
#' rigorous analysis. These experimental outputs are useful for:
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
#'   strategy = "probabilistic"
#' )
#'
#' # Check how many additional "likely" assignments we get
#' crosswalk_exp[, .(
#'   determined = sum(!is.na(ref_fortnight_in_quarter)),
#'   likely = sum(!is.na(ref_fortnight_likely)),
#'   high_confidence = sum(ref_fortnight_confidence >= 0.7, na.rm = TRUE)
#' )]
#' }
#'
#' @export
pnadc_experimental_periods <- function(
    crosswalk,
    data = NULL,
    strategy = c("none", "probabilistic", "upa_aggregation", "both"),
    upa_homogeneity_threshold = 0.80,
    verbose = TRUE
) {

  strategy <- match.arg(strategy)

  if (strategy == "none") {
    if (verbose) cat("No experimental strategy applied.\n")
    return(crosswalk)
  }

  # Copy crosswalk to avoid modifying original
  result <- data.table::copy(crosswalk)

  # ==========================================================================
  # PROBABILISTIC STRATEGY
  # ==========================================================================

  if (strategy %in% c("probabilistic", "both")) {
    if (verbose) cat("Applying probabilistic strategy...\n")

    if (is.null(data)) {
      warning("Probabilistic strategy requires original data to compute date bounds. ",
              "Skipping probabilistic strategy.")
    } else {
      result <- apply_probabilistic_strategy(result, data, verbose)
    }
  }

  # ==========================================================================
  # UPA AGGREGATION STRATEGY
  # ==========================================================================

  if (strategy %in% c("upa_aggregation", "both")) {
    if (verbose) cat("Applying UPA aggregation strategy...\n")
    result <- apply_upa_aggregation_strategy(result, upa_homogeneity_threshold, verbose)
  }

  result
}


#' Apply Probabilistic Strategy
#'
#' @param crosswalk Crosswalk data.table
#' @param data Original PNADC data
#' @param verbose Print progress
#' @return Modified crosswalk with probabilistic columns
#' @keywords internal
#' @noRd
apply_probabilistic_strategy <- function(crosswalk, data, verbose) {

  # We need to recalculate date bounds to get the range information
  # This is expensive, so we only do it for undetermined periods

  if (verbose) cat("  Calculating date bounds for probabilistic assignment...\n")

  # Convert to data.table
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

  # Calculate date bounds (simplified - using standard rule only)
  unique_quarters <- unique(dt[, .(Ano, Trimestre)])
  unique_quarters[, `:=`(
    month1 = quarter_month_n(Trimestre, 1L),
    month3 = quarter_month_n(Trimestre, 3L)
  )]
  unique_quarters[, `:=`(
    first_sat_m1 = first_valid_saturday(Ano, month1, min_days = 4L),
    first_sat_m3 = first_valid_saturday(Ano, month3, min_days = 4L)
  )]

  dt[unique_quarters, on = .(Ano, Trimestre), `:=`(
    month1 = i.month1, month3 = i.month3,
    first_sat_m1 = i.first_sat_m1, first_sat_m3 = i.first_sat_m3
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

  # Aggregate at household level
  # Handle all-NA groups by checking before aggregation to avoid -Inf/Inf
  dt[, `:=`(
    hh_date_min = fifelse(all(is.na(date_min)), as.Date(NA), max(date_min, na.rm = TRUE)),
    hh_date_max = fifelse(all(is.na(date_max)), as.Date(NA), min(date_max, na.rm = TRUE))
  ), by = .(Ano, Trimestre, UPA, V1008)]

  # Calculate fortnight range
  dt[, `:=`(
    hh_fortnight_min = date_to_fortnight_in_quarter(hh_date_min, Trimestre),
    hh_fortnight_max = date_to_fortnight_in_quarter(hh_date_max, Trimestre)
  )]

  # Get bounds at household level
  bounds <- unique(dt[, .(Ano, Trimestre, UPA, V1008, V1014,
                          hh_date_min, hh_date_max,
                          hh_fortnight_min, hh_fortnight_max)])

  # Calculate fortnight range
  bounds[, fortnight_range := hh_fortnight_max - hh_fortnight_min + 1L]

  # Initialize likely columns in crosswalk
  crosswalk[, `:=`(
    ref_fortnight_likely = NA_integer_,
    ref_fortnight_confidence = NA_real_,
    ref_week_likely = NA_integer_,
    ref_week_confidence = NA_real_
  )]

  # Join bounds to crosswalk
  crosswalk[bounds, on = .(Ano, Trimestre, UPA, V1008, V1014), `:=`(
    hh_date_min = i.hh_date_min,
    hh_date_max = i.hh_date_max,
    hh_fortnight_min = i.hh_fortnight_min,
    hh_fortnight_max = i.hh_fortnight_max,
    fortnight_range = i.fortnight_range
  )]

  # For range == 2, calculate probability
  # The fortnight boundary is at day 15/16 of the month
  if (verbose) cat("  Calculating probabilistic assignments for narrow ranges...\n")

  # Calculate the midpoint of the date range as a proxy for likely interview date
  crosswalk[fortnight_range == 2L & is.na(ref_fortnight_in_quarter), `:=`(
    date_midpoint = hh_date_min + as.integer((hh_date_max - hh_date_min) / 2)
  )]

  # Determine which fortnight the midpoint falls in
  crosswalk[fortnight_range == 2L & is.na(ref_fortnight_in_quarter), `:=`(
    midpoint_day = fast_mday(date_midpoint),
    ref_fortnight_likely = fifelse(fast_mday(date_midpoint) <= 15L,
                                   hh_fortnight_min, hh_fortnight_max)
  )]

  # Confidence is based on how far the midpoint is from the boundary
  # (closer to boundary = lower confidence, closer to edges = higher confidence)
  crosswalk[fortnight_range == 2L & is.na(ref_fortnight_in_quarter), `:=`(
    # Calculate distance from day 15 (boundary), normalized to 0-1
    boundary_distance = abs(midpoint_day - 15.5) / 15.5,
    ref_fortnight_confidence = 0.5 + abs(midpoint_day - 15.5) / 31
  )]

  # Cap confidence at reasonable levels
  crosswalk[!is.na(ref_fortnight_confidence),
            ref_fortnight_confidence := pmin(pmax(ref_fortnight_confidence, 0.5), 0.95)]

  # --------------------------------------------------------------------------
  # WEEK PROBABILISTIC STRATEGY
  # For weeks, we use the date range to estimate which week is most likely
  # --------------------------------------------------------------------------

  # Calculate week range (using YYYYWW format converted to sequential week count)
  # We need to compute week bounds again from the crosswalk's hh_date_min/max
  crosswalk[!is.na(hh_date_min) & !is.na(hh_date_max), `:=`(
    hh_week_min_yyyyww = date_to_yyyyww(hh_date_min),
    hh_week_max_yyyyww = date_to_yyyyww(hh_date_max)
  )]

  # Convert YYYYWW to a sequential week number for range calculation
  # (approximate - using year * 53 + week)
  crosswalk[!is.na(hh_week_min_yyyyww), `:=`(
    hh_week_min_seq = (hh_week_min_yyyyww %/% 100L) * 53L + (hh_week_min_yyyyww %% 100L),
    hh_week_max_seq = (hh_week_max_yyyyww %/% 100L) * 53L + (hh_week_max_yyyyww %% 100L)
  )]

  crosswalk[!is.na(hh_week_min_seq), week_range := hh_week_max_seq - hh_week_min_seq + 1L]

  # For week_range == 2, assign the week containing the date midpoint
  crosswalk[week_range == 2L & is.na(ref_week_in_quarter) & !is.na(date_midpoint), `:=`(
    midpoint_week_yyyyww = date_to_yyyyww(date_midpoint)
  )]

  crosswalk[week_range == 2L & is.na(ref_week_in_quarter) & !is.na(midpoint_week_yyyyww), `:=`(
    ref_week_likely = week_in_quarter(yyyyww_to_date(midpoint_week_yyyyww), Trimestre, Ano)
  )]

  # Confidence for weeks: based on position within the date range
  # If midpoint is closer to one week boundary, higher confidence
  crosswalk[week_range == 2L & is.na(ref_week_in_quarter), `:=`(
    week_boundary_distance = fifelse(!is.na(hh_week_min_seq) & !is.na(hh_week_max_seq),
                                      abs((hh_week_max_seq - hh_week_min_seq) / 2 - 0.5),
                                      NA_real_),
    ref_week_confidence = 0.5 + fifelse(!is.na(date_midpoint),
                                         abs(as.numeric(date_midpoint - hh_date_min) /
                                             as.numeric(hh_date_max - hh_date_min) - 0.5),
                                         NA_real_)
  )]

  # Cap week confidence at reasonable levels
  crosswalk[!is.na(ref_week_confidence),
            ref_week_confidence := pmin(pmax(ref_week_confidence, 0.5), 0.95)]

  # Clean up temporary columns
  temp_cols <- c("hh_date_min", "hh_date_max", "hh_fortnight_min", "hh_fortnight_max",
                 "fortnight_range", "date_midpoint", "midpoint_day", "boundary_distance",
                 "hh_week_min_yyyyww", "hh_week_max_yyyyww",
                 "hh_week_min_seq", "hh_week_max_seq", "week_range",
                 "midpoint_week_yyyyww", "week_boundary_distance")
  crosswalk[, (intersect(temp_cols, names(crosswalk))) := NULL]

  if (verbose) {
    n_likely_fortnight <- sum(!is.na(crosswalk$ref_fortnight_likely))
    n_undetermined_fortnight <- sum(is.na(crosswalk$ref_fortnight_in_quarter))
    pct_fortnight <- if (n_undetermined_fortnight > 0) n_likely_fortnight / n_undetermined_fortnight * 100 else 0

    n_likely_week <- sum(!is.na(crosswalk$ref_week_likely))
    n_undetermined_week <- sum(is.na(crosswalk$ref_week_in_quarter))
    pct_week <- if (n_undetermined_week > 0) n_likely_week / n_undetermined_week * 100 else 0

    cat(sprintf("  Probabilistic fortnight: %s assignments (%.1f%% of undetermined)\n",
                format(n_likely_fortnight, big.mark = ","), pct_fortnight))
    cat(sprintf("  Probabilistic week: %s assignments (%.1f%% of undetermined)\n",
                format(n_likely_week, big.mark = ","), pct_week))
  }

  crosswalk
}


#' Apply UPA Aggregation Strategy
#'
#' @param crosswalk Crosswalk data.table
#' @param threshold Minimum UPA homogeneity rate to apply aggregation
#' @param verbose Print progress
#' @return Modified crosswalk with UPA-aggregated columns
#' @keywords internal
#' @noRd
apply_upa_aggregation_strategy <- function(crosswalk, threshold, verbose) {

  # First, calculate UPA homogeneity rate
  if (verbose) cat("  Calculating UPA homogeneity rate...\n")

  # Fortnight homogeneity
  upa_fortnight <- crosswalk[!is.na(ref_fortnight_in_quarter), .(
    n_unique = uniqueN(ref_fortnight_in_quarter),
    n_households = .N
  ), by = .(Ano, Trimestre, UPA)]

  upa_homogeneity_fortnight <- mean(upa_fortnight$n_unique == 1)

  if (verbose) {
    cat(sprintf("  UPA homogeneity (fortnight): %.1f%%\n", upa_homogeneity_fortnight * 100))
  }

  # Initialize UPA columns
  crosswalk[, `:=`(
    ref_fortnight_upa = NA_integer_,
    ref_week_upa = NA_integer_
  )]

  if (upa_homogeneity_fortnight < threshold) {
    if (verbose) {
      cat(sprintf("  UPA homogeneity %.1f%% below threshold %.1f%%. ",
                  upa_homogeneity_fortnight * 100, threshold * 100))
      cat("UPA aggregation NOT applied.\n")
    }
    return(crosswalk)
  }

  if (verbose) {
    cat(sprintf("  UPA homogeneity %.1f%% >= threshold %.1f%%. Applying UPA aggregation.\n",
                upa_homogeneity_fortnight * 100, threshold * 100))
  }

  # For UPAs where ALL determined households have the same fortnight,

  # propagate that fortnight to undetermined households in the same UPA
  upa_consensus <- crosswalk[!is.na(ref_fortnight_in_quarter), .(
    consensus_fortnight = fifelse(uniqueN(ref_fortnight_in_quarter) == 1L,
                                  ref_fortnight_in_quarter[1L], NA_integer_)
  ), by = .(Ano, Trimestre, UPA)]

  # Join consensus back to crosswalk
  crosswalk[upa_consensus, on = .(Ano, Trimestre, UPA),
            ref_fortnight_upa := i.consensus_fortnight]

  # Do the same for weeks
  upa_week <- crosswalk[!is.na(ref_week_in_quarter), .(
    n_unique = uniqueN(ref_week_in_quarter)
  ), by = .(Ano, Trimestre, UPA)]

  upa_homogeneity_week <- mean(upa_week$n_unique == 1)

  if (verbose) {
    cat(sprintf("  UPA homogeneity (week): %.1f%%\n", upa_homogeneity_week * 100))
  }

  if (upa_homogeneity_week >= threshold) {
    upa_week_consensus <- crosswalk[!is.na(ref_week_in_quarter), .(
      consensus_week = fifelse(uniqueN(ref_week_in_quarter) == 1L,
                               ref_week_in_quarter[1L], NA_integer_)
    ), by = .(Ano, Trimestre, UPA)]

    crosswalk[upa_week_consensus, on = .(Ano, Trimestre, UPA),
              ref_week_upa := i.consensus_week]
  }

  if (verbose) {
    n_upa_fortnight <- sum(!is.na(crosswalk$ref_fortnight_upa) &
                             is.na(crosswalk$ref_fortnight_in_quarter))
    n_undetermined <- sum(is.na(crosswalk$ref_fortnight_in_quarter))
    # Avoid division by zero
    pct_upa <- if (n_undetermined > 0) n_upa_fortnight / n_undetermined * 100 else 0
    cat(sprintf("  UPA-aggregated fortnight: %s additional assignments (%.1f%% of undetermined)\n",
                format(n_upa_fortnight, big.mark = ","), pct_upa))
  }

  crosswalk
}
