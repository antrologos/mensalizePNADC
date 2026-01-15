#' Semanalize PNADC Quarterly Survey Data
#'
#' Main function to convert quarterly PNADC survey data to weekly time series.
#' Returns a crosswalk data.frame for joining with original data, adding
#' reference week information and optionally weekly survey weights.
#'
#' @description
#' This function processes stacked quarterly PNADC microdata to:
#' \enumerate{
#'   \item Identify which ISO 8601 week each observation refers to
#'   \item Optionally compute weekly adjusted survey weights (if \code{compute_weights = TRUE})
#' }
#'
#' The output is a crosswalk table that can be joined with original (stacked or unstacked)
#' PNADC data files to add weekly time information.
#'
#' @param data A data.frame or data.table with stacked quarterly PNADC microdata.
#'
#'   For reference week identification only (\code{compute_weights = FALSE}),
#'   minimum required columns are:
#'   \itemize{
#'     \item \code{Ano}, \code{Trimestre}: Year and quarter
#'     \item \code{UPA}, \code{V1008}, \code{V1014}: Primary sampling unit, household, panel
#'     \item \code{V2008}, \code{V20081}, \code{V20082}: Birth day, month, year
#'     \item \code{V2009}: Age
#'   }
#'
#'   For weekly weight computation (\code{compute_weights = TRUE}), additional
#'   columns are required. See Details.
#'
#' @param compute_weights Logical. If TRUE, compute weekly adjusted survey weights in
#'   addition to identifying reference weeks. Default is FALSE. Requires the
#'   \code{sidrar} package to fetch population data from IBGE SIDRA API.
#'   Note: Weekly weights are interpolated from monthly SIDRA data (Table 6022)
#'   because IBGE does not publish official weekly population estimates.
#'
#' @param output Character. What to return:
#'   \itemize{
#'     \item \code{"crosswalk"} (default): Minimal crosswalk for joining
#'     \item \code{"microdata"}: Full microdata with all computed columns
#'   }
#'
#' @param keep_all Logical. If TRUE (default), return all rows from input data,
#'   with \code{weight_weekly = NA} for observations where reference week
#'   could not be determined. If FALSE, only return observations with
#'   determined reference weeks (smaller output). Only applies when
#'   \code{compute_weights = TRUE}.
#'
#' @param verbose Logical. Print progress messages? Default TRUE.
#'
#' @return Depends on \code{output} parameter:
#'
#'   If \code{output = "crosswalk"} (default):
#'   A data.table with join keys and new time variables:
#'   \itemize{
#'     \item Join keys: \code{Ano}, \code{Trimestre}, \code{UPA}, \code{V1008}, \code{V1014}, \code{V2003}
#'     \item \code{ref_week}: Reference week as Date (Monday of that week)
#'     \item \code{ref_week_in_quarter}: Week position in quarter (1-14) or NA
#'     \item \code{ref_week_iso_yyyyww}: Integer YYYYWW format (ISO year-week)
#'     \item \code{weight_weekly}: Weekly weight (if \code{compute_weights = TRUE})
#'   }
#'
#'   If \code{output = "microdata"}:
#'   Full input data with all computed columns added.
#'
#' @details
#' ## Reference Week Identification
#'
#' The algorithm determines which ISO 8601 week each survey response refers to based on:
#' \itemize{
#'   \item IBGE's "Parada Tecnica" rules for reference week timing
#'   \item Respondent birthdates (constrains possible interview dates)
#'   \item Household-level aggregation (all persons in same household interviewed same day)
#' }
#'
#' ## Household Aggregation
#'
#' Unlike monthly identification (which aggregates by UPA-V1014 across all quarters),
#' weekly identification aggregates by household (UPA + V1008) WITHIN each quarter.
#' This is because:
#' \itemize{
#'   \item All persons in the same household are interviewed on the same day
#'   \item Households ARE interviewed in the same month across quarterly visits
#'   \item BUT they are NOT interviewed in the same week across quarterly visits
#' }
#'
#' ## Expected Determination Rate
#'
#' The determination rate for weeks (~50-75%) is lower than for months (~97%) because:
#' \itemize{
#'   \item Cannot aggregate constraints across quarters (no cross-quarter week consistency)
#'   \item Finer granularity (13-14 weeks vs 3 months per quarter)
#' }
#'
#' Household aggregation improves the rate compared to individual-level processing.
#'
#' ## ISO 8601 Weeks
#'
#' Weeks follow the ISO 8601 standard:
#' \itemize{
#'   \item Week 1 is the week containing January 4th
#'   \item Weeks start on Monday
#'   \item December 31 may be in week 1 of the following year
#'   \item January 1-3 may be in week 52/53 of the previous year
#' }
#'
#' ## Weekly Weight Computation
#'
#' When \code{compute_weights = TRUE}, the function:
#' \enumerate{
#'   \item Fetches monthly population from IBGE SIDRA API (table 6022)
#'   \item Distributes monthly population equally across weeks in each month
#'   \item Applies hierarchical rake weighting to adjust individual weights
#' }
#'
#' **Important:** Weekly weights are derived from monthly SIDRA data because IBGE
#' does not publish official weekly population estimates. The weekly population
#' targets are approximations.
#'
#' Additional required columns for weight computation:
#' \itemize{
#'   \item Survey design: \code{V1028}, \code{V1008}, \code{V2003}, \code{UF}, \code{posest}, \code{posest_sxi}
#' }
#'
#' @section Dependencies:
#' When \code{compute_weights = TRUE}, the \code{sidrar} package is required
#' to fetch population data. Install with: \code{install.packages("sidrar")}
#'
#' @examples
#' \dontrun{
#' # Basic usage: identify reference weeks only
#' library(mensalizePNADC)
#'
#' # Load stacked quarterly data (minimum columns)
#' pnadc <- data.table::fread("pnadc_stacked.csv",
#'   select = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
#'              "V2008", "V20081", "V20082", "V2009"))
#'
#' # Get crosswalk
#' crosswalk <- semanalizePNADC(pnadc)
#'
#' # Join with original quarterly file
#' original <- haven::read_dta("PNADC_2023T1.dta")
#' weekly <- dplyr::left_join(original, crosswalk,
#'   by = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003"))
#'
#'
#' # With weekly weights
#' # (requires sidrar package and internet connection)
#' pnadc_full <- haven::read_dta("PNADCtrimestralempilhada.dta")
#'
#' result <- semanalizePNADC(pnadc_full, compute_weights = TRUE)
#'
#' # Use weight_weekly for weekly aggregates
#' result[, .(pop = sum(weight_weekly)), by = ref_week_iso_yyyyww]
#' }
#'
#' @seealso
#' \code{\link{identify_reference_week}} for just reference week identification
#' \code{\link{mensalizePNADC}} for monthly identification (higher determination rate)
#'
#' @export
semanalizePNADC <- function(data,
                             compute_weights = FALSE,
                             keep_all = TRUE,
                             output = c("crosswalk", "microdata"),
                             verbose = TRUE) {

  # Validate arguments
  output <- match.arg(output)
  checkmate::assert_logical(compute_weights, len = 1)
  checkmate::assert_logical(keep_all, len = 1)
  checkmate::assert_logical(verbose, len = 1)

  # Validate input data columns FIRST (fail fast)
  # Use same validation as monthly - same columns needed
  validate_pnadc(data, check_weights = compute_weights, stop_on_error = TRUE)

  # Ensure V1008 (household) is present - required for weekly aggregation
  if (!"V1008" %in% names(data)) {
    stop("V1008 (household identifier) is required for weekly identification.\n",
         "  Weekly aggregation uses household-level constraints.",
         call. = FALSE)
  }

  # Determine number of steps for progress bar
  # Basic: 7 steps (identify_reference_week internal steps)
  # With weights: 7 + 2 = 9 steps (add SIDRA fetch, calibrate)
  n_steps <- if (compute_weights) 9L else 7L

  # Initialize progress bar
  if (verbose) {
    cat("Processing PNADC data for weekly identification...\n")
    pb <- txtProgressBar(min = 0, max = n_steps, style = 3)
  } else {
    pb <- NULL
  }

  # Step 1-7: Identify reference weeks
  crosswalk <- identify_reference_week(data, verbose = FALSE, .pb = pb, .pb_offset = 0L)
  gc()

  det_rate <- attr(crosswalk, "determination_rate")

  if (!compute_weights) {
    if (verbose) {
      close(pb)
      cat(sprintf("  Determination rate: %.1f%%\n", det_rate * 100))
    }

    if (output == "crosswalk") {
      return(crosswalk)
    } else {
      # output == "microdata"
      dt <- ensure_data_table(data, copy = TRUE)
      key_cols <- intersect(join_key_vars(), names(dt))
      data.table::setkeyv(crosswalk, key_cols)
      data.table::setkeyv(dt, key_cols)
      dt[crosswalk, on = key_cols,
         `:=`(ref_week = i.ref_week,
              ref_week_in_quarter = i.ref_week_in_quarter,
              ref_week_iso_yyyyww = i.ref_week_iso_yyyyww)]
      return(dt)
    }
  }

  # Steps 8-9: Weight computation pipeline

  # Step 8: Fetch monthly population from SIDRA and convert to weekly targets
  monthly_totals <- fetch_monthly_population(verbose = FALSE)
  weekly_totals <- monthly_to_weekly_targets(monthly_totals)

  if (verbose) setTxtProgressBar(pb, 8)

  # Step 9: Calibrate weights
  dt <- ensure_data_table(data, copy = TRUE)

  # Ensure consistent types for join keys
  if (is.character(dt$Ano)) dt[, Ano := as.integer(Ano)]
  if (is.character(dt$Trimestre)) dt[, Trimestre := as.integer(Trimestre)]

  key_cols <- intersect(join_key_vars(), names(dt))
  data.table::setkeyv(crosswalk, key_cols)
  data.table::setkeyv(dt, key_cols)
  dt[crosswalk, on = key_cols,
     `:=`(ref_week = i.ref_week,
          ref_week_in_quarter = i.ref_week_in_quarter,
          ref_week_iso_yyyyww = i.ref_week_iso_yyyyww)]

  # Calibrate weights using hierarchical rake weighting
  dt <- calibrate_weekly_weights(dt, weekly_totals, keep_all = keep_all, verbose = FALSE)
  gc()

  if (verbose) {
    setTxtProgressBar(pb, 9)
    close(pb)
    cat(sprintf("  Determination rate: %.1f%%\n", det_rate * 100))
    n_with_weights <- sum(!is.na(dt$weight_weekly))
    cat(sprintf("  Observations with weekly weights: %s\n",
                format(n_with_weights, big.mark = ",")))
  }

  # Return based on output type
  if (output == "crosswalk") {
    output_cols <- c(key_cols, "ref_week", "ref_week_in_quarter",
                     "ref_week_iso_yyyyww", "weight_weekly")
    output_cols <- intersect(output_cols, names(dt))
    result <- dt[, ..output_cols]
    attr(result, "determination_rate") <- det_rate
    return(result)
  } else {
    # output == "microdata"
    return(dt)
  }
}


#' Convert Monthly Population Targets to Weekly
#'
#' Distributes monthly SIDRA population targets to weekly targets.
#' Monthly population is divided equally across weeks in that month.
#'
#' @param monthly_totals data.table from fetch_monthly_population() with
#'   ref_month_yyyymm and m_populacao columns
#' @return data.table with ref_week_iso_yyyyww and w_populacao (weekly population in thousands)
#' @keywords internal
#' @noRd
monthly_to_weekly_targets <- function(monthly_totals) {
  # Create a mapping of all weeks to their containing month(s)
  # Most weeks are fully within one month; boundary weeks span two months

  # Get date range from monthly data
  min_yyyymm <- min(monthly_totals$ref_month_yyyymm, na.rm = TRUE)
  max_yyyymm <- max(monthly_totals$ref_month_yyyymm, na.rm = TRUE)

  min_year <- min_yyyymm %/% 100L
  min_month <- min_yyyymm %% 100L
  max_year <- max_yyyymm %/% 100L
  max_month <- max_yyyymm %% 100L

  # Create sequence of all dates in range
  start_date <- make_date(min_year, min_month, 1L)
  # Last day of max month
  days_in_month <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
  last_day <- days_in_month[max_month]
  if (max_month == 2L && is_leap_year(max_year)) last_day <- 29L
  end_date <- make_date(max_year, max_month, last_day)

  # Generate all dates
  all_dates <- seq.Date(start_date, end_date, by = "day")

  # Create data.table with date info
  dt <- data.table::data.table(
    date = all_dates,
    yyyymm = yyyymm(fast_year(all_dates), fast_month(all_dates)),
    yyyyww = date_to_yyyyww(all_dates)
  )

  # Count days per week-month combination
  week_month <- dt[, .(n_days = .N), by = .(yyyyww, yyyymm)]

  # Join with monthly population
  week_month[monthly_totals, on = .(yyyymm = ref_month_yyyymm),
             m_populacao := i.m_populacao]

  # Calculate weekly population as weighted average based on days
  # For boundary weeks, allocate proportionally
  weekly_pop <- week_month[!is.na(m_populacao), .(
    w_populacao = sum(m_populacao * n_days / 7, na.rm = TRUE)
  ), by = yyyyww]

  # Rename for consistency
  data.table::setnames(weekly_pop, "yyyyww", "ref_week_iso_yyyyww")

  weekly_pop
}
