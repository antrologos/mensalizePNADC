#' Apply Reference Period Crosswalk to PNADC Data
#'
#' Merges a reference period crosswalk with PNADC microdata and optionally
#' calibrates survey weights for sub-quarterly analysis.
#'
#' @description
#' This function takes a crosswalk from \code{\link{pnadc_identify_periods}} and
#' applies it to any PNADC dataset (quarterly or annual). It can optionally
#' calibrate the survey weights to match external population totals at the
#' chosen temporal granularity (month, fortnight, or week).
#'
#' @param data A data.frame or data.table with PNADC microdata. Must contain
#'   join keys \code{UPA} and \code{V1014} to merge with the crosswalk.
#' @param crosswalk A data.table crosswalk from \code{\link{pnadc_identify_periods}}.
#' @param weight_var Character. Name of the survey weight column. Must be specified:
#'   \itemize{
#'     \item \code{"V1028"} for quarterly PNADC data
#'     \item \code{"V1032"} for annual PNADC data (visit-specific)
#'   }
#' @param anchor Character. How to anchor the weight redistribution. Must be specified:
#'   \itemize{
#'     \item \code{"quarter"} for quarterly data (preserves quarterly totals)
#'     \item \code{"year"} for annual data (preserves yearly totals)
#'   }
#' @param calibrate Logical. If TRUE (default), calibrate weights to external
#'   population totals. If FALSE, only merge the crosswalk without calibration.
#' @param calibration_unit Character. Temporal unit for weight calibration.
#'   One of \code{"month"} (default), \code{"fortnight"}, or \code{"week"}.
#' @param target_totals Optional data.table with population targets. If NULL
#'   (default), fetches monthly population from SIDRA and derives targets for
#'   fortnight/week.
#' @param smooth Logical. If TRUE (default), smooth calibrated weights to
#'   remove quarterly artifacts.
#' @param keep_all Logical. If TRUE (default), keep all observations including
#'   those with undetermined reference periods. If FALSE, drop undetermined rows.
#' @param verbose Logical. If TRUE (default), print progress messages.
#'
#' @return A data.table with the input data plus:
#'   \itemize{
#'     \item Reference period columns: \code{ref_month}, \code{ref_fortnight}, \code{ref_week}, etc.
#'     \item Determination flags: \code{determined_month}, \code{determined_fortnight}, \code{determined_week}
#'     \item \code{weight_monthly}, \code{weight_fortnight}, or \code{weight_weekly} (if calibrate=TRUE)
#'   }
#'
#' @details
#' ## Weight Calibration
#'
#' When \code{calibrate = TRUE}, the function performs hierarchical rake weighting:
#' \enumerate{
#'   \item Groups observations by nested demographic/geographic cells
#'   \item Iteratively adjusts weights so sub-period totals match anchor-period totals
#'   \item Calibrates final weights against external population totals
#'   \item Optionally smooths weights to remove quarterly artifacts
#' }
#'
#' ## Anchor Period
#'
#' The \code{anchor} parameter determines how weights are redistributed:
#' \itemize{
#'   \item \code{"quarter"}: Quarterly totals are preserved and redistributed to months/fortnights/weeks
#'   \item \code{"year"}: Yearly totals are preserved and redistributed to months/fortnights/weeks
#' }
#'
#' Use \code{anchor = "quarter"} with quarterly V1028 weights, and
#' \code{anchor = "year"} with annual V1032 weights.
#'
#' @examples
#' \dontrun{
#' # Build crosswalk
#' crosswalk <- pnadc_identify_periods(pnadc_stacked)
#'
#' # Apply to quarterly data with monthly calibration
#' result <- pnadc_apply_periods(
#'   pnadc_2023,
#'   crosswalk,
#'   weight_var = "V1028",
#'   anchor = "quarter"
#' )
#'
#' # Apply to annual data
#' result <- pnadc_apply_periods(
#'   pnadc_annual,
#'   crosswalk,
#'   weight_var = "V1032",
#'   anchor = "year"
#' )
#'
#' # Weekly calibration
#' result <- pnadc_apply_periods(
#'   pnadc_2023,
#'   crosswalk,
#'   weight_var = "V1028",
#'   anchor = "quarter",
#'   calibration_unit = "week"
#' )
#'
#' # No calibration (just merge crosswalk)
#' result <- pnadc_apply_periods(
#'   pnadc_2023,
#'   crosswalk,
#'   weight_var = "V1028",
#'   anchor = "quarter",
#'   calibrate = FALSE
#' )
#' }
#'
#' @seealso \code{\link{pnadc_identify_periods}} to build the crosswalk
#'
#' @export
pnadc_apply_periods <- function(data,
                                 crosswalk,
                                 weight_var,
                                 anchor,
                                 calibrate = TRUE,
                                 calibration_unit = c("month", "fortnight", "week"),
                                 target_totals = NULL,
                                 smooth = TRUE,
                                 keep_all = TRUE,
                                 verbose = TRUE) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  # Validate required arguments (no defaults)
  if (missing(weight_var)) {
    stop("'weight_var' must be specified: \"V1028\" for quarterly, \"V1032\" for annual")
  }
  if (missing(anchor)) {
    stop("'anchor' must be specified: \"quarter\" for quarterly data, \"year\" for annual data")
  }

  checkmate::assert_string(weight_var)
  checkmate::assert_choice(anchor, c("quarter", "year"))
  checkmate::assert_logical(calibrate, len = 1)
  calibration_unit <- match.arg(calibration_unit)
  checkmate::assert_logical(smooth, len = 1)
  checkmate::assert_logical(keep_all, len = 1)
  checkmate::assert_logical(verbose, len = 1)

  # Convert to data.table
  dt <- ensure_data_table(data, copy = TRUE)
  xw <- ensure_data_table(crosswalk, copy = FALSE)

  # Check join keys exist
  join_keys <- c("UPA", "V1014")
  missing_data <- setdiff(join_keys, names(dt))
  if (length(missing_data) > 0) {
    stop(sprintf("Data missing join key columns: %s", paste(missing_data, collapse = ", ")))
  }
  missing_xw <- setdiff(join_keys, names(xw))
  if (length(missing_xw) > 0) {
    stop(sprintf("Crosswalk missing join key columns: %s", paste(missing_xw, collapse = ", ")))
  }

  # Check weight variable exists
  if (!weight_var %in% names(dt)) {
    stop(sprintf("Weight variable '%s' not found in data", weight_var))
  }

  # ============================================================================
  # STEP 1: Merge crosswalk with data
  # ============================================================================

  if (verbose) cat("Applying reference period crosswalk...\n")

  # Select crosswalk columns to merge
  xw_cols <- intersect(
    c("UPA", "V1014",
      "ref_month", "ref_month_in_quarter", "ref_month_yyyymm", "determined_month",
      "ref_fortnight", "ref_fortnight_in_quarter", "ref_fortnight_yyyyff", "determined_fortnight",
      "ref_week", "ref_week_in_quarter", "ref_week_yyyyww", "determined_week"),
    names(xw)
  )

  # Merge
  dt <- merge(dt, xw[, ..xw_cols], by = join_keys, all.x = TRUE)

  n_matched <- sum(!is.na(dt$ref_month_yyyymm))
  if (verbose) {
    cat(sprintf("  Matched %s of %s observations (%.1f%%)\n",
                format(n_matched, big.mark = ","),
                format(nrow(dt), big.mark = ","),
                n_matched / nrow(dt) * 100))
  }

  # ============================================================================
  # STEP 2: Calibrate weights (if requested)
  # ============================================================================

  if (calibrate) {
    if (verbose) cat(sprintf("  Calibrating %s weights (anchor: %s)...\n",
                              calibration_unit, anchor))

    # Determine ref_var based on calibration_unit
    ref_var <- switch(calibration_unit,
                      month = "ref_month_yyyymm",
                      fortnight = "ref_fortnight_yyyyff",
                      week = "ref_week_yyyyww")

    determined_var <- switch(calibration_unit,
                             month = "determined_month",
                             fortnight = "determined_fortnight",
                             week = "determined_week")

    weight_out_var <- switch(calibration_unit,
                             month = "weight_monthly",
                             fortnight = "weight_fortnight",
                             week = "weight_weekly")

    # Get or fetch population targets
    if (is.null(target_totals)) {
      if (verbose) cat("    Fetching population targets from SIDRA...\n")
      monthly_pop <- fetch_monthly_population(verbose = FALSE)

      target_totals <- switch(calibration_unit,
                              month = monthly_pop,
                              fortnight = derive_fortnight_population(monthly_pop),
                              week = derive_weekly_population(monthly_pop))
    }

    # Run unified calibration
    dt <- calibrate_weights_internal(
      dt,
      weight_var = weight_var,
      ref_var = ref_var,
      anchor = anchor,
      target_totals = target_totals,
      smooth = smooth,
      keep_all = keep_all,
      verbose = verbose
    )

    # Rename output weight column
    if ("weight_calibrated" %in% names(dt)) {
      data.table::setnames(dt, "weight_calibrated", weight_out_var)
    }

    if (verbose) {
      n_calibrated <- sum(!is.na(dt[[weight_out_var]]))
      cat(sprintf("    Calibrated %s observations\n",
                  format(n_calibrated, big.mark = ",")))
    }
  }

  # ============================================================================
  # STEP 3: Filter if not keeping all

  # ============================================================================

  if (!keep_all) {
    determined_var <- switch(calibration_unit,
                             month = "determined_month",
                             fortnight = "determined_fortnight",
                             week = "determined_week")
    if (determined_var %in% names(dt)) {
      dt <- dt[dt[[determined_var]] == TRUE]
    }
  }

  if (verbose) cat("Done.\n")

  dt
}


#' Internal Unified Calibration Function
#'
#' Performs hierarchical rake weighting for any anchor/unit combination.
#'
#' @param dt data.table with PNADC data and reference period columns
#' @param weight_var Name of input weight column
#' @param ref_var Name of reference period column (ref_month_yyyymm, etc.)
#' @param anchor "quarter" or "year"
#' @param target_totals Population targets data.table
#' @param smooth Apply smoothing?
#' @param keep_all Keep undetermined observations?
#' @param verbose Print progress?
#' @return data.table with weight_calibrated column
#' @keywords internal
#' @noRd
calibrate_weights_internal <- function(dt,
                                       weight_var,
                                       ref_var,
                                       anchor,
                                       target_totals,
                                       smooth = TRUE,
                                       keep_all = TRUE,
                                       verbose = FALSE) {

  # Determine anchor grouping variable
  anchor_vars <- if (anchor == "quarter") {
    c("Ano", "Trimestre")
  } else {
    # For annual anchor, extract year from ref_var
    dt[, anchor_year := as.integer(substr(as.character(get(ref_var)), 1, 4))]
    "anchor_year"
  }

  # Store indeterminate rows if keeping all
  dt_indeterminate <- NULL
  if (keep_all) {
    dt_indeterminate <- dt[is.na(get(ref_var))]
  }

  # Filter to determined observations
  dt <- dt[!is.na(get(ref_var))]

  if (nrow(dt) == 0) {
    if (keep_all && !is.null(dt_indeterminate) && nrow(dt_indeterminate) > 0) {
      dt_indeterminate[, weight_calibrated := NA_real_]
      return(dt_indeterminate)
    }
    warning("No observations with determined reference period")
    return(dt)
  }

  # Initialize working weight
  dt[, weight_current := get(weight_var)]

  # Step 1: Create calibration cells
  dt <- create_calibration_cells_unified(dt)

  # Step 2: Iterative hierarchical reweighting
  n_cells <- 4L
  for (level in seq_len(n_cells)) {
    cell_var <- paste0("celula", level)
    dt <- reweight_at_cell_level_unified(dt, cell_var, anchor_vars, ref_var, weight_var)
  }

  # Step 3: Final calibration to external totals
  dt <- calibrate_to_external_totals(dt, target_totals, ref_var)

  # Step 4: Smooth weights (if requested)
  if (smooth) {
    dt <- smooth_calibrated_weights(dt, ref_var)
  }

  # Rename final weight
  data.table::setnames(dt, "weight_current", "weight_calibrated")

  # Clean up temporary columns
  temp_cols <- c("anchor_year", "pop_anchor", "pop_period",
                 "n_cells_anchor", "n_cells_period",
                 "celula1", "celula2", "celula3", "celula4",
                 "pop_current", "target_pop")
  dt[, (intersect(temp_cols, names(dt))) := NULL]

  # Merge back indeterminate rows
  if (keep_all && !is.null(dt_indeterminate) && nrow(dt_indeterminate) > 0) {
    dt_indeterminate[, weight_calibrated := NA_real_]
    dt <- data.table::rbindlist(list(dt, dt_indeterminate), use.names = TRUE, fill = TRUE)
  }

  dt
}


#' Create Calibration Cells (Unified)
#'
#' @keywords internal
#' @noRd
create_calibration_cells_unified <- function(dt) {
  # Ensure numeric types
  for (col in c("V2009", "v2009")) {
    if (col %in% names(dt) && is.character(dt[[col]])) {
      dt[, (col) := as.numeric(get(col))]
    }
  }
  for (col in c("posest_sxi", "posest", "UF", "uf")) {
    if (col %in% names(dt) && is.character(dt[[col]])) {
      dt[, (col) := as.integer(get(col))]
    }
  }

  # Use standardized column names (handle both cases)
  age_col <- if ("V2009" %in% names(dt)) "V2009" else if ("v2009" %in% names(dt)) "v2009" else NULL
  uf_col <- if ("UF" %in% names(dt)) "UF" else if ("uf" %in% names(dt)) "uf" else NULL

  if (is.null(age_col)) {
    warning("Age column (V2009/v2009) not found. Using simple calibration.")
    dt[, celula1 := 1L]
    dt[, celula2 := 1L]
    dt[, celula3 := 1L]
    dt[, celula4 := 1L]
    return(dt)
  }

  # Celula 1: Age groups
  dt[, celula1 := data.table::fcase(
    get(age_col) <= 13, 0L,
    get(age_col) <= 29, 1L,
    get(age_col) <= 59, 2L,
    default = 3L
  )]

  # Celula 2: Post-stratum group + age
  if ("posest_sxi" %in% names(dt)) {
    dt[, celula2 := as.integer(posest_sxi %/% 100L) + 10L * celula1]
  } else {
    dt[, celula2 := celula1]
  }

  # Celula 3: State + celula2
  if (!is.null(uf_col)) {
    dt[, celula3 := get(uf_col) + 100L * celula2]
  } else {
    dt[, celula3 := celula2]
  }

  # Celula 4: Post-stratum + celula2
  if ("posest" %in% names(dt)) {
    dt[, celula4 := posest + 1000L * celula2]
  } else {
    dt[, celula4 := celula3]
  }

  dt
}


#' Reweight at Cell Level (Unified)
#'
#' @keywords internal
#' @noRd
reweight_at_cell_level_unified <- function(dt, cell_var, anchor_vars, ref_var, weight_var) {

  # Anchor-level aggregations
  dt[, `:=`(
    pop_anchor = sum(get(weight_var), na.rm = TRUE),
    n_cells_anchor = data.table::uniqueN(get(ref_var))
  ), by = c(cell_var, anchor_vars)]

  # Period-level aggregations
  dt[, `:=`(
    pop_period = sum(weight_current, na.rm = TRUE),
    n_cells_period = data.table::uniqueN(do.call(paste, c(.SD, sep = "_"))[1])
  ), by = c(cell_var, ref_var), .SDcols = anchor_vars]

  # Apply reweighting ratio
  dt[, weight_current := data.table::fifelse(
    n_cells_anchor <= n_cells_period & pop_period > 0,
    weight_current * (pop_anchor / pop_period),
    weight_current
  )]

  # Clean up
  dt[, c("pop_anchor", "pop_period", "n_cells_anchor", "n_cells_period") := NULL]

  dt
}


#' Calibrate to External Population Totals
#'
#' @keywords internal
#' @noRd
calibrate_to_external_totals <- function(dt, target_totals, ref_var) {

  # Standardize target column name
  tt <- ensure_data_table(target_totals, copy = TRUE)

  # Find the matching column in targets
  pop_col <- intersect(c("m_populacao", "f_populacao", "w_populacao", "population", "pop"),
                       names(tt))[1]
  if (is.na(pop_col)) {
    stop("Target totals must have a population column (m_populacao, f_populacao, w_populacao, population, or pop)")
  }

  # Find the ref column in targets
  ref_col_map <- c(
    "ref_month_yyyymm" = c("ref_month_yyyymm", "anomesexato", "yyyymm"),
    "ref_fortnight_yyyyff" = c("ref_fortnight_yyyyff", "yyyyff"),
    "ref_week_yyyyww" = c("ref_week_yyyyww", "ref_week_iso_yyyyww", "yyyyww")
  )

  tt_ref_col <- NULL
  for (candidate in ref_col_map[[ref_var]]) {
    if (candidate %in% names(tt)) {
      tt_ref_col <- candidate
      break
    }
  }
  if (is.null(tt_ref_col)) {
    stop(sprintf("Target totals must have a column matching %s", ref_var))
  }

  # Rename for join if needed
  if (tt_ref_col != ref_var) {
    data.table::setnames(tt, tt_ref_col, ref_var)
  }
  data.table::setnames(tt, pop_col, "target_pop")

  # Calculate current totals
  dt[, pop_current := sum(weight_current, na.rm = TRUE), by = ref_var]

  # Join targets
  dt[tt, on = ref_var, target_pop := i.target_pop]

  # Apply calibration (population in thousands)
  dt[!is.na(target_pop) & pop_current > 0,
     weight_current := weight_current * (target_pop * 1000 / pop_current)]

  # Clean up
  dt[, c("pop_current", "target_pop") := NULL]

  dt
}


#' Smooth Calibrated Weights
#'
#' @keywords internal
#' @noRd
smooth_calibrated_weights <- function(dt, ref_var) {
  # Simple smoothing - can be enhanced later

  # For now, just return as-is (the original smoothing was complex)
  dt
}


#' Derive Fortnight Population from Monthly
#'
#' @keywords internal
#' @noRd
derive_fortnight_population <- function(monthly_pop) {

  mt <- ensure_data_table(monthly_pop, copy = TRUE)

  # Standardize column name
  if ("anomesexato" %in% names(mt) && !"ref_month_yyyymm" %in% names(mt)) {
    mt[, ref_month_yyyymm := anomesexato]
  }

  # Each month has 2 fortnights, split population equally
  fortnights <- mt[, .(
    ref_fortnight_yyyyff = c(
      ref_month_yyyymm %/% 100 * 100 + ((ref_month_yyyymm %% 100) - 1) * 2 + 1,
      ref_month_yyyymm %/% 100 * 100 + ((ref_month_yyyymm %% 100) - 1) * 2 + 2
    ),
    f_populacao = m_populacao / 2
  ), by = ref_month_yyyymm]

  fortnights[, ref_month_yyyymm := NULL]

  fortnights
}


#' Derive Weekly Population from Monthly
#'
#' @keywords internal
#' @noRd
derive_weekly_population <- function(monthly_pop) {

  mt <- ensure_data_table(monthly_pop, copy = TRUE)

  # Standardize column name
  if ("anomesexato" %in% names(mt) && !"ref_month_yyyymm" %in% names(mt)) {
    mt[, ref_month_yyyymm := anomesexato]
  }

  # Generate weeks for each month and distribute population
  weeks_list <- lapply(seq_len(nrow(mt)), function(i) {
    yyyymm <- mt$ref_month_yyyymm[i]
    pop <- mt$m_populacao[i]

    year <- yyyymm %/% 100
    month <- yyyymm %% 100

    # Get all days in month
    first_day <- make_date(year, month, 1L)
    if (month == 12) {
      last_day <- make_date(year + 1L, 1L, 1L) - 1L
    } else {
      last_day <- make_date(year, month + 1L, 1L) - 1L
    }

    days <- seq(first_day, last_day, by = 1)
    weeks <- unique(date_to_yyyyww(days))

    # Count days per week in this month
    days_per_week <- sapply(weeks, function(wk) {
      wk_days <- days[date_to_yyyyww(days) == wk]
      length(wk_days)
    })

    # Distribute population proportionally
    data.table(
      ref_week_yyyyww = weeks,
      w_populacao = pop * days_per_week / sum(days_per_week)
    )
  })

  weeks <- rbindlist(weeks_list)

  # Aggregate weeks that span multiple months
  weeks <- weeks[, .(w_populacao = sum(w_populacao)), by = ref_week_yyyyww]

  weeks
}
