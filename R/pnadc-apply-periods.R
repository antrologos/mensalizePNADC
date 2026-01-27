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
#'     \item \code{"V1032"} for annual PNADC data (visit-specific or annual releases organized by quarters)
#'   }
#' @param anchor Character. How to anchor the weight redistribution. Must be specified:
#'   \itemize{
#'     \item \code{"quarter"} for quarterly data or annual releases organized by quarters (preserves quarterly totals)
#'     \item \code{"year"} for annual visit-specific data (preserves yearly totals)
#'   }
#' @param calibrate Logical. If TRUE (default), calibrate weights to external
#'   population totals. If FALSE, only merge the crosswalk without calibration.
#' @param calibration_unit Character. Temporal unit for weight calibration.
#'   One of \code{"month"} (default), \code{"fortnight"}, or \code{"week"}.
#' @param target_totals Optional data.table with population targets. If NULL
#'   (default), fetches monthly population from SIDRA and derives targets for
#'   fortnight/week. Each time period (month, fortnight, or week) is calibrated
#'   to the FULL Brazilian population from SIDRA.
#'
#'   If providing custom targets, the population column (\code{m_populacao} for
#'   months, \code{f_populacao} for fortnights, \code{w_populacao} for weeks)
#'   must be in **thousands**. The function multiplies by 1000 internally.
#' @param smooth Logical. If TRUE (default), smooth calibrated weights to
#'   remove quarterly artifacts. Smoothing is adapted per time period:
#'   monthly (3-period window), fortnight (7-period window), weekly (no smoothing).
#' @param keep_all Logical. If TRUE (default), keep all observations including
#'   those with undetermined reference periods. If FALSE, drop undetermined rows.
#' @param verbose Logical. If TRUE (default), print progress messages.
#'
#' @return A data.table with the input data plus:
#'   \describe{
#'     \item{ref_month_start, ref_month_end}{IBGE month boundaries (Sunday/Saturday)}
#'     \item{ref_fortnight_start, ref_fortnight_end}{IBGE fortnight boundaries (Sunday/Saturday)}
#'     \item{ref_week_start, ref_week_end}{IBGE week boundaries (Sunday/Saturday)}
#'     \item{ref_month_in_quarter, ref_fortnight_in_quarter, ref_week_in_quarter}{Position within quarter (1-3, 1-6, 1-12)}
#'     \item{ref_month_yyyymm, ref_fortnight_yyyyff, ref_week_yyyyww}{Integer period codes}
#'     \item{ref_month_weeks}{Number of IBGE reference weeks in month (always 4)}
#'     \item{determined_month, determined_fortnight, determined_week}{Logical determination flags}
#'     \item{weight_monthly, weight_fortnight, or weight_weekly}{Calibrated weights (if calibrate=TRUE)}
#'   }
#'
#' @details
#' ## Weight Calibration
#'
#' When \code{calibrate = TRUE}, the function performs hierarchical rake weighting:
#' \enumerate{
#'   \item Groups observations by nested demographic/geographic cells
#'   \item Iteratively adjusts weights so sub-period totals match anchor-period totals
#'   \item Calibrates final weights against external population totals (FULL Brazilian population)
#'   \item Optionally smooths weights to remove quarterly artifacts
#' }
#'
#' ## Population Targets
#'
#' All time periods (months, fortnights, and weeks) are calibrated to the FULL
#' Brazilian population from SIDRA. This means:
#' \itemize{
#'   \item Monthly weights sum to the Brazilian population for that month
#'   \item Fortnight weights sum to the Brazilian population for the containing month
#'   \item Weekly weights sum to the Brazilian population for the containing month
#' }
#'
#' ## Hierarchical Raking Levels
#'
#' The number of hierarchical cell levels is automatically adjusted based on the
#' calibration unit to avoid sparse cell issues:
#' \itemize{
#'   \item \code{"month"}: 4 levels (age, region, state, post-stratum) - full hierarchy
#'   \item \code{"fortnight"}: 2 levels (age, region) - simplified for lower sample size
#'   \item \code{"week"}: 1 level (age groups only) - minimal hierarchy for sparse data
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
  if (!is.character(weight_var) || length(weight_var) != 1L) {
    stop("'weight_var' must be a single character string")
  }
  if (!anchor %in% c("quarter", "year")) {
    stop("'anchor' must be either \"quarter\" or \"year\"")
  }
  if (!is.logical(calibrate) || length(calibrate) != 1L) {
    stop("'calibrate' must be TRUE or FALSE")
  }
  calibration_unit <- match.arg(calibration_unit)
  if (!is.logical(smooth) || length(smooth) != 1L) {
    stop("'smooth' must be TRUE or FALSE")
  }
  if (!is.logical(keep_all) || length(keep_all) != 1L) {
    stop("'keep_all' must be TRUE or FALSE")
  }
  if (!is.logical(verbose) || length(verbose) != 1L) {
    stop("'verbose' must be TRUE or FALSE")
  }

  # Cache column names for repeated checks
  dt_names <- names(data)
  xw_names <- names(crosswalk)

  join_keys <- c("Ano", "Trimestre", "UPA", "V1008", "V1014")

  # Check join keys exist
  # Crosswalk must have ALL join keys (it's generated by pnadc_identify_periods which always includes them)
  missing_in_xw <- setdiff(join_keys, xw_names)
  if (length(missing_in_xw) > 0) {
    stop(sprintf("Crosswalk missing required columns: %s. ",
                 paste(missing_in_xw, collapse = ", ")),
         "The crosswalk should be created by pnadc_identify_periods() which always includes all keys.")
  }

  # Data must also have all join keys for proper merging
  missing_in_data <- setdiff(join_keys, dt_names)
  if (length(missing_in_data) > 0) {
    stop(sprintf("Data missing required join key columns: %s. ",
                 paste(missing_in_data, collapse = ", ")),
         "Required: Ano, Trimestre, UPA, V1008, V1014")
  }

  # Check weight variable exists
  if (!weight_var %in% dt_names) {
    stop(sprintf("Weight variable '%s' not found in data", weight_var))
  }

  # Convert to data.table
  dt <- PNADCperiods:::ensure_data_table(data, copy = FALSE)
  xw <- PNADCperiods:::ensure_data_table(crosswalk, copy = FALSE)  # No copy needed - only read

  # Freeing memory
  gc()

  # Ensure consistent types for join keys
  for (col in join_keys) {
    if (col %in% dt_names && col %in% xw_names) {

      # Coerce both to integer for consistent joins
      if (!is.integer(dt[[col]])) {
        data.table::set(dt, j = col, value = as.integer(dt[[col]]))
      }

      # Coerce crosswalk if needed
      if (!is.integer(xw[[col]])) {
        data.table::set(xw, j = col, value = as.integer(xw[[col]]))
      }
    }
  }

  # Freeing memory
  gc()



  # ============================================================================
  # STEP 1: Merge crosswalk with data
  # ============================================================================

  if (verbose) cat("Applying reference period crosswalk...\n")

  # Select crosswalk columns to merge
  xw_cols <- intersect(
    c("Ano",
      "Trimestre",
      "UPA",
      "V1008",
      "V1014",
      "ref_month_in_quarter",
      "ref_month_in_year",
      "ref_fortnight_in_month",
      "ref_fortnight_in_quarter",
      "ref_week_in_month",
      "ref_week_in_quarter",
      "ref_month_yyyymm",
      "ref_fortnight_yyyyff",
      "ref_week_yyyyww",
      "determined_month",
      "determined_fortnight",
      "determined_week"),
    xw_names
  )

  # Removing unecessary repetitions
  xw_small = unique(xw[, ..xw_cols])

  # Freeing memory
  rm(xw);gc()

  # Set keys before merge
  data.table::setkeyv(dt, join_keys)
  data.table::setkeyv(xw_small, join_keys)

  # Merge (keyed merge is faster)
  dt <- merge(dt, xw_small, by = join_keys, all.x = TRUE)

  n_matched <- sum(dt$determined_month == T)
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

    # Check for calibration columns and warn if missing
    dt_names <- names(dt)  # Update after merge
    calib_cols <- c("V2009", "UF", "posest", "posest_sxi")
    missing_calib <- setdiff(calib_cols, dt_names)
    if (length(missing_calib) > 0 && verbose) {
      warning("Missing calibration columns: ", paste(missing_calib, collapse = ", "), ". ",
              "Using simplified cell hierarchy for weight calibration. ",
              "For best results, ensure data includes V2009 (age), UF, posest, and posest_sxi.",
              call. = FALSE)
    }

    # Determine ref_var based on calibration_unit
    ref_var <- switch(calibration_unit,
                      month     = "ref_month_yyyymm",
                      fortnight = "ref_fortnight_yyyyff",
                      week      = "ref_week_yyyyww")

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
                              fortnight = PNADCperiods:::derive_fortnight_population(monthly_pop),
                              week      = PNADCperiods:::derive_weekly_population(monthly_pop))
    }

    # Run unified calibration
    dt <- PNADCperiods:::calibrate_weights_internal(
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

      keep_rows <- dt[[determined_var]] == TRUE
      dt <- dt[keep_rows]
    }
  }

  # Freeing memory
  gc()

  if (verbose) cat("Done.\n")

  dt
}


#' Internal Unified Calibration Function
#'
#' Performs hierarchical rake weighting for any anchor/unit combination.
#' The number of hierarchical cell levels is automatically adjusted based on
#' the time period granularity to avoid sparse cell issues.
#'
#' @param dt data.table with PNADC data and reference period columns
#' @param weight_var Name of input weight column
#' @param ref_var Name of reference period column (ref_month_yyyymm, etc.)
#' @param anchor "quarter" or "year"
#' @param target_totals Population targets data.table
#' @param n_cells Integer (1-4) or NULL. Number of hierarchical cell levels.
#'   If NULL (default), auto-selects based on ref_var:
#'   - month: 4 levels (full hierarchy)
#'   - fortnight: 2 levels (age + region)
#'   - week: 1 level (age only)
#' @param min_cell_size Minimum observations per cell. Levels with smaller
#'   cells are skipped. Default 10.
#' @param smooth Apply smoothing?
#' @param keep_all Keep undetermined observations?
#' @param verbose Print progress?
#' @return data.table with weight_calibrated column
#' @keywords internal
#' @noRd

#dt
#weight_var = weight_var
#ref_var = ref_var
#anchor = anchor
#target_totals = target_totals
#n_cells = NULL
#min_cell_size = 10L
#smooth = smooth
#keep_all = keep_all
#verbose = verbose

calibrate_weights_internal <- function(dt,
                                       weight_var,
                                       ref_var,
                                       anchor,
                                       target_totals,
                                       n_cells = NULL,
                                       min_cell_size = 10L,
                                       smooth = TRUE,
                                       keep_all = TRUE,
                                       verbose = FALSE) {

  # Pre-extract ref column for direct access
  ref_col <- dt[[ref_var]]

  # Determine anchor grouping variable
  anchor_vars <- if (anchor == "quarter") {
    c("Ano", "Trimestre")
  } else {
    # For annual anchor, extract year using integer arithmetic
    # ref_var is YYYYMM/YYYYFF/YYYYWW format, so %/% 100 gives YYYY
    data.table::set(dt, j = "anchor_year", value = ref_col %/% 100L)
    "anchor_year"
  }

  # Create integer anchor key for efficient uniqueN

  if (length(anchor_vars) == 2L) {
    # Quarterly: create composite key from Ano and Trimestre
    data.table::set(dt, j = ".anchor_key", value = dt[["Ano"]] * 10L + dt[["Trimestre"]])
  } else {
    # Annual: use anchor_year directly
    data.table::set(dt, j = ".anchor_key", value = dt[["anchor_year"]])
  }

  # Mark determined observations
  is_determined <- !is.na(ref_col)
  data.table::set(dt, j = ".is_determined", value = is_determined)

  n_determined <- sum(is_determined)
  if (n_determined == 0L) {
    if (keep_all) {
      dt[, weight_calibrated := NA_real_]
      dt[, c(".is_determined", ".anchor_key") := NULL]
      if ("anchor_year" %in% names(dt)) dt[, anchor_year := NULL]
      return(dt)
    }
    warning("No observations with determined reference period")
    dt[, c(".is_determined", ".anchor_key") := NULL]
    if ("anchor_year" %in% names(dt)) dt[, anchor_year := NULL]
    return(dt[0])  # Return empty dt
  }

  # Auto-select number of cell levels based on time period granularity
  if (is.null(n_cells)) {
    n_cells <- if (grepl("week", ref_var, ignore.case = TRUE)) {
      1L  # Weekly: use age groups only (celula1)
    } else if (grepl("fortnight", ref_var, ignore.case = TRUE)) {
      2L  # Fortnight: use age + region (celula1, celula2)
    } else {
      4L  # Monthly: use full hierarchy (celula1-4)
    }
    if (verbose) {
      cat(sprintf("    Auto-selected %d cell level(s) for %s calibration\n",
                  n_cells, ref_var))
    }
  } else {
    if (!n_cells %in% 1L:4L) {
      stop("'n_cells' must be an integer between 1 and 4")
    }
  }

  # Initialize working weight using direct column copy
  data.table::set(dt, j = "weight_current", value = dt[[weight_var]])

  # Step 1: Create calibration cells (only create needed levels)
  dt <- PNADCperiods:::create_calibration_cells_unified(dt, n_cells = n_cells)

  # Pre-extract weight column for reweighting loop
  weight_vec <- dt[[weight_var]]

  # Step 2: Iterative hierarchical reweighting with cell size checking
  levels_applied <- 0L
  for (level in seq_len(n_cells)) {
    cell_var <- paste0("celula", level)

    # Check minimum cell size before applying this level
    # Only check determined observations
    cell_sizes <- dt[.is_determined == TRUE, .N, by = c(cell_var, ref_var)]
    min_size <- min(cell_sizes$N)

    if (min_size < min_cell_size) {
      if (verbose) {
        cat(sprintf("    Skipping %s: min cell size %d < threshold %d\n",
                    cell_var, min_size, min_cell_size))
      }
      break
    }

    # Inline reweighting with pre-extracted columns
    dt <- PNADCperiods:::reweight_at_cell_level(dt          = dt,
                                                          cell_var    = cell_var,
                                                          anchor_vars = anchor_vars,
                                                          ref_var     = ref_var,
                                                          weight_vec  = weight_vec)
    levels_applied <- levels_applied + 1L
  }

  if (verbose && levels_applied > 0) {
    cat(sprintf("    Applied %d hierarchical cell level(s)\n", levels_applied))
  }

  # Step 3: Final calibration to external totals (only for determined obs)
  dt <- PNADCperiods:::calibrate_to_external_totals(dt, target_totals, ref_var)

  # Step 4: Smooth weights (if requested and appropriate for the time period)
  if (smooth) {
    dt <- PNADCperiods:::smooth_calibrated_weights(dt, ref_var)
  }

  # Rename final weight
  data.table::setnames(dt, "weight_current", "weight_calibrated")

  # Set NA for undetermined observations
  dt[.is_determined == FALSE, weight_calibrated := NA_real_]

  # Clean up temporary columns
  cell_cols <- paste0("celula", seq_len(n_cells))
  temp_cols <- c("anchor_year", ".anchor_key", ".is_determined",
                 cell_cols,
                 "pop_current", "target_pop")
  existing_temp <- temp_cols[temp_cols %in% names(dt)]
  if (length(existing_temp) > 0) {
    dt[, (existing_temp) := NULL]
  }

  dt
}


#' Create Calibration Cells
#'
#' @param dt data.table with PNADC data
#' @param n_cells Integer (1-4). Number of cell levels to create. Default 4.
#' @keywords internal
#' @noRd
create_calibration_cells_unified <- function(dt, n_cells = 4L) {

  # Cache column names
  dt_names <- names(dt)

  # Convert age column to integer if needed
  age_col <- if ("V2009" %in% dt_names) "V2009" else if ("v2009" %in% dt_names) "v2009" else NULL

  if (!is.null(age_col) && !is.integer(dt[[age_col]])) {
    data.table::set(dt, j = age_col, value = as.integer(dt[[age_col]]))
  }

  # Only convert other columns if we'll use them
  if (n_cells >= 2L && "posest_sxi" %in% dt_names && !is.integer(dt[["posest_sxi"]])) {
    data.table::set(dt, j = "posest_sxi", value = as.integer(dt[["posest_sxi"]]))
  }
  if (n_cells >= 4L && "posest" %in% dt_names && !is.integer(dt[["posest"]])) {
    data.table::set(dt, j = "posest", value = as.integer(dt[["posest"]]))
  }
  if (n_cells >= 3L) {
    uf_col <- if ("UF" %in% dt_names) "UF" else if ("uf" %in% dt_names) "uf" else NULL
    if (!is.null(uf_col) && !is.integer(dt[[uf_col]])) {
      data.table::set(dt, j = uf_col, value = as.integer(dt[[uf_col]]))
    }
  }

  # Handle case where age column is missing
  if (is.null(age_col)) {
    warning("Age column (V2009/v2009) not found. Using simple calibration.")
    for (i in seq_len(n_cells)) {
      data.table::set(dt, j = paste0("celula", i), value = 1L)
    }
    return(dt)
  }

  # Pre-extract age vector for fcase
  age_vec <- dt[[age_col]]

  # Celula 1: Age groups (always needed)
  data.table::set(dt, j = "celula1", value = data.table::fcase(
    age_vec <= 13L, 0L,
    age_vec <= 29L, 1L,
    age_vec <= 59L, 2L,
    default = 3L
  ))

  # Only create higher-level cells if needed
  if (n_cells >= 2L) {
    # Celula 2: Post-stratum group + age
    if ("posest_sxi" %in% dt_names) {
      # Use direct column access
      celula1_vec <- dt[["celula1"]]
      posest_sxi_vec <- dt[["posest_sxi"]]
      data.table::set(dt, j = "celula2", value = posest_sxi_vec %/% 100L + 10L * celula1_vec)
    } else {
      data.table::set(dt, j = "celula2", value = dt[["celula1"]])
    }
  }

  if (n_cells >= 3L) {
    # Celula 3: State + celula2
    uf_col <- if ("UF" %in% dt_names) "UF" else if ("uf" %in% dt_names) "uf" else NULL
    if (!is.null(uf_col)) {
      uf_vec <- dt[[uf_col]]
      celula2_vec <- dt[["celula2"]]
      data.table::set(dt, j = "celula3", value = uf_vec + 100L * celula2_vec)
    } else {
      data.table::set(dt, j = "celula3", value = dt[["celula2"]])
    }
  }

  if (n_cells >= 4L) {
    # Celula 4: Post-stratum + celula2
    if ("posest" %in% dt_names) {
      posest_vec <- dt[["posest"]]
      celula2_vec <- dt[["celula2"]]
      data.table::set(dt, j = "celula4", value = posest_vec + 1000L * celula2_vec)
    } else {
      data.table::set(dt, j = "celula4", value = dt[["celula3"]])
    }
  }

  dt
}


#' Reweight at Cell Level
#'
#' @keywords internal
#' @noRd
reweight_at_cell_level <- function(dt, cell_var, anchor_vars, ref_var, weight_vec) {

  # Only process determined observations
  # Use direct column access throughout

  # Anchor-level aggregations: sum of original weights per cell-anchor
  # Use pre-extracted weight_vec instead of get(weight_var)
  dt[.is_determined == TRUE, pop_anchor := sum(weight_vec[.I], na.rm = TRUE),
     by = c(cell_var, anchor_vars)]

  # Count unique ref periods per cell-anchor
  dt[.is_determined == TRUE, n_cells_anchor := data.table::uniqueN(.SD[[1]]),
     by = c(cell_var, anchor_vars), .SDcols = ref_var]

  # Period-level aggregations: sum of current weights
  dt[.is_determined == TRUE, pop_period := sum(weight_current, na.rm = TRUE),
     by = c(cell_var, ref_var)]

  # Count unique anchors using pre-computed .anchor_key
  # This replaces uniqueN(do.call(paste, ...)) which was extremely slow
  dt[.is_determined == TRUE, n_cells_period := data.table::uniqueN(.anchor_key),
     by = c(cell_var, ref_var)]

  # Apply reweighting ratio (only for determined observations)
  dt[.is_determined == TRUE & n_cells_anchor <= n_cells_period & pop_period > 0,
     weight_current := weight_current * (pop_anchor / pop_period)]

  # Clean up temporary columns
  dt[, c("pop_anchor", "pop_period", "n_cells_anchor", "n_cells_period") := NULL]

  dt
}


#' Calibrate to External Population Totals
#'
#' @keywords internal
#' @noRd
calibrate_to_external_totals <- function(dt, target_totals, ref_var) {

  # Don't copy target_totals, work with it directly
  tt <- ensure_data_table(target_totals, copy = FALSE)
  tt_names <- names(tt)

  # Find the matching column in targets
  pop_col <- intersect(c("m_populacao", "f_populacao", "w_populacao", "population", "pop"),
                       tt_names)[1]
  if (is.na(pop_col)) {
    stop("Target totals must have a population column (m_populacao, f_populacao, w_populacao, population, or pop)")
  }

  # Find the ref column in targets
  ref_col_map <- list(
    "ref_month_yyyymm"     = c("ref_month_yyyymm", "anomesexato", "yyyymm"),
    "ref_fortnight_yyyyff" = c("ref_fortnight_yyyyff", "yyyyff"),
    "ref_week_yyyyww"      = c("ref_week_yyyyww", "ref_week_iso_yyyyww", "yyyyww")
  )

  if (!ref_var %in% names(ref_col_map)) {
    stop(sprintf("Unknown reference variable: %s. Expected one of: %s",
                 ref_var, paste(names(ref_col_map), collapse = ", ")))
  }

  tt_ref_col <- NULL
  for (candidate in ref_col_map[[ref_var]]) {
    if (candidate %in% tt_names) {
      tt_ref_col <- candidate
      break
    }
  }
  if (is.null(tt_ref_col)) {
    stop(sprintf("Target totals must have a column matching %s. Expected one of: %s",
                 ref_var, paste(ref_col_map[[ref_var]], collapse = ", ")))
  }

  # Create minimal lookup table (only copy if we need to rename)
  if (tt_ref_col != ref_var || pop_col != "target_pop") {
    tt <- data.table::copy(tt[, c(tt_ref_col, pop_col), with = FALSE])
    if (tt_ref_col != ref_var) {
      data.table::setnames(tt, tt_ref_col, ref_var)
    }
    data.table::setnames(tt, pop_col, "target_pop")
  } else {
    tt <- tt[, c(ref_var, pop_col), with = FALSE]
    data.table::setnames(tt, pop_col, "target_pop")
  }

  # Calculate current totals (only for determined observations)
  dt[.is_determined == TRUE, pop_current := sum(weight_current, na.rm = TRUE), by = ref_var]

  # Join targets
  dt[tt, on = ref_var, target_pop := i.target_pop]

  # Apply calibration (population in thousands) - only for determined
  dt[.is_determined == TRUE & !is.na(target_pop) & pop_current > 0,
     weight_current := weight_current * (target_pop * 1000 / pop_current)]

  # Clean up
  dt[, c("pop_current", "target_pop") := NULL]

  dt
}


#' Smooth Calibrated Weights
#'
#' @param dt data.table with calibrated weights
#' @param ref_var Name of reference period variable (e.g., "ref_month_yyyymm")
#' @return data.table with smoothed weights in weight_current column
#' @keywords internal
#' @noRd
smooth_calibrated_weights <- function(dt, ref_var) {

  # Skip smoothing for weekly data
  if (grepl("week", ref_var, ignore.case = TRUE)) {
    return(dt)
  }

  # Determine smoothing parameters
  if (grepl("fortnight", ref_var, ignore.case = TRUE)) {
    window_size <- 7L
    cell_var <- "celula2"
  } else {
    window_size <- 3L
    cell_var <- "celula4"
  }

  # Check if the required cell variable exists
  dt_names <- names(dt)
  if (!cell_var %in% dt_names) {
    cell_var <- if ("celula2" %in% dt_names) "celula2" else
      if ("celula1" %in% dt_names) "celula1" else NULL
    if (is.null(cell_var)) {
      return(dt)
    }
  }

  # Get unique periods from determined observations only
  ref_vec <- dt[[ref_var]]
  determined_mask <- dt[[".is_determined"]]
  periods <- sort(unique(ref_vec[determined_mask]))
  n_periods <- length(periods)

  if (n_periods < 2L * window_size + 1L) {
    return(dt)
  }

  # Single aggregation pass on determined data only
  # Aggregate by cell and ref_var to get cell-period populations
  cell_period_agg <- dt[.is_determined == TRUE,
                        .(cell_pop = sum(weight_current, na.rm = TRUE)),
                        keyby = c(cell_var, ref_var)]

  # Derive original period totals from the smaller aggregated table
  original_pop <- cell_period_agg[, .(pop_orig = sum(cell_pop)), keyby = ref_var]

  # Add period_pos using match()
  cell_period_agg[, period_pos := match(cell_period_agg[[ref_var]], periods)]

  # Rekey for frollmean
  data.table::setkeyv(cell_period_agg, c(cell_var, "period_pos"))

  # Compute centered rolling mean per cell
  cell_period_agg[, pop_smoothed := data.table::frollmean(
    cell_pop,
    n = window_size,
    align = "center",
    na.rm = TRUE
  ), by = c(cell_var)]

  # Handle boundary NAs
  cell_period_agg[is.na(pop_smoothed), pop_smoothed := cell_pop]

  # Calculate smoothing factor
  cell_period_agg[, smooth_factor := data.table::fifelse(
    cell_pop > 0 & pop_smoothed > 0,
    pop_smoothed / cell_pop,
    1.0
  )]

  # Join smooth_factor using (cell_var, ref_var)
  # Create minimal join table
  smooth_join <- cell_period_agg[, c(cell_var, ref_var, "smooth_factor"), with = FALSE]
  data.table::setkeyv(smooth_join, c(cell_var, ref_var))

  # Join and apply smoothing factor (only for determined)
  dt[smooth_join, on = c(cell_var, ref_var), smooth_factor := i.smooth_factor]
  dt[.is_determined == TRUE & !is.na(smooth_factor) & !is.na(weight_current),
     weight_current := weight_current * smooth_factor]

  # Recalibrate to preserve original period totals
  dt[.is_determined == TRUE, pop_new := sum(weight_current, na.rm = TRUE), by = ref_var]

  # Join original totals
  dt[original_pop, on = ref_var, pop_orig := i.pop_orig]

  # Apply final adjustment
  dt[.is_determined == TRUE & pop_new > 0 & !is.na(pop_orig) & !is.na(weight_current),
     weight_current := weight_current * (pop_orig / pop_new)]

  # Clean up temporary columns
  temp_cols <- c("smooth_factor", "pop_new", "pop_orig")
  existing_temp <- temp_cols[temp_cols %in% names(dt)]
  if (length(existing_temp) > 0) {
    dt[, (existing_temp) := NULL]
  }

  dt
}


#' Derive Fortnight Population from Monthly
#'
#' Each fortnight within a month receives the FULL month's population as its
#' calibration target. This ensures that weights for each fortnight sum to the
#' actual Brazilian population, consistent with the monthly calibration approach.
#'
#' @keywords internal
#' @noRd
derive_fortnight_population <- function(monthly_pop) {

  # Don't copy if not needed
  mt <- ensure_data_table(monthly_pop, copy = FALSE)

  # Standardize column name
  if ("anomesexato" %in% names(mt) && !"ref_month_yyyymm" %in% names(mt)) {
    mt <- data.table::copy(mt)  # Copy only if we modify
    mt[, ref_month_yyyymm := anomesexato]
  }

  # Each fortnight gets the FULL month's population (not divided)
  fortnights <- mt[, .(
    ref_fortnight_yyyyff = c(
      ref_month_yyyymm %/% 100 * 100 + ((ref_month_yyyymm %% 100) - 1) * 2 + 1,
      ref_month_yyyymm %/% 100 * 100 + ((ref_month_yyyymm %% 100) - 1) * 2 + 2
    ),
    f_populacao = m_populacao
  ), by = ref_month_yyyymm]

  fortnights[, ref_month_yyyymm := NULL]

  fortnights
}


#' Derive Weekly Population from Monthly (IBGE Calendar)
#'
#' Each IBGE week receives the FULL month's population as its calibration target.
#' In the IBGE calendar, each week belongs to exactly one IBGE month (the month
#' where the week's Saturday falls with >= 4 days), so there is no ambiguity.
#' This ensures that weights for each week sum to the actual Brazilian population.
#'
#' Uses integer arithmetic for performance - avoids creating Date objects.
#'
#' @keywords internal
#' @noRd
derive_weekly_population <- function(monthly_pop) {

  # Don't copy if not needed
  mt <- ensure_data_table(monthly_pop, copy = FALSE)

  # Standardize column name
  if ("anomesexato" %in% names(mt) && !"ref_month_yyyymm" %in% names(mt)) {
    mt <- data.table::copy(mt)  # Copy only if we modify
    mt[, ref_month_yyyymm := anomesexato]
  }

  # Vectorized approach instead of lapply + rbindlist
  # Each IBGE month has exactly 4 reference weeks
  n_months <- nrow(mt)

  # Pre-allocate result
  yyyymm_vec <- mt$ref_month_yyyymm
  pop_vec <- mt$m_populacao
  year_vec <- yyyymm_vec %/% 100L
  month_vec <- yyyymm_vec %% 100L

  # Week positions within the year (1-48 for each month's 4 weeks)
  week_start <- (month_vec - 1L) * 4L + 1L

  # Expand to all weeks (4 per month)
  result <- data.table::data.table(
    ref_week_yyyyww = c(
      year_vec * 100L + week_start,
      year_vec * 100L + week_start + 1L,
      year_vec * 100L + week_start + 2L,
      year_vec * 100L + week_start + 3L
    ),
    w_populacao = rep(pop_vec, 4L)
  )

  # Sort by week
  data.table::setorder(result, ref_week_yyyyww)

  result
}
