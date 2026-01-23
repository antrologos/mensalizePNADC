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
#'     \item{ref_month_weeks}{Number of IBGE reference weeks in month (4 or 5)}
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

  checkmate::assert_string(weight_var)
  checkmate::assert_choice(anchor, c("quarter", "year"))
  checkmate::assert_logical(calibrate, len = 1)
  calibration_unit <- match.arg(calibration_unit)
  checkmate::assert_logical(smooth, len = 1)
  checkmate::assert_logical(keep_all, len = 1)
  checkmate::assert_logical(verbose, len = 1)

  # Convert to data.table
  dt <- ensure_data_table(data, copy = TRUE)
  xw <- ensure_data_table(crosswalk, copy = TRUE)  # Copy to allow type coercion

  # Ensure consistent types for join keys
  # PNADC data may have character or integer columns depending on source
  # OPTIMIZATION: Use data.table::set() for more efficient by-reference updates
  type_coerce_cols <- c("Ano", "Trimestre", "UPA", "V1008", "V1014")
  for (col in type_coerce_cols) {
    if (col %in% names(dt) && col %in% names(xw)) {
      # Coerce both to integer for consistent joins
      if (!is.integer(dt[[col]])) {
        data.table::set(dt, j = col, value = as.integer(dt[[col]]))
      }
      if (!is.integer(xw[[col]])) {
        data.table::set(xw, j = col, value = as.integer(xw[[col]]))
      }
    }
  }

  # Check join keys exist
  # Crosswalk is at household-quarter level: all 5 keys are required for correct merging
  # Using fewer keys would create incorrect many-to-many relationships
  join_keys <- c("Ano", "Trimestre", "UPA", "V1008", "V1014")

  # Crosswalk must have ALL join keys (it's generated by pnadc_identify_periods which always includes them)
  missing_in_xw <- setdiff(join_keys, names(xw))
  if (length(missing_in_xw) > 0) {
    stop(sprintf("Crosswalk missing required columns: %s. ",
                 paste(missing_in_xw, collapse = ", ")),
         "The crosswalk should be created by pnadc_identify_periods() which always includes all keys.")
  }

  # Data must also have all join keys for proper merging
  missing_in_data <- setdiff(join_keys, names(dt))
  if (length(missing_in_data) > 0) {
    stop(sprintf("Data missing required join key columns: %s. ",
                 paste(missing_in_data, collapse = ", ")),
         "Required: Ano, Trimestre, UPA, V1008, V1014")
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
    c("Ano", "Trimestre", "UPA", "V1008", "V1014",
      "ref_month_start", "ref_month_end", "ref_month_in_quarter", "ref_month_yyyymm", "ref_month_weeks", "determined_month",
      "ref_fortnight_start", "ref_fortnight_end", "ref_fortnight_in_quarter", "ref_fortnight_yyyyff", "ref_fortnight_weeks", "determined_fortnight",
      "ref_week_start", "ref_week_end", "ref_week_in_quarter", "ref_week_yyyyww", "determined_week"),
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

    # Check for calibration columns and warn if missing
    # Calibration uses person-level cells based on age (V2009), geography (UF),
    # and post-strata (posest, posest_sxi)
    calib_cols <- c("V2009", "UF", "posest", "posest_sxi")
    missing_calib <- setdiff(calib_cols, names(dt))
    if (length(missing_calib) > 0 && verbose) {
      warning("Missing calibration columns: ", paste(missing_calib, collapse = ", "), ". ",
              "Using simplified cell hierarchy for weight calibration. ",
              "For best results, ensure data includes V2009 (age), UF, posest, and posest_sxi.",
              call. = FALSE)
    }

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

  # Auto-select number of cell levels based on time period granularity
  # Weeks and fortnights have much fewer observations, so use fewer cell levels
  # to avoid sparse cells that cause unstable weight adjustments
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
    # Validate user-provided n_cells
    if (!n_cells %in% 1L:4L) {
      stop("'n_cells' must be an integer between 1 and 4")
    }
  }

  # Initialize working weight
  dt[, weight_current := get(weight_var)]

  # Step 1: Create calibration cells (OPTIMIZATION: only create needed levels)
  dt <- create_calibration_cells_unified(dt, n_cells = n_cells)

  # Step 2: Iterative hierarchical reweighting with cell size checking
  levels_applied <- 0L
  for (level in seq_len(n_cells)) {
    cell_var <- paste0("celula", level)

    # Check minimum cell size before applying this level
    cell_sizes <- dt[, .N, by = c(cell_var, ref_var)]
    min_size <- min(cell_sizes$N)

    if (min_size < min_cell_size) {
      if (verbose) {
        cat(sprintf("    Skipping %s: min cell size %d < threshold %d\n",
                    cell_var, min_size, min_cell_size))
      }
      break  # Stop at this level, don't apply finer cells
    }

    dt <- reweight_at_cell_level_unified(dt, cell_var, anchor_vars, ref_var, weight_var)
    levels_applied <- levels_applied + 1L
  }

  if (verbose && levels_applied > 0) {
    cat(sprintf("    Applied %d hierarchical cell level(s)\n", levels_applied))
  }

  # Step 3: Final calibration to external totals
  dt <- calibrate_to_external_totals(dt, target_totals, ref_var)

  # Step 4: Smooth weights (if requested and appropriate for the time period)
  if (smooth) {
    dt <- smooth_calibrated_weights(dt, ref_var)
  }

  # Rename final weight
  data.table::setnames(dt, "weight_current", "weight_calibrated")

  # Clean up temporary columns (OPTIMIZATION: only remove cells that were created)
  cell_cols <- paste0("celula", seq_len(n_cells))
  temp_cols <- c("anchor_year", "pop_anchor", "pop_period",
                 "n_cells_anchor", "n_cells_period",
                 cell_cols,
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
#' OPTIMIZATION: Now accepts n_cells parameter to only create needed columns.
#' For weekly calibration (n_cells=1), only celula1 is created.
#' For fortnight calibration (n_cells=2), only celula1-2 are created.
#' This reduces memory allocation for finer time periods.
#'
#' @param dt data.table with PNADC data
#' @param n_cells Integer (1-4). Number of cell levels to create. Default 4.
#' @keywords internal
#' @noRd
create_calibration_cells_unified <- function(dt, n_cells = 4L) {
  # OPTIMIZATION: Use data.table::set() for more efficient by-reference updates
  # Convert V2009/v2009 to integer (saves 4 bytes per row vs numeric, and age is always integer)
  for (col in c("V2009", "v2009")) {
    if (col %in% names(dt)) {
      if (is.character(dt[[col]])) {
        data.table::set(dt, j = col, value = as.integer(dt[[col]]))
      } else if (!is.integer(dt[[col]])) {
        data.table::set(dt, j = col, value = as.integer(dt[[col]]))
      }
    }
  }
  # Only convert posest columns if we'll use them (n_cells >= 2 for posest_sxi, >= 4 for posest)
  if (n_cells >= 2L && "posest_sxi" %in% names(dt) && !is.integer(dt[["posest_sxi"]])) {
    data.table::set(dt, j = "posest_sxi", value = as.integer(dt[["posest_sxi"]]))
  }
  if (n_cells >= 4L && "posest" %in% names(dt) && !is.integer(dt[["posest"]])) {
    data.table::set(dt, j = "posest", value = as.integer(dt[["posest"]]))
  }
  # Only convert UF if we'll use it (n_cells >= 3)
  if (n_cells >= 3L) {
    for (col in c("UF", "uf")) {
      if (col %in% names(dt) && !is.integer(dt[[col]])) {
        data.table::set(dt, j = col, value = as.integer(dt[[col]]))
      }
    }
  }

  # Use standardized column names (handle both cases)
  age_col <- if ("V2009" %in% names(dt)) "V2009" else if ("v2009" %in% names(dt)) "v2009" else NULL
  uf_col <- if ("UF" %in% names(dt)) "UF" else if ("uf" %in% names(dt)) "uf" else NULL

  if (is.null(age_col)) {
    warning("Age column (V2009/v2009) not found. Using simple calibration.")
    # Only create the needed cell columns
    for (i in seq_len(n_cells)) {
      dt[, paste0("celula", i) := 1L]
    }
    return(dt)
  }

  # Celula 1: Age groups (always needed)
  dt[, celula1 := data.table::fcase(
    get(age_col) <= 13, 0L,
    get(age_col) <= 29, 1L,
    get(age_col) <= 59, 2L,
    default = 3L
  )]

  # OPTIMIZATION: Only create higher-level cells if needed
  if (n_cells >= 2L) {
    # Celula 2: Post-stratum group + age
    if ("posest_sxi" %in% names(dt)) {
      dt[, celula2 := as.integer(posest_sxi %/% 100L) + 10L * celula1]
    } else {
      dt[, celula2 := celula1]
    }
  }

  if (n_cells >= 3L) {
    # Celula 3: State + celula2
    if (!is.null(uf_col)) {
      dt[, celula3 := get(uf_col) + 100L * celula2]
    } else {
      dt[, celula3 := celula2]
    }
  }

  if (n_cells >= 4L) {
    # Celula 4: Post-stratum + celula2
    if ("posest" %in% names(dt)) {
      dt[, celula4 := posest + 1000L * celula2]
    } else {
      dt[, celula4 := celula3]
    }
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
  # Count how many unique anchor periods (quarters or years) contain this cell-period combination
  dt[, `:=`(
    pop_period = sum(weight_current, na.rm = TRUE),
    n_cells_period = data.table::uniqueN(do.call(paste, c(.SD, sep = "_")))
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

  # Find the ref column in targets - use list for multiple possible column names
  ref_col_map <- list(
    "ref_month_yyyymm" = c("ref_month_yyyymm", "anomesexato", "yyyymm"),
    "ref_fortnight_yyyyff" = c("ref_fortnight_yyyyff", "yyyyff"),
    "ref_week_yyyyww" = c("ref_week_yyyyww", "ref_week_iso_yyyyww", "yyyyww")
  )

  # Check if ref_var is in the map
  if (!ref_var %in% names(ref_col_map)) {
    stop(sprintf("Unknown reference variable: %s. Expected one of: %s",
                 ref_var, paste(names(ref_col_map), collapse = ", ")))
  }

  tt_ref_col <- NULL
  for (candidate in ref_col_map[[ref_var]]) {
    if (candidate %in% names(tt)) {
      tt_ref_col <- candidate
      break
    }
  }
  if (is.null(tt_ref_col)) {
    stop(sprintf("Target totals must have a column matching %s. Expected one of: %s",
                 ref_var, paste(ref_col_map[[ref_var]], collapse = ", ")))
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
#' Applies a moving average adjustment to calibrated weights to remove
#' quarterly artifacts that arise from the survey's quarterly design.
#'
#' The smoothing algorithm adapts to the time period granularity:
#' - Monthly: 3-period moving average using celula4 (finest hierarchy)
#' - Fortnight: 6-period moving average using celula2 (coarser hierarchy)
#' - Weekly: No smoothing (too few observations per cell for stable smoothing)
#'
#' The algorithm:
#' 1. Groups observations by calibration cell and reference period
#' 2. Computes period-specific adjustment factors based on neighboring periods
#' 3. Applies an N-period moving average to smooth the adjustment factors
#' 4. Adjusts weights to preserve total population within each anchor period
#'
#' @param dt data.table with calibrated weights
#' @param ref_var Name of reference period variable (e.g., "ref_month_yyyymm")
#' @return data.table with smoothed weights in weight_current column
#' @keywords internal
#' @noRd
smooth_calibrated_weights <- function(dt, ref_var) {

  # Determine smoothing parameters based on time period granularity
  # Weekly data has too few observations for stable cell-level smoothing
  if (grepl("week", ref_var, ignore.case = TRUE)) {
    # Skip smoothing for weekly data - insufficient observations per cell
    return(dt)
  }

  # For fortnights: use 7-period rolling window (spans ~1 quarter) with celula2
  # For months: use 3-period rolling window (spans 1 quarter) with celula4
  if (grepl("fortnight", ref_var, ignore.case = TRUE)) {
    window_size <- 7L  # 7-period rolling mean for fortnights (~1 quarter)
    cell_var <- "celula2"  # Use coarser cells for fortnights
  } else {
    window_size <- 3L  # 3-period rolling mean for months (1 quarter)
    cell_var <- "celula4"  # Use finest cells for months
  }

  # Check if the required cell variable exists
  if (!cell_var %in% names(dt)) {
    # Fall back to coarser cell level if finer not available
    cell_var <- if ("celula2" %in% names(dt)) "celula2" else
                if ("celula1" %in% names(dt)) "celula1" else NULL
    if (is.null(cell_var)) {
      return(dt)  # No cell variables available, skip smoothing
    }
  }

  # Skip if insufficient periods for smoothing
  n_periods <- data.table::uniqueN(dt[[ref_var]])
  min_periods <- 2 * window_size + 1
  if (n_periods < min_periods) {
    return(dt)
  }

  # Calculate total population by period before smoothing
  original_pop <- dt[, .(pop_orig = sum(weight_current, na.rm = TRUE)), by = ref_var]

  # Calculate adjustment factors by cell and period
  dt[, pop_current := sum(weight_current, na.rm = TRUE), by = c(cell_var, ref_var)]

  # Get unique periods sorted
  periods <- sort(unique(dt[[ref_var]]))
  n_periods <- length(periods)

  # Create lookup for period position
  period_lookup <- data.table::data.table(
    period = periods,
    pos = seq_along(periods)
  )
  data.table::setnames(period_lookup, "period", ref_var)

  # Join position to data
  dt[period_lookup, on = ref_var, period_pos := i.pos]

  # Compute smoothed population by cell-period using proper rolling mean
  cell_period_pop <- dt[, .(cell_pop = sum(weight_current, na.rm = TRUE)),
                         by = c(cell_var, "period_pos")]
  data.table::setkeyv(cell_period_pop, c(cell_var, "period_pos"))

  # For each cell, compute centered rolling mean with appropriate window
  # Using data.table::frollmean for proper rolling average
  cell_period_pop[, pop_smoothed := data.table::frollmean(
    cell_pop,
    n = window_size,
    align = "center",
    na.rm = TRUE
  ), by = c(cell_var)]

  # For boundary periods where rolling mean produces NA, use original values
  cell_period_pop[is.na(pop_smoothed), pop_smoothed := cell_pop]

  # Calculate smoothing factor for each cell-period
  # Avoid division issues: both cell_pop and pop_smoothed must be positive
  cell_period_pop[cell_pop > 0 & pop_smoothed > 0, smooth_factor := pop_smoothed / cell_pop]
  cell_period_pop[cell_pop <= 0 | pop_smoothed <= 0 | is.na(smooth_factor), smooth_factor := 1]

  # Join smoothing factor back to main data
  dt[cell_period_pop, on = c(cell_var, "period_pos"), smooth_factor := i.smooth_factor]

  # Apply smoothing factor
  dt[!is.na(smooth_factor) & !is.na(weight_current),
     weight_current := weight_current * smooth_factor]

  # Recalibrate to preserve original period totals
  new_pop <- dt[, .(pop_new = sum(weight_current, na.rm = TRUE)), by = ref_var]
  pop_adjust <- original_pop[new_pop, on = ref_var]
  pop_adjust[pop_new > 0, final_factor := pop_orig / pop_new]
  pop_adjust[pop_new <= 0 | is.na(final_factor), final_factor := 1]

  dt[pop_adjust, on = ref_var, final_factor := i.final_factor]
  dt[!is.na(final_factor) & !is.na(weight_current),
     weight_current := weight_current * final_factor]

  # Clean up temporary columns
  dt[, c("pop_current", "period_pos", "smooth_factor", "final_factor") := NULL]

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

  mt <- ensure_data_table(monthly_pop, copy = TRUE)

  # Standardize column name
  if ("anomesexato" %in% names(mt) && !"ref_month_yyyymm" %in% names(mt)) {
    mt[, ref_month_yyyymm := anomesexato]
  }

  # Each fortnight gets the FULL month's population (not divided)
  # This ensures fortnight weights sum to the Brazilian population
  fortnights <- mt[, .(
    ref_fortnight_yyyyff = c(
      ref_month_yyyymm %/% 100 * 100 + ((ref_month_yyyymm %% 100) - 1) * 2 + 1,
      ref_month_yyyymm %/% 100 * 100 + ((ref_month_yyyymm %% 100) - 1) * 2 + 2
    ),
    f_populacao = m_populacao  # FULL population, not divided
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

  mt <- ensure_data_table(monthly_pop, copy = TRUE)

  # Standardize column name
  if ("anomesexato" %in% names(mt) && !"ref_month_yyyymm" %in% names(mt)) {
    mt[, ref_month_yyyymm := anomesexato]
  }

  # IBGE months always have exactly 4 reference weeks each
  # Total: 48 reference weeks per year (4 weeks × 12 months)
  # Technical stop weeks are NOT counted as reference weeks

  # Each IBGE month has 4 weeks, so week_in_year = (month - 1) * 4 + week_in_month
  # For month M, weeks are: (M-1)*4 + 1, (M-1)*4 + 2, (M-1)*4 + 3, (M-1)*4 + 4

  weeks_list <- lapply(seq_len(nrow(mt)), function(i) {
    yyyymm <- mt$ref_month_yyyymm[i]
    pop <- mt$m_populacao[i]
    year <- yyyymm %/% 100L
    month <- yyyymm %% 100L

    # Week positions within the year (1-48)
    # Each month has exactly 4 IBGE reference weeks
    week_start <- (month - 1L) * 4L + 1L
    week_positions <- week_start:(week_start + 3L)

    # Create yyyyww format: year * 100 + week_in_year
    data.table(
      ref_week_yyyyww = year * 100L + week_positions,
      w_populacao = pop  # FULL population for each week
    )
  })

  rbindlist(weeks_list)
}
