#' Calibrate Monthly Weights
#'
#' Redistributes quarterly survey weights to monthly weights using hierarchical
#' calibration across demographic and geographic cells.
#'
#' @description
#' The original PNADC survey weights (\code{V1028}) are designed for quarterly
#' estimates. This function creates monthly weights by:
#' \enumerate{
#'   \item Grouping observations by nested demographic/geographic cells
#'   \item Iteratively adjusting weights so monthly totals match quarterly totals
#'     within each cell
#'   \item Calibrating final weights against external monthly population totals
#' }
#'
#' @param data A data.frame or data.table with PNADC microdata including reference
#'   month information (output from \code{\link{identify_reference_month}}).
#'   Required columns include:
#'   \itemize{
#'     \item Reference month columns: \code{ref_month_yyyymm}, \code{ref_month_in_quarter}
#'     \item Survey design: \code{V1028}, \code{UF}, \code{posest}, \code{posest_sxi}
#'     \item Demographics: \code{V2009} (age)
#'     \item Time: \code{Ano}, \code{Trimestre}
#'   }
#'
#' @param monthly_totals A data.frame with monthly population totals. Required columns:
#'   \itemize{
#'     \item \code{ref_month_yyyymm} or \code{anomesexato}: YYYYMM integer
#'     \item \code{m_populacao}: Monthly population in thousands
#'   }
#'
#' @param n_cells Integer. Number of hierarchical cell levels to use (1-4).
#'   Default is 4 (full hierarchy). Lower values are faster but less precise.
#'
#' @param keep_all Logical. If TRUE (default), return all rows including those
#'   with indeterminate reference months (with \code{weight_calibrated = NA}).
#'   If FALSE, only return observations with determined reference months.
#'
#' @param verbose Logical. Print progress messages? Default TRUE.
#'
#' @return A data.table with the input data plus:
#'   \itemize{
#'     \item \code{weight_calibrated}: Calibrated monthly weight
#'     \item \code{celula1} through \code{celula4}: Cell identifiers (if computed)
#'   }
#'
#' @details
#' The hierarchical calibration cells are:
#' \describe{
#'   \item{celula1}{Age groups: 0-13, 14-29, 30-59, 60+}
#'   \item{celula2}{Post-stratum group + age group}
#'   \item{celula3}{State (UF) + celula2}
#'   \item{celula4}{Post-stratum (posest) + celula2}
#' }
#'
#' At each level, weights are adjusted so that:
#' \code{sum(weight_new) by (cell, month) / sum(weight_old) by (cell, month)}
#' equals
#' \code{sum(V1028) by (cell, quarter) / sum(weight_old) by (cell, month)}
#'
#' This preserves the quarterly totals while redistributing to months.
#'
#' @note
#' **Important**: This function is designed for **quarterly** PNADC data where all 5
#' panel visits are present. For **annual** PNADC analysis (visit-specific data),
#' use \code{\link{pnadc_apply_periods}} with \code{anchor = "year"} instead.
#'
#' @examples
#' \dontrun{
#' # Recommended workflow using main API:
#' crosswalk <- pnadc_identify_periods(pnadc_stacked)
#' result <- pnadc_apply_periods(pnadc_2023, crosswalk,
#'                               weight_var = "V1028",
#'                               anchor = "quarter")
#'
#' # Alternative: manual workflow with this function
#' crosswalk <- pnadc_identify_periods(pnadc_stacked)
#' merged <- merge(pnadc_data, crosswalk, by = c("Ano", "Trimestre", "UPA", "V1008", "V1014"))
#' monthly_pop <- fetch_monthly_population()
#' result <- calibrate_monthly_weights(merged, monthly_pop)
#' }
#'
#' @seealso \code{\link{pnadc_apply_periods}} for the recommended unified workflow,
#'   \code{\link{pnadc_identify_periods}} to build crosswalks
#'
#' @export
calibrate_monthly_weights <- function(data, monthly_totals, n_cells = 4L,
                                       keep_all = TRUE, verbose = TRUE) {

  checkmate::assert_int(n_cells, lower = 1L, upper = 4L)
  checkmate::assert_logical(keep_all, len = 1)
  checkmate::assert_logical(verbose, len = 1)

  # Note: PNADC data validation is done in pnadc_apply_periods() for fail-fast behavior.
  # Only validate monthly_totals here since it's not validated upstream.
  validate_monthly_totals(monthly_totals, stop_on_error = TRUE)

  # Convert to data.table
  dt <- ensure_data_table(data, copy = TRUE)

  # Standardize monthly totals column name
  mt <- ensure_data_table(monthly_totals, copy = TRUE)
  if ("anomesexato" %in% names(mt) && !"ref_month_yyyymm" %in% names(mt)) {
    mt[, ref_month_yyyymm := anomesexato]
  }

  # Store indeterminate rows if keeping all
  dt_indeterminate <- NULL
  if (keep_all) {
    dt_indeterminate <- dt[is.na(ref_month_in_quarter)]
  }

  # Filter to determined observations for calibration
  dt <- dt[!is.na(ref_month_in_quarter)]

  if (nrow(dt) == 0) {
    warning("No observations with determined reference month")
    if (keep_all && !is.null(dt_indeterminate) && nrow(dt_indeterminate) > 0) {
      dt_indeterminate[, weight_calibrated := NA_real_]
      return(dt_indeterminate)
    }
    return(dt)
  }

  # Create quarter identifier
  dt[, quarter_yyyyq := Ano * 10L + Trimestre]

  # Step 1: Create calibration cells
  dt <- create_calibration_cells(dt)

  # Step 2: Iterative hierarchical reweighting
  dt[, weight_current := V1028]

  for (level in seq_len(n_cells)) {
    cell_var <- paste0("celula", level)
    dt <- reweight_at_cell_level(dt, cell_var)
  }

  # Step 3: Final calibration against monthly population totals
  dt <- calibrate_to_monthly_totals(dt, mt)

  # Rename final weight
  data.table::setnames(dt, "weight_current", "weight_calibrated")

  # Clean up temporary columns
  temp_cols <- c("quarter_yyyyq", "pop_quarter", "pop_month")
  dt[, (intersect(temp_cols, names(dt))) := NULL]

  # Merge back indeterminate rows if keep_all = TRUE
  if (keep_all && !is.null(dt_indeterminate) && nrow(dt_indeterminate) > 0) {
    dt_indeterminate[, weight_calibrated := NA_real_]
    # Add calibration cell columns as NA
    for (col in c("celula1", "celula2", "celula3", "celula4")) {
      if (col %in% names(dt) && !col %in% names(dt_indeterminate)) {
        dt_indeterminate[, (col) := NA_integer_]
      }
    }
    dt <- data.table::rbindlist(list(dt, dt_indeterminate), use.names = TRUE, fill = TRUE)
  }

  dt
}

#' Create Hierarchical Calibration Cells
#'
#' Creates the nested cell identifiers for hierarchical reweighting.
#'
#' @param dt data.table with PNADC data
#' @return data.table with celula1-4 columns added
#' @keywords internal
#' @noRd
create_calibration_cells <- function(dt) {

  # OPTIMIZATION: Use data.table::set() for more efficient by-reference updates
  # Convert V2009 to integer (saves 4 bytes per row vs numeric, and age is always integer)
  if ("V2009" %in% names(dt)) {
    if (is.character(dt$V2009)) {
      data.table::set(dt, j = "V2009", value = as.integer(dt$V2009))
    } else if (!is.integer(dt$V2009)) {
      data.table::set(dt, j = "V2009", value = as.integer(dt$V2009))
    }
  }
  if ("posest_sxi" %in% names(dt) && !is.integer(dt$posest_sxi)) {
    data.table::set(dt, j = "posest_sxi", value = as.integer(dt$posest_sxi))
  }
  if ("posest" %in% names(dt) && !is.integer(dt$posest)) {
    data.table::set(dt, j = "posest", value = as.integer(dt$posest))
  }
  if ("UF" %in% names(dt) && !is.integer(dt$UF)) {
    data.table::set(dt, j = "UF", value = as.integer(dt$UF))
  }

  # Celula 1: Age groups
  # 0 = 0-13, 1 = 14-29, 2 = 30-59, 3 = 60+
  dt[, celula1 := data.table::fcase(
    V2009 <= 13L, 0L,
    V2009 <= 29L, 1L,
    V2009 <= 59L, 2L,
    default = 3L
  )]

  # Celula 2: Post-stratum group (from posest_sxi) + age
  # posest_sxi encodes region info in first digits
  dt[, celula2 := as.integer(posest_sxi %/% 100L) + 10L * celula1]

  # Celula 3: State (UF) + celula2
  dt[, celula3 := UF + 100L * celula2]

  # Celula 4: Post-stratum (posest) + celula2
  dt[, celula4 := posest + 1000L * celula2]

  dt
}

#' Reweight at Single Cell Level
#'
#' Adjusts weights within each cell so monthly totals approximate quarterly totals.
#'
#' @param dt data.table with weight_current column
#' @param cell_var Name of cell variable to use
#' @return data.table with updated weight_current
#' @keywords internal
#' @noRd
reweight_at_cell_level <- function(dt, cell_var) {

  # OPTIMIZATION: Combine aggregations with same grouping keys (4 passes → 2 passes)
  # Quarter-level aggregations
  dt[, `:=`(
    pop_quarter = sum(V1028, na.rm = TRUE),
    n_cells_quarter = data.table::uniqueN(ref_month_yyyymm)
  ), by = c(cell_var, "quarter_yyyyq")]

  # Month-level aggregations
  dt[, `:=`(
    pop_month = sum(weight_current, na.rm = TRUE),
    n_cells_month = data.table::uniqueN(quarter_yyyyq)
  ), by = c(cell_var, "ref_month_yyyymm")]

  # Apply reweighting ratio, but only if cell structure is stable
  # (more quarterly cells than monthly cells indicates instability)
  dt[, weight_current := data.table::fifelse(
    n_cells_quarter <= n_cells_month & pop_month > 0,
    weight_current * (pop_quarter / pop_month),
    weight_current
  )]

  # Clean up
  dt[, c("pop_quarter", "pop_month", "n_cells_quarter", "n_cells_month") := NULL]

  dt
}

#' Calibrate to Monthly Population Totals
#'
#' Final adjustment to match external monthly population totals.
#'
#' @param dt data.table with calibrated weights
#' @param mt data.table with monthly totals
#' @return data.table with final calibrated weights
#' @keywords internal
#' @noRd
calibrate_to_monthly_totals <- function(dt, mt) {

  # Calculate current monthly totals
  dt[, pop_current := sum(weight_current, na.rm = TRUE),
     by = ref_month_yyyymm]

  # OPTIMIZATION: Use data.table join instead of merge() for speed
  dt[mt, on = .(ref_month_yyyymm), m_populacao := i.m_populacao]

  # Apply final calibration
  # m_populacao is in thousands, so multiply by 1000
  dt[!is.na(m_populacao) & pop_current > 0,
     weight_current := weight_current * (m_populacao * 1000 / pop_current)]

  # Clean up
  dt[, c("pop_current", "m_populacao") := NULL]

  dt
}


#' Calibrate Annual Weights to Monthly Totals
#'
#' Calibrates annual survey weights (V1032) to monthly population totals from SIDRA
#' using hierarchical rake weighting.
#'
#' @description
#' Unlike \code{calibrate_monthly_weights()} which redistributes quarterly totals,
#' this function:
#' \enumerate{
#'   \item Uses V1032 (annual weight) as starting point
#'   \item Applies hierarchical rake reweighting using yearly totals as anchor
#'   \item Final calibration to SIDRA monthly population totals
#' }
#'
#' @param data data.table with annual PNADC data including ref_month_yyyymm, v1032,
#'   and calibration variables (v2009, uf, posest, posest_sxi)
#' @param monthly_totals Optional data.table with monthly population targets.
#'   If NULL (default), fetched from SIDRA.
#' @param n_cells Integer (1-4). Number of hierarchical cell levels. Default 4.
#' @param keep_all Logical. Keep observations without determined month? Default TRUE.
#' @param verbose Logical. Print progress? Default TRUE.
#'
#' @return data.table with \code{weight_monthly} column added
#'
#' @keywords internal
#' @noRd
calibrate_annual_weights <- function(data,
                                      monthly_totals = NULL,
                                      n_cells = 4L,
                                      keep_all = TRUE,
                                      verbose = TRUE) {

  dt <- ensure_data_table(data, copy = TRUE)

  # Standardize column names
  name_map <- c("V1032" = "v1032", "V2009" = "v2009", "UF" = "uf",
                "posest" = "posest", "posest_sxi" = "posest_sxi",
                "Ano" = "ano")
  for (old in names(name_map)) {
    if (old %in% names(dt) && !name_map[old] %in% names(dt)) {
      data.table::setnames(dt, old, name_map[old])
    }
  }

  # Validate required columns
  required <- c("v1032", "ref_month_yyyymm")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing, collapse = ", ")))
  }

  # For hierarchical raking, need additional columns
  if (n_cells > 0) {
    raking_cols <- c("v2009", "uf", "posest", "posest_sxi")
    missing_raking <- setdiff(raking_cols[1:min(n_cells, length(raking_cols))], names(dt))
    if (length(missing_raking) > 0) {
      warning(sprintf("Columns for hierarchical raking missing: %s. Using simple calibration.",
                      paste(missing_raking, collapse = ", ")))
      n_cells <- 0
    }
  }

  # Fetch monthly population if not provided
  if (is.null(monthly_totals)) {
    if (verbose) message("  Fetching monthly population from SIDRA...")
    monthly_totals <- fetch_monthly_population(verbose = FALSE)
  }

  mt <- ensure_data_table(monthly_totals, copy = TRUE)
  if ("anomesexato" %in% names(mt) && !"ref_month_yyyymm" %in% names(mt)) {
    mt[, ref_month_yyyymm := anomesexato]
  }

  # Store indeterminate rows if keeping all
  dt_indeterminate <- NULL
  if (keep_all) {
    dt_indeterminate <- dt[is.na(ref_month_yyyymm)]
  }

  # Filter to determined observations
  dt <- dt[!is.na(ref_month_yyyymm)]

  if (nrow(dt) == 0) {
    warning("No observations with determined reference month")
    if (keep_all && !is.null(dt_indeterminate) && nrow(dt_indeterminate) > 0) {
      dt_indeterminate[, weight_monthly := NA_real_]
      return(dt_indeterminate)
    }
    return(dt)
  }

  # Extract year from ref_month_yyyymm
  dt[, year := ref_month_yyyymm %/% 100L]

  # Initialize weight from V1032
  dt[, weight_current := v1032]

  # Step 1: Create calibration cells (reuse existing function)
  if (n_cells > 0) {
    # Need to standardize column names for create_calibration_cells()
    if (!"V2009" %in% names(dt) && "v2009" %in% names(dt)) {
      dt[, V2009 := v2009]
    }
    if (!"UF" %in% names(dt) && "uf" %in% names(dt)) {
      dt[, UF := uf]
    }

    dt <- create_calibration_cells(dt)

    # Step 2: Iterative hierarchical reweighting (adapted for annual data)
    for (level in seq_len(n_cells)) {
      cell_var <- paste0("celula", level)
      dt <- reweight_annual_at_cell_level(dt, cell_var)
    }
  }

  # Step 3: Final calibration against monthly population totals (reuse existing)
  dt <- calibrate_to_monthly_totals(dt, mt)

  # Rename to final output
  data.table::setnames(dt, "weight_current", "weight_monthly")

  # Clean up temporary columns
  temp_cols <- c("year", "pop_year", "pop_month", "V2009", "UF",
                 "celula1", "celula2", "celula3", "celula4")
  dt[, (intersect(temp_cols, names(dt))) := NULL]

  # Merge back indeterminate rows if keep_all = TRUE
  if (keep_all && !is.null(dt_indeterminate) && nrow(dt_indeterminate) > 0) {
    dt_indeterminate[, weight_monthly := NA_real_]
    dt <- data.table::rbindlist(list(dt, dt_indeterminate), use.names = TRUE, fill = TRUE)
  }

  if (verbose) {
    n_calibrated <- sum(!is.na(dt$weight_monthly))
    message(sprintf("  Calibrated %s observations", format(n_calibrated, big.mark = ",")))
  }

  dt
}


#' Reweight Annual Data at Single Cell Level
#'
#' Adjusts weights within each cell so monthly totals approximate yearly totals
#' divided by number of months.
#'
#' @param dt data.table with weight_current and year columns
#' @param cell_var Name of cell variable to use
#' @return data.table with updated weight_current
#' @keywords internal
#' @noRd
reweight_annual_at_cell_level <- function(dt, cell_var) {

  # Year-level aggregations: total weight in cell-year
  dt[, `:=`(
    pop_year = sum(v1032, na.rm = TRUE),
    n_months_in_year = data.table::uniqueN(ref_month_yyyymm)
  ), by = c(cell_var, "year")]

  # Month-level aggregations: current weight total in cell-month
  dt[, `:=`(
    pop_month = sum(weight_current, na.rm = TRUE),
    n_years_in_month = data.table::uniqueN(year)
  ), by = c(cell_var, "ref_month_yyyymm")]

  # Expected monthly total: yearly total divided by number of months
  # Apply reweighting ratio
  dt[, weight_current := data.table::fifelse(
    n_months_in_year > 0 & pop_month > 0,
    weight_current * ((pop_year / n_months_in_year) / pop_month),
    weight_current
  )]

  # Clean up
  dt[, c("pop_year", "pop_month", "n_months_in_year", "n_years_in_month") := NULL]

  dt
}
