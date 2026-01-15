#' Calibrate Weekly Weights
#'
#' Redistributes quarterly survey weights to weekly weights using hierarchical
#' calibration across demographic and geographic cells.
#'
#' @description
#' The original PNADC survey weights (\code{V1028}) are designed for quarterly
#' estimates. This function creates weekly weights by:
#' \enumerate{
#'   \item Grouping observations by nested demographic/geographic cells
#'   \item Iteratively adjusting weights so weekly totals match quarterly totals
#'     within each cell
#'   \item Calibrating final weights against external weekly population totals
#' }
#'
#' **Important:** Weekly population targets are interpolated from monthly SIDRA
#' data (Table 6022) because IBGE does not publish official weekly estimates.
#' The resulting weights are approximations.
#'
#' @param data A data.frame or data.table with PNADC microdata including reference
#'   week information (output from \code{\link{identify_reference_week}}).
#'   Required columns include:
#'   \itemize{
#'     \item Reference week columns: \code{ref_week_iso_yyyyww}, \code{ref_week_in_quarter}
#'     \item Survey design: \code{V1028}, \code{UF}, \code{posest}, \code{posest_sxi}
#'     \item Demographics: \code{V2009} (age)
#'     \item Time: \code{Ano}, \code{Trimestre}
#'   }
#'
#' @param weekly_totals A data.frame with weekly population totals. Required columns:
#'   \itemize{
#'     \item \code{ref_week_iso_yyyyww}: YYYYWW integer (ISO year-week)
#'     \item \code{w_populacao}: Weekly population in thousands
#'   }
#'
#' @param n_cells Integer. Number of hierarchical cell levels to use (1-4).
#'   Default is 4 (full hierarchy). Lower values are faster but less precise.
#'
#' @param keep_all Logical. If TRUE (default), return all rows including those
#'   with indeterminate reference weeks (with \code{weight_weekly = NA}).
#'   If FALSE, only return observations with determined reference weeks.
#'
#' @param verbose Logical. Print progress messages? Default TRUE.
#'
#' @return A data.table with the input data plus:
#'   \itemize{
#'     \item \code{weight_weekly}: Calibrated weekly weight
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
#' \code{sum(weight_new) by (cell, week) / sum(weight_old) by (cell, week)}
#' equals
#' \code{sum(V1028) by (cell, quarter) / sum(weight_old) by (cell, week)}
#'
#' This preserves the quarterly totals while redistributing to weeks.
#'
#' @seealso \code{\link{identify_reference_week}}, \code{\link{semanalizePNADC}}
#'
#' @export
calibrate_weekly_weights <- function(data, weekly_totals, n_cells = 4L,
                                      keep_all = TRUE, verbose = TRUE) {

  checkmate::assert_int(n_cells, lower = 1L, upper = 4L)
  checkmate::assert_logical(keep_all, len = 1)
  checkmate::assert_logical(verbose, len = 1)

  # Validate weekly totals
  if (!"ref_week_iso_yyyyww" %in% names(weekly_totals)) {
    stop("weekly_totals must contain 'ref_week_iso_yyyyww' column")
  }
  if (!"w_populacao" %in% names(weekly_totals)) {
    stop("weekly_totals must contain 'w_populacao' column")
  }

  # Convert to data.table
  dt <- ensure_data_table(data, copy = TRUE)
  wt <- ensure_data_table(weekly_totals, copy = TRUE)

  # Store indeterminate rows if keeping all
  dt_indeterminate <- NULL
  if (keep_all) {
    dt_indeterminate <- dt[is.na(ref_week_in_quarter)]
  }

  # Filter to determined observations for calibration
  dt <- dt[!is.na(ref_week_in_quarter)]

  if (nrow(dt) == 0) {
    warning("No observations with determined reference week")
    if (keep_all && !is.null(dt_indeterminate) && nrow(dt_indeterminate) > 0) {
      dt_indeterminate[, weight_weekly := NA_real_]
      return(dt_indeterminate)
    }
    return(dt)
  }

  # Create quarter identifier
  dt[, quarter_yyyyq := Ano * 10L + Trimestre]

  # Step 1: Create calibration cells (reuse existing function)
  dt <- create_calibration_cells(dt)

  # Step 2: Iterative hierarchical reweighting
  dt[, weight_current := V1028]

  for (level in seq_len(n_cells)) {
    cell_var <- paste0("celula", level)
    dt <- reweight_weekly_at_cell_level(dt, cell_var)
  }

  # Step 3: Final calibration against weekly population totals
  dt <- calibrate_to_weekly_totals(dt, wt)

  # Rename final weight
  data.table::setnames(dt, "weight_current", "weight_weekly")

  # Clean up temporary columns
  temp_cols <- c("quarter_yyyyq", "pop_quarter", "pop_week")
  dt[, (intersect(temp_cols, names(dt))) := NULL]

  # Merge back indeterminate rows if keep_all = TRUE
  if (keep_all && !is.null(dt_indeterminate) && nrow(dt_indeterminate) > 0) {
    dt_indeterminate[, weight_weekly := NA_real_]
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


#' Reweight Weekly Data at Single Cell Level
#'
#' Adjusts weights within each cell so weekly totals approximate quarterly totals.
#'
#' @param dt data.table with weight_current column
#' @param cell_var Name of cell variable to use
#' @return data.table with updated weight_current
#' @keywords internal
#' @noRd
reweight_weekly_at_cell_level <- function(dt, cell_var) {

  # Quarter-level aggregations
  dt[, `:=`(
    pop_quarter = sum(V1028, na.rm = TRUE),
    n_cells_quarter = data.table::uniqueN(ref_week_iso_yyyyww)
  ), by = c(cell_var, "quarter_yyyyq")]

  # Week-level aggregations
  dt[, `:=`(
    pop_week = sum(weight_current, na.rm = TRUE),
    n_cells_week = data.table::uniqueN(quarter_yyyyq)
  ), by = c(cell_var, "ref_week_iso_yyyyww")]

  # Apply reweighting ratio, but only if cell structure is stable
  dt[, weight_current := data.table::fifelse(
    n_cells_quarter <= n_cells_week & pop_week > 0,
    weight_current * (pop_quarter / pop_week),
    weight_current
  )]

  # Clean up
  dt[, c("pop_quarter", "pop_week", "n_cells_quarter", "n_cells_week") := NULL]

  dt
}


#' Calibrate to Weekly Population Totals
#'
#' Final adjustment to match external weekly population totals.
#'
#' @param dt data.table with calibrated weights
#' @param wt data.table with weekly totals
#' @return data.table with final calibrated weights
#' @keywords internal
#' @noRd
calibrate_to_weekly_totals <- function(dt, wt) {

  # Calculate current weekly totals
  dt[, pop_current := sum(weight_current, na.rm = TRUE),
     by = ref_week_iso_yyyyww]

  # Join with weekly targets
  dt[wt, on = .(ref_week_iso_yyyyww), w_populacao := i.w_populacao]

  # Apply final calibration
  # w_populacao is in thousands, so multiply by 1000
  dt[!is.na(w_populacao) & pop_current > 0,
     weight_current := weight_current * (w_populacao * 1000 / pop_current)]

  # Clean up
  dt[, c("pop_current", "w_populacao") := NULL]

  dt
}
