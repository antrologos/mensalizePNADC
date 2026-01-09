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
#' @examples
#' \dontrun{
#' # First identify reference months
#' crosswalk <- identify_reference_month(pnadc_data)
#' merged <- merge(pnadc_data, crosswalk, by = join_keys)
#'
#' # Then calibrate weights
#' result <- calibrate_monthly_weights(merged, monthly_pop)
#' }
#'
#' @param keep_all Logical. If TRUE (default), return all rows including those
#'   with indeterminate reference months (with \code{weight_calibrated = NA}).
#'   If FALSE, only return observations with determined reference months.
#'
#' @param verbose Logical. Print progress messages? Default TRUE.
#'
#' @seealso \code{\link{identify_reference_month}}, \code{\link{calibrate_to_sidra}}
#'
#' @export
calibrate_monthly_weights <- function(data, monthly_totals, n_cells = 4L,
                                       keep_all = TRUE, verbose = TRUE) {

  checkmate::assert_int(n_cells, lower = 1L, upper = 4L)
  checkmate::assert_logical(keep_all, len = 1)
  checkmate::assert_logical(verbose, len = 1)

  # Note: PNADC data validation is done in mensalizePNADC() for fail-fast behavior.
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

  # Ensure numeric types for stratification variables (PNADC data may have character cols)
  if (is.character(dt$V2009)) dt[, V2009 := as.numeric(V2009)]
  if (is.character(dt$posest_sxi)) dt[, posest_sxi := as.integer(posest_sxi)]
  if (is.character(dt$posest)) dt[, posest := as.integer(posest)]
  if (is.character(dt$UF)) dt[, UF := as.integer(UF)]

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

  # Calculate quarterly totals (sum of V1028)
  dt[, pop_quarter := sum(V1028, na.rm = TRUE),
     by = c(cell_var, "quarter_yyyyq")]

  # Calculate monthly totals of current weight
  dt[, pop_month := sum(weight_current, na.rm = TRUE),
     by = c(cell_var, "ref_month_yyyymm")]

  # Count cells in quarter vs month to detect instability
  dt[, n_cells_quarter := data.table::uniqueN(.SD),
     by = c(cell_var, "quarter_yyyyq"),
     .SDcols = "ref_month_yyyymm"]

  dt[, n_cells_month := data.table::uniqueN(.SD),
     by = c(cell_var, "ref_month_yyyymm"),
     .SDcols = "quarter_yyyyq"]

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

  # Merge with target population
  dt <- merge(dt, mt[, .(ref_month_yyyymm, m_populacao)],
              by = "ref_month_yyyymm", all.x = TRUE)

  # Apply final calibration
  # m_populacao is in thousands, so multiply by 1000
  dt[!is.na(m_populacao) & pop_current > 0,
     weight_current := weight_current * (m_populacao * 1000 / pop_current)]

  # Clean up
  dt[, c("pop_current", "m_populacao") := NULL]

  dt
}
