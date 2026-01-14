#' Smooth Monthly Aggregates
#'
#' Applies moving average methodology to remove quarterly artifacts from
#' monthly estimates derived from quarterly survey data.
#'
#' @description
#' When quarterly survey data is converted to monthly estimates, the resulting
#' time series often shows artificial quarterly patterns. This function applies
#' a smoothing algorithm that:
#' \enumerate{
#'   \item Computes 3-month differences to capture trends
#'   \item Stratifies by month position within quarter (1st, 2nd, 3rd)
#'   \item Accumulates changes via cumulative sums
#'   \item Calibrates against a reference period
#'   \item Produces smooth monthly series that preserve genuine variation
#' }
#'
#' @param monthly_aggregates A data.table with monthly aggregated indicators.
#'   Required columns:
#'   \itemize{
#'     \item \code{ref_month_yyyymm}: Month identifier in YYYYMM format
#'     \item \code{z_*}: Indicator columns with monthly totals (e.g., z_popocup)
#'   }
#'
#' @param calibration_start Integer YYYYMM. Start of calibration period. Default 201301.
#' @param calibration_end Integer YYYYMM. End of calibration period. Default 201912.
#'
#' @return A data.table with smoothed monthly estimates. For each input variable
#'   \code{z_varname}, the output includes:
#'   \itemize{
#'     \item \code{m_varname}: Smoothed monthly estimate
#'   }
#'
#' @details
#' The smoothing algorithm works as follows:
#'
#' For each indicator variable:
#' \enumerate{
#'   \item Compute 3-month difference: \code{d3 = 3 * (value[t] - value[t-1])}
#'   \item Split by month position (1, 2, 3 within quarter)
#'   \item Compute cumulative sum within each position
#'   \item Calculate residual from intermediate series during calibration period
#'   \item Apply position-specific adjustment
#'   \item Final smoothed value uses moving average of quarterly reference
#' }
#'
#' @examples
#' \dontrun{
#' # After calibrating weights, aggregate to monthly totals
#' monthly_agg <- calibrated_data[, .(z_populacao = sum(weight_calibrated)), by = ref_month_yyyymm]
#' smoothed <- smooth_monthly_aggregates(monthly_agg)
#' }
#'
#' @seealso \code{\link{calibrate_monthly_weights}}, \code{\link{mensalizePNADC}}
#'
#' @export
smooth_monthly_aggregates <- function(monthly_aggregates,
                                       calibration_start = 201301L,
                                       calibration_end = 201912L) {

  checkmate::assert_data_frame(monthly_aggregates, min.rows = 3)
  checkmate::assert_int(calibration_start)
  checkmate::assert_int(calibration_end)

  dt <- ensure_data_table(monthly_aggregates, copy = TRUE)

  # Ensure sorted by month
  data.table::setorder(dt, ref_month_yyyymm)

  # Extract year and month components
  dt[, `:=`(
    year = ref_month_yyyymm %/% 100L,
    month = ref_month_yyyymm %% 100L
  )]

  # Calculate month position in quarter (1, 2, or 3)
  dt[, month_pos := ((month - 1L) %% 3L) + 1L]

  # Find all z_ prefixed indicator columns
  z_vars <- grep("^z_", names(dt), value = TRUE)

  if (length(z_vars) == 0) {
    warning("No indicator columns (z_*) found")
    return(dt)
  }

  # Process each indicator
  for (zvar in z_vars) {
    # Extract base name (without z_ prefix)
    base_name <- sub("^z_", "", zvar)
    m_var <- paste0("m_", base_name)

    # Apply smoothing algorithm
    dt <- smooth_single_variable(dt, zvar, m_var,
                                  calibration_start, calibration_end)
  }

  # Clean up temporary columns
  dt[, c("year", "month", "month_pos") := NULL]

  dt
}

#' Smooth Single Variable
#'
#' Applies the moving average smoothing algorithm to a single indicator variable.
#' OPTIMIZED: Uses vectorized operations instead of row-by-row loops.
#'
#' @param dt data.table with monthly aggregates
#' @param z_var Name of input variable (z_ prefix)
#' @param m_var Name of output variable (m_ prefix)
#' @param cal_start Calibration period start (YYYYMM)
#' @param cal_end Calibration period end (YYYYMM)
#' @return data.table with m_var column added
#' @keywords internal
#' @noRd
smooth_single_variable <- function(dt, z_var, m_var, cal_start, cal_end) {

  n <- nrow(dt)

  # Get values and scale (z_ values are typically in thousands or millions)
  values <- dt[[z_var]]

  # Handle missing values
  if (all(is.na(values))) {
    dt[[m_var]] <- NA_real_
    return(dt)
  }

  # Step 1: Compute 3-month differences
  # d3[t] = 3 * (value[t] - value[t-1])
  d3 <- c(NA_real_, 3 * diff(values))

  # Steps 2-3: OPTIMIZED - Cumulative sum by month position using data.table
  # Instead of split/lapply/reconstruct, use native grouped cumsum
  month_pos <- dt$month_pos
  dt_temp <- data.table::data.table(
    d3 = d3,
    month_pos = month_pos,
    row_idx = seq_len(n)
  )
  dt_temp[, d3_filled := data.table::fifelse(is.na(d3), 0, d3)]
  dt_temp[, cum_values := cumsum(d3_filled), by = month_pos]
  dt_temp[is.na(d3), cum_values := NA_real_]
  data.table::setorder(dt_temp, row_idx)
  cum_values <- dt_temp$cum_values

  # Step 4: Calculate residual from intermediate series (during calibration period)
  # Scale factor: values are typically in thousands, we work in base units
  scale <- determine_scale(values)
  scaled_values <- values / scale

  # Residual: difference between scaled intermediate series and cumulative
  in_cal <- dt$ref_month_yyyymm >= cal_start & dt$ref_month_yyyymm <= cal_end
  e0 <- scaled_values - cum_values
  e0[!in_cal] <- NA

  # Step 5: OPTIMIZED - Calculate mean residual by month position using data.table
  dt_temp[, e0 := e0]
  dt_temp[, mean_e0 := mean(e0, na.rm = TRUE), by = month_pos]
  dt_temp[!is.finite(mean_e0), mean_e0 := 0]
  y0 <- dt_temp$mean_e0

  # Step 6: Adjusted values
  y <- y0 + cum_values

  # Step 7: OPTIMIZED - Moving average correction using vectorized shift operations
  # Pre-compute shifted values
  y_lead1 <- data.table::shift(y, n = 1L, type = "lead")
  y_lead2 <- data.table::shift(y, n = 2L, type = "lead")
  y_lag1 <- data.table::shift(y, n = 1L, type = "lag")
  y_lag2 <- data.table::shift(y, n = 2L, type = "lag")

  # Pre-compute quarterly averages for each position type (vectorized)
  quarterly_avg_pos1 <- (y + y_lead1 + y_lead2) / 3  # pos 1: look forward 2
  quarterly_avg_pos2 <- (y_lag1 + y + y_lead1) / 3   # pos 2: look back 1, forward 1
  quarterly_avg_pos3 <- (y_lag2 + y_lag1 + y) / 3    # pos 3: look back 2

  # Pre-compute quarterly references (scaled values at appropriate positions)
  values_lead2 <- data.table::shift(values, n = 2L, type = "lead")
  values_lead1 <- data.table::shift(values, n = 1L, type = "lead")

  # Vectorized conditional calculation using fcase
  m_values <- data.table::fcase(
    # Position 1: look forward 2 months
    month_pos == 1L & !is.na(y_lead2),
    y + values_lead2 / scale - quarterly_avg_pos1,

    # Position 2: look forward 1, back 1
    month_pos == 2L & !is.na(y_lead1) & !is.na(y_lag1),
    y + values_lead1 / scale - quarterly_avg_pos2,

    # Position 3: look back 2 months
    month_pos == 3L & !is.na(y_lag2),
    y + values / scale - quarterly_avg_pos3,

    # Default: use y when conditions not met
    default = y
  )

  # Scale back
  dt[[m_var]] <- m_values * scale

  dt
}

#' Determine Scale Factor
#'
#' Determines whether values are in thousands, millions, etc.
#'
#' @param values Numeric vector
#' @return Scale factor (1, 1000, 1000000, etc.)
#' @keywords internal
#' @noRd
determine_scale <- function(values) {
  # Typical PNADC values: population ~200 million, employment ~100 million
  # If values are > 100000, they're likely in base units
  # If values are > 100 but < 100000, they're likely in thousands
  # If values are < 100, they're likely in millions

  median_val <- median(abs(values), na.rm = TRUE)

  if (is.na(median_val) || median_val == 0) {
    return(1)
  }

  if (median_val > 100000) {
    return(1)
  } else if (median_val > 100) {
    return(1000)
  } else {
    return(1000000)
  }
}
