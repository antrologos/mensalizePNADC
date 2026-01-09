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
#' @seealso \code{\link{calibrate_monthly_weights}}, \code{\link{calibrate_to_sidra}}
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

  # Step 2: Split by month position
  month_pos <- dt$month_pos
  d3_by_pos <- split(d3, month_pos)

  # Step 3: Cumulative sum within each position
  cum_by_pos <- lapply(d3_by_pos, function(x) {
    result <- cumsum(ifelse(is.na(x), 0, x))
    result[is.na(x)] <- NA
    result
  })

  # Reconstruct cumulative sum in original order
  cum_values <- numeric(n)
  for (pos in 1:3) {
    idx <- which(month_pos == pos)
    cum_values[idx] <- cum_by_pos[[as.character(pos)]]
  }

  # Step 4: Calculate residual from intermediate series (during calibration period)
  # Scale factor: values are typically in thousands, we work in base units
  scale <- determine_scale(values)
  scaled_values <- values / scale

  # Residual: difference between scaled intermediate series and cumulative
  in_cal <- dt$ref_month_yyyymm >= cal_start & dt$ref_month_yyyymm <= cal_end
  e0 <- scaled_values - cum_values
  e0[!in_cal] <- NA

  # Step 5: Calculate mean residual by month position (during calibration)
  y0 <- numeric(n)
  for (pos in 1:3) {
    idx <- which(month_pos == pos)
    mean_e0 <- mean(e0[idx], na.rm = TRUE)
    y0[idx] <- ifelse(is.finite(mean_e0), mean_e0, 0)
  }

  # Step 6: Adjusted values
  y <- y0 + cum_values

  # Step 7: Moving average correction
  # The correction ensures smooth series matches quarterly totals
  m_values <- numeric(n)

  for (i in seq_len(n)) {
    pos <- month_pos[i]

    if (pos == 1L) {
      # First month of quarter: look forward 2 months
      if (i + 2 <= n) {
        quarterly_ref <- values[i + 2] / scale
        quarterly_avg <- mean(y[i:(i+2)], na.rm = TRUE)
        m_values[i] <- y[i] + quarterly_ref - quarterly_avg
      } else {
        m_values[i] <- y[i]
      }
    } else if (pos == 2L) {
      # Second month: look forward 1, back 1
      if (i + 1 <= n && i - 1 >= 1) {
        quarterly_ref <- values[i + 1] / scale
        quarterly_avg <- mean(y[(i-1):(i+1)], na.rm = TRUE)
        m_values[i] <- y[i] + quarterly_ref - quarterly_avg
      } else {
        m_values[i] <- y[i]
      }
    } else {
      # Third month: look back 2 months
      if (i - 2 >= 1) {
        quarterly_ref <- values[i] / scale
        quarterly_avg <- mean(y[(i-2):i], na.rm = TRUE)
        m_values[i] <- y[i] + quarterly_ref - quarterly_avg
      } else {
        m_values[i] <- y[i]
      }
    }
  }

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
