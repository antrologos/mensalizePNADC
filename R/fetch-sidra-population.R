#' @title Fetch Monthly Population from SIDRA
#' @description Functions to download and transform IBGE's population estimates
#'   from SIDRA API for use in monthly weight calibration.
#' @name fetch-sidra-population
NULL

#' Fetch Monthly Population from SIDRA
#'
#' Downloads population estimates from IBGE SIDRA API (table 6022) and
#' transforms from moving-quarter to exact monthly values.
#'
#' @param start_yyyymm Integer. First month to include (YYYYMM format).
#'   If NULL, returns all available months.
#' @param end_yyyymm Integer. Last month to include (YYYYMM format).
#'   If NULL, returns all available months.
#' @param verbose Logical. Print progress messages? Default TRUE.
#'
#' @return A data.table with columns:
#'   \itemize{
#'     \item \code{ref_month_yyyymm}: Integer in YYYYMM format
#'     \item \code{m_populacao}: Monthly population in thousands
#'   }
#'
#' @details
#' SIDRA table 6022 provides moving-quarter population estimates. Each value
#' represents the 3-month average centered on the middle month. For example,
#' the value for code 201203 (quarter ending March 2012) represents the
#' population for February 2012.
#'
#' This function:
#' \enumerate{
#'   \item Fetches raw moving-quarter data from SIDRA
#'   \item Transforms to exact monthly values by aligning with middle months
#'   \item Extrapolates boundary months (first and last) using quadratic regression
#' }
#'
#' The extrapolation follows the methodology in Hecksher (2024), using quadratic
#' regression on population differences to estimate the first month (Jan 2012)
#' and the most recent month.
#'
#' @section Dependencies:
#' This function requires the \code{sidrar} package for API access.
#' Install with: \code{install.packages("sidrar")}
#'
#' @examples
#' \dontrun{
#' # Fetch all available months
#' pop <- fetch_monthly_population()
#'
#' # Fetch specific date range
#' pop <- fetch_monthly_population(201301, 201912)
#' }
#'
#' @seealso \code{\link{mensalizePNADC}} which uses this function when
#'   \code{compute_weights = TRUE}
#'
#' @export
fetch_monthly_population <- function(start_yyyymm = NULL,
                                      end_yyyymm = NULL,
                                      verbose = TRUE) {

 # Check for sidrar package
 if (!requireNamespace("sidrar", quietly = TRUE)) {
   stop(
     "Package 'sidrar' is required for fetching population from SIDRA.\n",
     "Install with: install.packages('sidrar')",
     call. = FALSE
   )
 }

 if (verbose) message("  Fetching population from SIDRA API (table 6022)...")

 # Fetch from SIDRA
 # Table 6022: Population estimates from PNADC
 # Variable 606: Population (in thousands)
 # n1/all: National level
 # p/all: All periods
 raw <- tryCatch({
   # suppressMessages to hide sidrar's "All others arguments are desconsidered when 'api' is informed"
   suppressMessages(sidrar::get_sidra(api = "/t/6022/n1/all/v/606/p/all"))
 }, error = function(e) {
   stop(
     "Failed to fetch from SIDRA API. Check internet connection.\n",
     "Error: ", conditionMessage(e),
     call. = FALSE
   )
 })

 if (verbose) message("  Transforming moving-quarter to exact months...")

 # Convert to data.table and extract relevant columns
 dt <- data.table::as.data.table(raw)

 # The column name may vary; find the moving quarter code column
 code_col <- grep("Trimestre.*vel.*digo|trimestre.*vel.*digo",
                  names(dt), value = TRUE, ignore.case = TRUE)
 if (length(code_col) == 0) {
   # Try alternative pattern
   code_col <- grep("M.*vel.*C.*digo", names(dt), value = TRUE, ignore.case = TRUE)
 }
 if (length(code_col) == 0) {
   stop("Could not find moving quarter code column in SIDRA response")
 }
 code_col <- code_col[1]

 dt <- dt[, .(
   anomesfinaltrimmovel = as.integer(get(code_col)),
   populacao = as.numeric(Valor)
 )]

 # Remove any rows with NA codes
 dt <- dt[!is.na(anomesfinaltrimmovel)]

 # Sort by moving quarter code
 data.table::setorder(dt, anomesfinaltrimmovel)

 # Transform moving quarter to exact month
 dt <- transform_moving_quarter_to_monthly(dt, verbose = verbose)

 # Apply quadratic extrapolation for boundary months
 if (verbose) message("  Extrapolating boundary months...")
 dt <- extrapolate_boundary_months(dt)

 # Rename to standard output column name
 data.table::setnames(dt, "anomesexato", "ref_month_yyyymm")

 # Filter to requested date range if specified
 if (!is.null(start_yyyymm)) {
   dt <- dt[ref_month_yyyymm >= start_yyyymm]
 }
 if (!is.null(end_yyyymm)) {
   dt <- dt[ref_month_yyyymm <= end_yyyymm]
 }

 # Keep only final columns
 dt <- dt[, .(ref_month_yyyymm, m_populacao)]

 if (verbose) {
   message("  Population data: ",
           min(dt$ref_month_yyyymm), " to ", max(dt$ref_month_yyyymm),
           " (", nrow(dt), " months)")
 }

 dt
}


#' Transform Moving Quarter to Monthly Population
#'
#' Internal function that transforms SIDRA moving-quarter population values
#' to exact monthly values.
#'
#' @param dt data.table with columns anomesfinaltrimmovel and populacao
#' @param verbose Logical. Print messages?
#' @return data.table with columns anomesexato and m_populacao
#' @keywords internal
#' @noRd
transform_moving_quarter_to_monthly <- function(dt, verbose = FALSE) {

 # The moving quarter code (e.g., 201203) represents the ENDING month
 # of a 3-month window. The population value is for the MIDDLE month.
 #
 # Example:
 #   Code 201203 = Jan+Feb+Mar window = Feb 2012 population
 #   Code 201204 = Feb+Mar+Apr window = Mar 2012 population
 #
 # To get exact monthly values:
 # 1. Add dummy rows for the first two months (Jan and Feb of first year)
 # 2. Sort by date
 # 3. Shift: m_populacao[n] = populacao[n+1]
 #
 # After this shift:
 #   anomesexato=201201 gets populacao from row with code=201202 -> NA
 #   anomesexato=201202 gets populacao from row with code=201203 -> Feb value
 #   anomesexato=201203 gets populacao from row with code=201204 -> Mar value

 # Get the first moving quarter code (e.g., 201203)
 first_code <- min(dt$anomesfinaltrimmovel)
 year_start <- first_code %/% 100L

 # Add two dummy rows for Jan and Feb of the first year
 # These will have populacao = NA (need extrapolation later)
 dummy_rows <- data.table::data.table(
   anomesfinaltrimmovel = c(year_start * 100L + 1L, year_start * 100L + 2L),
   populacao = NA_real_
 )
 dt <- data.table::rbindlist(list(dummy_rows, dt))

 # Create anomesexato (exact month) = same as anomesfinaltrimmovel for now
 dt[, anomesexato := anomesfinaltrimmovel]

 # Sort by exact month
 data.table::setorder(dt, anomesexato)

 # Shift: m_populacao gets value from NEXT row's populacao
 # This aligns the moving quarter value with its middle month
 # Stata: gen m_populacao = populacao[_n+1] is equivalent to shift(x, n=1, type="lead")
 dt[, m_populacao := data.table::shift(populacao, n = 1L, type = "lead")]

 # Clean up
 dt[, populacao := NULL]
 dt[, anomesfinaltrimmovel := NULL]

 dt
}


#' Extrapolate Boundary Months Using Quadratic Regression
#'
#' Internal function that fills in NA values for the first and last months
#' of the population series using quadratic extrapolation on differences.
#'
#' @param dt data.table with columns anomesexato and m_populacao
#' @return data.table with extrapolated values for boundary months
#' @keywords internal
#' @noRd
extrapolate_boundary_months <- function(dt) {

 n <- nrow(dt)

 # Create row indices for regression
 dt[, row_num := .I]
 dt[, row_num2 := row_num^2]

 # First differences of population
 dt[, d_pop := m_populacao - data.table::shift(m_populacao, 1L)]

 # Extrapolate FIRST month (row 1) using quadratic regression on first 26 observations
 # This fills in Jan 2012 which has no moving quarter centered on it
 if (n >= 26 && is.na(dt[1, m_populacao])) {
   # Fit quadratic model on first 26 rows (excluding row 1 which has NA d_pop)
   fit_data <- dt[2:26]
   if (sum(!is.na(fit_data$d_pop)) >= 3) {
     fit_start <- stats::lm(d_pop ~ row_num + row_num2, data = fit_data)
     predicted_diff_2 <- stats::predict(fit_start, newdata = dt[2])

     # Extrapolate: value[1] = value[2] - predicted_diff[2]
     dt[1, m_populacao := dt[2, m_populacao] - round(predicted_diff_2)]
   }
 }

 # Extrapolate LAST month (row n) using quadratic regression on last 25 observations
 # This fills in the final month which has no moving quarter centered on it
 if (n >= 25 && is.na(dt[n, m_populacao])) {
   # Fit quadratic model on last 25 rows (excluding row n which has NA)
   start_row <- max(1, n - 24)
   fit_data <- dt[start_row:(n-1)]
   if (sum(!is.na(fit_data$d_pop)) >= 3) {
     fit_end <- stats::lm(d_pop ~ row_num + row_num2, data = fit_data)
     predicted_diff_n <- stats::predict(fit_end, newdata = dt[n])

     # Extrapolate: value[n] = value[n-1] + predicted_diff[n]
     dt[n, m_populacao := dt[n-1, m_populacao] + round(predicted_diff_n)]
   }
 }

 # Clean up temporary columns
 dt[, c("row_num", "row_num2", "d_pop") := NULL]

 dt
}
