#' Mensalize PNADC Quarterly Survey Data
#'
#' Main function to convert quarterly PNADC survey data to monthly time series.
#' Returns a crosswalk data.frame for joining with original data, adding
#' reference month information and optionally monthly survey weights.
#'
#' @description
#' This function processes stacked quarterly PNADC microdata to:
#' \enumerate{
#'   \item Identify which month within each quarter each observation refers to
#'   \item Optionally compute monthly adjusted survey weights (if \code{compute_weights = TRUE})
#' }
#'
#' The output is a crosswalk table that can be joined with original (stacked or unstacked)
#' PNADC data files to add monthly time information.
#'
#' @param data A data.frame or data.table with stacked quarterly PNADC microdata.
#'
#'   For reference month identification only (\code{compute_weights = FALSE}),
#'   minimum required columns are:
#'   \itemize{
#'     \item \code{Ano}, \code{Trimestre}: Year and quarter
#'     \item \code{UPA}, \code{V1014}: Primary sampling unit and panel
#'     \item \code{V2008}, \code{V20081}, \code{V20082}: Birth day, month, year
#'     \item \code{V2009}: Age
#'   }
#'
#'   For monthly weight computation (\code{compute_weights = TRUE}), additional
#'   columns are required. See Details.
#'
#' @param compute_weights Logical. If TRUE, compute monthly adjusted survey weights in
#'   addition to identifying reference months. Default is FALSE. Requires the
#'   \code{sidrar} package to fetch population data from IBGE SIDRA API. It makes the monthly
#'   weights sum up to the population results at https://sidra.ibge.gov.br/tabela/6022.
#'
#' @param output Character. What to return:
#'   \itemize{
#'     \item \code{"crosswalk"} (default): Minimal crosswalk for joining
#'     \item \code{"microdata"}: Full microdata with all computed columns
#'     \item \code{"aggregates"}: Monthly aggregated indicators
#'   }
#'
#' @param keep_all Logical. If TRUE (default), return all rows from input data,
#'   with \code{weight_monthly = NA} for observations where reference month
#'   could not be determined. If FALSE, only return observations with
#'   determined reference months (smaller output). Only applies when
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
#'     \item \code{ref_month}: Reference month as Date
#'     \item \code{ref_month_in_quarter}: Position in quarter (1, 2, 3) or NA
#'     \item \code{ref_month_yyyymm}: Integer YYYYMM format
#'     \item \code{weight_monthly}: Monthly weight (if \code{compute_weights = TRUE})
#'   }
#'
#'   If \code{output = "microdata"}:
#'   Full input data with all computed columns added.
#'
#'   If \code{output = "aggregates"}:
#'   Monthly aggregated indicators.
#'
#' @details
#' ## Reference Month Identification
#'
#' The algorithm determines which month each survey response refers to based on:
#' \itemize{
#'   \item IBGE's "Parada Tecnica" rules for reference week timing
#'   \item Respondent birthdates (constrains possible interview dates)
#'   \item UPA-panel grouping (everyone interviewed together)
#' }
#'
#' ## Cross-Quarter Aggregation (Important!)
#'
#' **For optimal determination rates (~97%), input data should be stacked across
#' multiple quarter datasets** (ideally 4+ years). The algorithm leverages PNADC's rotating
#' panel design where the locations defined by the same values in variables 
#' UPA-V1014 are always interviewed in the same relative month within a given quarter
#' across all quarterly visits.
#'
#' \itemize{
#'   \item **Per-quarter processing**: ~65-75% determination rate
#'   \item **Multi-quarter stacked**: ~97% determination rate
#' }
#'
#' The cross-quarter aggregation dramatically improves accuracy by combining
#' birthday constraints from multiple interview rounds.
#'
#' ## Monthly Weight Computation
#'
#' When \code{compute_weights = TRUE}, the function:
#' \enumerate{
#'   \item Fetches monthly population from IBGE SIDRA API (table 6022)
#'   \item Redistributes quarterly weights to months using hierarchical rake weighting
#'   \item Smooths monthly aggregates to remove quarterly artifacts
#' }
#'
#' The resulting \code{weight_monthly} is the adjusted weight for general-purpose
#' monthly analysis.
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
#' # Basic usage: identify reference months only
#' library(mensalizePNADC)
#'
#' # Load stacked quarterly data (minimum columns)
#' pnadc <- data.table::fread("pnadc_stacked.csv",
#'   select = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
#'              "V2008", "V20081", "V20082", "V2009"))
#'
#' # Get crosswalk
#' crosswalk <- mensalizePNADC(pnadc)
#'
#' # Join with original quarterly file
#' original <- haven::read_dta("PNADC_2023T1.dta")
#' monthly <- dplyr::left_join(original, crosswalk,
#'   by = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003"))
#'
#'
#' # With monthly weights for general analysis
#' # (requires sidrar package and internet connection)
#' pnadc_full <- haven::read_dta("PNADCtrimestralempilhada.dta")
#'
#' result <- mensalizePNADC(pnadc_full, compute_weights = TRUE)
#'
#' # Use weight_monthly for any monthly aggregate
#' result[, .(pop = sum(weight_monthly)), by = ref_month_yyyymm]
#' }
#'
#' @seealso
#' \code{\link{identify_reference_month}} for just reference month identification
#'
#' @export
mensalizePNADC <- function(data,
                            compute_weights = FALSE,
                            keep_all = TRUE,
                            output = c("crosswalk", "microdata", "aggregates"),
                            verbose = TRUE) {

  # Validate arguments
  output <- match.arg(output)
  checkmate::assert_logical(compute_weights, len = 1)
  checkmate::assert_logical(keep_all, len = 1)
  checkmate::assert_logical(verbose, len = 1)

  # Validate input data columns FIRST (fail fast)
  # Check for weight-related columns only if compute_weights = TRUE
  validate_pnadc(data, check_weights = compute_weights, stop_on_error = TRUE)

  # Determine number of steps for progress bar
  # Basic: 8 steps (identify_reference_month internal steps)
  # With weights: 8 + 3 = 11 steps (add SIDRA fetch, calibrate, smooth)
  n_steps <- if (compute_weights) 11L else 8L

  # Initialize progress bar
  if (verbose) {
    cat("Processing PNADC data...\n")
    pb <- txtProgressBar(min = 0, max = n_steps, style = 3)
  } else {
    pb <- NULL
  }

  # Step 1-8: Identify reference months
  # Pass our progress bar so it shows granular progress
  crosswalk <- identify_reference_month(data, verbose = FALSE, .pb = pb, .pb_offset = 0L)
  gc()  # OPTIMIZATION: Free temporary objects from ref month identification

  det_rate <- attr(crosswalk, "determination_rate")

  if (!compute_weights) {
    if (verbose) {
      close(pb)
      cat(sprintf("  Determination rate: %.1f%%\n", det_rate * 100))
    }

    if (output == "crosswalk") {
      return(crosswalk)
    } else if (output == "microdata") {
      # OPTIMIZATION: Use data.table join instead of merge() for speed
      dt <- ensure_data_table(data, copy = TRUE)
      key_cols <- intersect(join_key_vars(), names(dt))
      data.table::setkeyv(crosswalk, key_cols)
      data.table::setkeyv(dt, key_cols)
      dt[crosswalk, on = key_cols,
         `:=`(ref_month = i.ref_month,
              ref_month_in_quarter = i.ref_month_in_quarter,
              ref_month_yyyymm = i.ref_month_yyyymm)]
      return(dt)
    } else {
      stop("output = 'aggregates' requires compute_weights = TRUE")
    }
  }

  # Steps 9-11: Weight computation pipeline (without Bayesian adjustment)

  # Step 9: Fetch monthly population from SIDRA
  monthly_totals <- fetch_monthly_population(verbose = FALSE)

  if (verbose) setTxtProgressBar(pb, 9)

  # Step 10: Calibrate weights using rake weighting
  dt <- ensure_data_table(data, copy = TRUE)

  # Ensure consistent types for join keys (PNADC data often has character columns)
  if (is.character(dt$Ano)) dt[, Ano := as.integer(Ano)]
  if (is.character(dt$Trimestre)) dt[, Trimestre := as.integer(Trimestre)]

  key_cols <- intersect(join_key_vars(), names(dt))
  # OPTIMIZATION: Use data.table join instead of merge() for speed
  crosswalk_cols <- c("ref_month", "ref_month_in_quarter", "ref_month_yyyymm")
  data.table::setkeyv(crosswalk, key_cols)
  data.table::setkeyv(dt, key_cols)
  dt[crosswalk, on = key_cols,
     `:=`(ref_month = i.ref_month,
          ref_month_in_quarter = i.ref_month_in_quarter,
          ref_month_yyyymm = i.ref_month_yyyymm)]

  # Calibrate weights using hierarchical rake weighting
  dt <- calibrate_monthly_weights(dt, monthly_totals, keep_all = keep_all, verbose = FALSE)
  gc()  # OPTIMIZATION: Free temporary objects from weight calibration

  if (verbose) setTxtProgressBar(pb, 10)

  # Step 11: Smooth monthly aggregates
  # The smoothing step adjusts weights so monthly series don't show artificial quarterly patterns
  dt <- smooth_calibrated_weights(dt)

  # Rename to final output name
  if ("weight_smoothed" %in% names(dt)) {
    data.table::setnames(dt, "weight_smoothed", "weight_monthly")
  } else if ("weight_calibrated" %in% names(dt)) {
    data.table::setnames(dt, "weight_calibrated", "weight_monthly")
  }

  if (verbose) {
    setTxtProgressBar(pb, 11)
    close(pb)
    cat(sprintf("  Determination rate: %.1f%%\n", det_rate * 100))
    n_with_weights <- sum(!is.na(dt$weight_monthly))
    cat(sprintf("  Observations with monthly weights: %s\n",
                format(n_with_weights, big.mark = ",")))
  }

  # Return based on output type
  if (output == "crosswalk") {
    # Minimal crosswalk with weights
    output_cols <- c(key_cols, "ref_month", "ref_month_in_quarter",
                     "ref_month_yyyymm", "weight_monthly")
    output_cols <- intersect(output_cols, names(dt))
    result <- dt[, ..output_cols]
    attr(result, "determination_rate") <- det_rate
    return(result)
  } else if (output == "microdata") {
    return(dt)
  } else {
    # Return monthly aggregates
    aggregates <- compute_monthly_aggregates(dt)
    return(aggregates)
  }
}

#' Smooth Calibrated Weights
#'
#' Apply smoothing to calibrated weights to remove artificial quarterly patterns.
#'
#' @param dt data.table with weight_calibrated column
#' @return data.table with weight_smoothed column added
#' @keywords internal
#' @noRd
smooth_calibrated_weights <- function(dt) {
  # Aggregate to monthly totals
  # Use z_ prefix so smooth_monthly_aggregates() processes it
  monthly_totals <- dt[!is.na(ref_month_in_quarter), .(
    z_populacao = sum(weight_calibrated, na.rm = TRUE)
  ), by = ref_month_yyyymm]

  # Apply smoothing to remove quarterly artifacts
  # This will produce m_populacao from z_populacao
  smoothed <- smooth_monthly_aggregates(monthly_totals)

  if ("m_populacao" %in% names(smoothed)) {
    # OPTIMIZATION: Use data.table join instead of merge() for speed
    dt[smoothed, on = .(ref_month_yyyymm), m_populacao := i.m_populacao]

    # Compute monthly weight totals for calibration
    dt[, pop_month := sum(weight_calibrated, na.rm = TRUE), by = ref_month_yyyymm]

    # Scale individual weights to match smoothed monthly totals
    # m_populacao is in thousands
    dt[!is.na(m_populacao) & pop_month > 0,
       weight_smoothed := weight_calibrated * (m_populacao * 1000 / pop_month)]

    # For months without smoothed values, keep calibrated weight
    dt[is.na(weight_smoothed), weight_smoothed := weight_calibrated]

    # Clean up
    dt[, c("m_populacao", "pop_month") := NULL]
  } else {
    # If smoothing failed, just use calibrated weights
    dt[, weight_smoothed := weight_calibrated]
  }

  dt
}

#' Compute Monthly Aggregates
#'
#' Compute monthly aggregated indicators from weighted microdata.
#'
#' @param dt data.table with weight_monthly column
#' @return data.table with monthly aggregates
#' @keywords internal
#' @noRd
compute_monthly_aggregates <- function(dt) {
  dt[!is.na(ref_month_in_quarter), .(
    n_obs = .N,
    population = sum(weight_monthly, na.rm = TRUE)
  ), by = ref_month_yyyymm]
}
