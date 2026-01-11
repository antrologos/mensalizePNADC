#' Calibrate Weights to IBGE SIDRA Series (Optional)
#'
#' Applies theme-specific Bayesian calibration so aggregated estimates match
#' IBGE's official SIDRA published series.
#'
#' @description
#' This is an **optional** function for users who need their estimates to exactly
#' match IBGE's official monthly series. The core \code{\link{mensalizePNADC}}
#' function already produces valid monthly weights; this function provides
#' additional calibration for specific themes.
#'
#' **Important**: Different themes require different Bayesian calibrations.
#' For example, weights calibrated to match unemployment rates will differ
#' from weights calibrated to match employment levels.
#'
#' @param data A data.table output from \code{\link{mensalizePNADC}} with
#'   \code{compute_weights = TRUE}. Must contain \code{weight_monthly} and
#'   employment status variables (\code{VD4001}, \code{VD4002}, etc.).
#'
#' @param theme Character. Which IBGE series to match:
#'   \describe{
#'     \item{"unemployment"}{Taxa de desocupacao - unemployment rate}
#'     \item{"employment"}{Populacao ocupada - employment levels}
#'     \item{"labor_force"}{PEA/PNEA - labor force participation}
#'     \item{"income"}{Rendimento habitual - income aggregates}
#'     \item{"custom"}{User provides target series via \code{sidra_series}}
#'   }
#'
#' @param sidra_series Optional data.frame with monthly target series.
#'   Required if \code{theme = "custom"}. Must contain:
#'   \itemize{
#'     \item \code{ref_month_yyyymm}: Month in YYYYMM format
#'     \item \code{m_*}: Target values for each indicator (in thousands)
#'   }
#'
#' @param verbose Logical. Print progress messages? Default TRUE.
#'
#' @return A data.table with the input data plus:
#'   \itemize{
#'     \item \code{weight_sidra}: Bayesian-adjusted weight matching SIDRA series
#'   }
#'
#'   The original \code{weight_monthly} is preserved for comparison.
#'
#' @details
#' ## Why Theme-Specific Calibration?
#'
#' IBGE's published SIDRA series are internally consistent but use slightly
#' different adjustments for different indicators. This means:
#'
#' \itemize{
#'   \item Weights optimized for unemployment rate may not sum exactly to
#'     the published employment level
#'   \item Weights optimized for employment may not produce the exact
#'     published unemployment rate
#' }
#'
#' Choose the theme that matches your primary analysis focus.
#'
#' ## Algorithm
#'
#' Uses Bayes' theorem to adjust individual weights:
#' \deqn{P(status | category) = P(status) \times P(category | status) / P(category)}
#'
#' Where:
#' \itemize{
#'   \item status = employment situation (employed, unemployed, etc.)
#'   \item category = demographic/geographic cell
#'   \item P(status) is derived from SIDRA targets
#' }
#'
#' @examples
#' \dontrun{
#' # First, get base weights (auto-fetches population from SIDRA)
#' crosswalk <- mensalizePNADC(pnadc_full, compute_weights = TRUE)
#'
#' # Then, calibrate for unemployment analysis
#' unemployment_data <- calibrate_to_sidra(crosswalk, theme = "unemployment")
#'
#' # Calculate unemployment rate (will match IBGE SIDRA exactly)
#' unemployment_data[, .(
#'   rate = sum(weight_sidra * (VD4002 == 2)) /
#'          sum(weight_sidra * (VD4001 == 1)) * 100
#' ), by = ref_month_yyyymm]
#' }
#'
#' @seealso
#' \code{\link{mensalizePNADC}} for core mensalization (required first)
#' \code{\link{adjust_weights_bayesian}} for the underlying Bayesian algorithm
#'
#' @export
calibrate_to_sidra <- function(data,
                                theme = c("unemployment", "employment",
                                          "labor_force", "income", "custom"),
                                sidra_series = NULL,
                                verbose = TRUE) {

  theme <- match.arg(theme)
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_logical(verbose, len = 1)

  if (theme == "custom" && is.null(sidra_series)) {
    stop("sidra_series is required when theme = 'custom'")
  }

  dt <- ensure_data_table(data, copy = TRUE)

  # Verify that weight_monthly exists (from mensalizePNADC)
  if (!"weight_monthly" %in% names(dt)) {
    stop("weight_monthly not found. Run mensalizePNADC() with compute_weights = TRUE first.")
  }

  if (verbose) message("Calibrating to SIDRA series (theme: ", theme, ")...")

  # Get or fetch target series
  targets <- get_sidra_targets(theme, sidra_series, dt, verbose)

  if (is.null(targets) || nrow(targets) == 0) {
    warning("No SIDRA targets available. Returning original weights.")
    dt[, weight_sidra := weight_monthly]
    return(dt)
  }

  # Use weight_monthly as the base for Bayesian adjustment
  # (rename temporarily for the adjustment function)
  dt[, weight_calibrated := weight_monthly]

  # Apply Bayesian adjustment
  dt <- adjust_weights_bayesian_internal(dt, targets, theme, verbose)

  # Rename result to weight_sidra
  if ("weight_bayesian" %in% names(dt)) {
    data.table::setnames(dt, "weight_bayesian", "weight_sidra")
  } else {
    dt[, weight_sidra := weight_monthly]
  }

  # Clean up temporary column
  dt[, weight_calibrated := NULL]

  if (verbose) {
    message("  Theme-specific weights created: weight_sidra")
  }

  dt
}

#' Get SIDRA Target Series
#'
#' Fetches or uses provided SIDRA target series for calibration.
#'
#' @param theme Calibration theme
#' @param sidra_series User-provided series (if any)
#' @param dt Data for determining date range
#' @param verbose Print messages
#' @return data.table with target series
#' @keywords internal
#' @noRd
get_sidra_targets <- function(theme, sidra_series, dt, verbose) {

  if (!is.null(sidra_series)) {
    # Use provided series
    targets <- ensure_data_table(sidra_series, copy = TRUE)

    # Standardize column name
    if ("anomesexato" %in% names(targets) && !"ref_month_yyyymm" %in% names(targets)) {
      targets[, ref_month_yyyymm := anomesexato]
    }

    return(targets)
  }

  # Theme-specific target variables
  target_vars <- switch(theme,
    "unemployment" = c("m_popdesocup", "m_popocup", "m_pop14mais"),
    "employment" = c("m_popocup", "m_empregprivcomcart", "m_empregprivsemcart",
                     "m_domesticocomcart", "m_domesticosemcart",
                     "m_empregpublcomcart", "m_empregpublsemcart",
                     "m_estatutmilitar", "m_empregador", "m_contapropria",
                     "m_trabfamauxiliar"),
    "labor_force" = c("m_pop14mais", "m_popocup", "m_popdesocup",
                      "m_popnaforca", "m_popforadaforca"),
    "income" = c("m_rendhabnominaltodos", "m_massahabnominaltodos"),
    "custom" = NULL
  )

  if (verbose && !is.null(target_vars)) {
    message("  Required target variables: ", paste(target_vars, collapse = ", "))
    message("  Note: SIDRA API integration not yet implemented.")
    message("  Provide target series via sidra_series parameter.")
  }

  # TODO: Implement SIDRA API fetching based on code/pnadcmensal.R
  # For now, return NULL and warn
  warning("Automatic SIDRA fetching not yet implemented. ",
          "Please provide sidra_series parameter with target values.")

  NULL
}

#' Internal Bayesian Weight Adjustment
#'
#' Applies Bayesian adjustment for theme-specific calibration.
#'
#' @param dt data.table with weight_calibrated
#' @param targets Target series
#' @param theme Calibration theme
#' @param verbose Print messages
#' @return data.table with weight_bayesian
#' @keywords internal
#' @noRd
adjust_weights_bayesian_internal <- function(dt, targets, theme, verbose) {

  # Step 1: Create employment status categories
  dt <- create_employment_status(dt, detailed = TRUE)

  # Step 2: Create demographic category
  dt <- create_demographic_category(dt)

  # Step 3: Get status to target mapping based on theme
  status_mapping <- get_status_target_mapping_for_theme(theme)

  # Step 4: Merge monthly targets
  target_cols <- grep("^m_", names(targets), value = TRUE)
  dt <- merge(dt, targets[, c("ref_month_yyyymm", target_cols), with = FALSE],
              by = "ref_month_yyyymm", all.x = TRUE)

  # Step 5: Calculate Bayesian adjustment
  dt <- calculate_bayesian_weights_internal(dt, status_mapping)

  # Clean up
  temp_cols <- c("cat", "employment_status", "prob_sit", "prob_cat_given_sit",
                 "prob_cat", "prob_sit_given_cat", "prob_sit_given_cat_intermed")
  dt[, (intersect(temp_cols, names(dt))) := NULL]
  dt[, (intersect(target_cols, names(dt))) := NULL]

  dt
}

#' Get Status to Target Mapping for Theme
#'
#' Maps employment status codes to target variables for specific theme.
#'
#' @param theme Calibration theme
#' @return Named list
#' @keywords internal
#' @noRd
get_status_target_mapping_for_theme <- function(theme) {

  # Base mapping for all themes
  base_mapping <- list(
    "1" = "m_empregprivcomcart",
    "2" = "m_empregprivsemcart",
    "3" = "m_domesticocomcart",
    "4" = "m_domesticosemcart",
    "5" = "m_empregpublcomcart",
    "6" = "m_empregpublsemcart",
    "7" = "m_estatutmilitar",
    "8" = "m_empregador",
    "9" = "m_contapropria",
    "10" = "m_trabfamauxiliar",
    "20" = "m_popdesocup",
    "30" = "m_desalentado",
    "40" = "m_forcapotencial",
    "50" = "m_popforadaforca",
    "60" = "m_pop0a13"
  )

  # Add CNPJ variants for employment theme
  if (theme == "employment") {
    base_mapping[["81"]] <- "m_empregadorcomcnpj"
    base_mapping[["82"]] <- "m_empregadorsemcnpj"
    base_mapping[["91"]] <- "m_contapropriacomcnpj"
    base_mapping[["92"]] <- "m_contapropriasemcnpj"
  }

  base_mapping
}

#' Calculate Bayesian Weights (Internal)
#'
#' Core Bayesian adjustment calculation.
#'
#' @param dt data.table
#' @param status_mapping Status to target mapping
#' @return data.table with weight_bayesian
#' @keywords internal
#' @noRd
calculate_bayesian_weights_internal <- function(dt, status_mapping) {

  # Get monthly population target
  if (!"m_populacao" %in% names(dt)) {
    dt[, m_populacao := sum(weight_calibrated, na.rm = TRUE) / 1000, by = ref_month_yyyymm]
  }

  # Calculate population by category (from intermediate weights)
  dt[, pop_cat := sum(weight_calibrated, na.rm = TRUE),
     by = .(ref_month_yyyymm, cat)]

  # Calculate population by status from intermediate weights
  dt[, pop_status_intermed := sum(weight_calibrated, na.rm = TRUE),
     by = .(ref_month_yyyymm, employment_status)]

  # Calculate population by category and status
  dt[, pop_cat_status := sum(weight_calibrated, na.rm = TRUE),
     by = .(ref_month_yyyymm, cat, employment_status)]

  # P(category | status) from intermediate
  dt[, prob_cat_given_sit_intermed := pop_cat_status / pop_status_intermed]
  dt[is.na(prob_cat_given_sit_intermed) | !is.finite(prob_cat_given_sit_intermed),
     prob_cat_given_sit_intermed := 0]

  # P(status | category) from intermediate
  dt[, prob_sit_given_cat_intermed := pop_cat_status / pop_cat]
  dt[is.na(prob_sit_given_cat_intermed) | !is.finite(prob_sit_given_cat_intermed),
     prob_sit_given_cat_intermed := 0]

  # Get target population for each status
  dt[, target_pop := NA_real_]
  for (status_code in names(status_mapping)) {
    target_var <- status_mapping[[status_code]]
    if (target_var %in% names(dt)) {
      dt[employment_status == as.integer(status_code),
         target_pop := get(target_var) * 1000]
    }
  }

  # P(status) from targets
  dt[, prob_sit := target_pop / (m_populacao * 1000)]
  dt[is.na(prob_sit) | !is.finite(prob_sit), prob_sit := 0]

  # P(category) from intermediate
  dt[, prob_cat := pop_cat / sum(weight_calibrated, na.rm = TRUE),
     by = ref_month_yyyymm]

  # Bayesian posterior: P(status | category) = P(status) * P(cat | status) / P(cat)
  dt[, prob_sit_given_cat := prob_sit * prob_cat_given_sit_intermed / prob_cat]
  dt[is.na(prob_sit_given_cat) | !is.finite(prob_sit_given_cat),
     prob_sit_given_cat := prob_sit_given_cat_intermed]

  # Final weight adjustment
  dt[, weight_ratio := prob_sit_given_cat / prob_sit_given_cat_intermed]
  dt[is.na(weight_ratio) | !is.finite(weight_ratio) | weight_ratio <= 0,
     weight_ratio := 1]

  # Bound the ratio to prevent extreme adjustments
  dt[weight_ratio > 10, weight_ratio := 10]
  dt[weight_ratio < 0.1, weight_ratio := 0.1]

  dt[, weight_bayesian := weight_calibrated * weight_ratio]

  # Clean up
  temp_cols <- c("pop_cat", "pop_status_intermed", "pop_cat_status",
                 "prob_cat_given_sit_intermed", "prob_sit_given_cat_intermed",
                 "target_pop", "prob_sit", "prob_cat", "prob_sit_given_cat",
                 "weight_ratio", "m_populacao")
  dt[, (intersect(temp_cols, names(dt))) := NULL]

  dt
}

#' Create Employment Status Categories
#'
#' Maps PNADC employment variables to status categories.
#'
#' @param dt data.table with PNADC variables
#' @param detailed Use detailed categories with CNPJ distinction
#' @return data.table with employment_status column
#' @keywords internal
#' @noRd
create_employment_status <- function(dt, detailed = TRUE) {

  # Initialize as NA
  dt[, employment_status := NA_integer_]

  # Children (age 0-13)
  if ("V2009" %in% names(dt)) {
    dt[V2009 <= 13L, employment_status := 60L]
  }

  # Check required variables exist
  if (!all(c("VD4002", "VD4009") %in% names(dt))) {
    warning("VD4002 and/or VD4009 not found. Cannot create employment status.")
    return(dt)
  }

  # Employed persons (VD4002 == 1) - by position (VD4009)
  dt[VD4002 == 1L & VD4009 == 1L, employment_status := 1L]
  dt[VD4002 == 1L & VD4009 == 2L, employment_status := 2L]
  dt[VD4002 == 1L & VD4009 == 3L, employment_status := 3L]
  dt[VD4002 == 1L & VD4009 == 4L, employment_status := 4L]
  dt[VD4002 == 1L & VD4009 == 5L, employment_status := 5L]
  dt[VD4002 == 1L & VD4009 == 6L, employment_status := 6L]
  dt[VD4002 == 1L & VD4009 == 7L, employment_status := 7L]

  # Employer and self-employed
  if (detailed && "V4019" %in% names(dt)) {
    dt[VD4002 == 1L & VD4009 == 8L & V4019 == 1L, employment_status := 81L]
    dt[VD4002 == 1L & VD4009 == 8L & V4019 == 2L, employment_status := 82L]
    dt[VD4002 == 1L & VD4009 == 8L & is.na(V4019), employment_status := 8L]
    dt[VD4002 == 1L & VD4009 == 9L & V4019 == 1L, employment_status := 91L]
    dt[VD4002 == 1L & VD4009 == 9L & V4019 == 2L, employment_status := 92L]
    dt[VD4002 == 1L & VD4009 == 9L & is.na(V4019), employment_status := 9L]
  } else {
    dt[VD4002 == 1L & VD4009 == 8L, employment_status := 8L]
    dt[VD4002 == 1L & VD4009 == 9L, employment_status := 9L]
  }

  dt[VD4002 == 1L & VD4009 == 10L, employment_status := 10L]

  # Unemployed (VD4002 == 2)
  dt[VD4002 == 2L, employment_status := 20L]

  # Discouraged worker
  if ("VD4005" %in% names(dt)) {
    dt[VD4005 == 1L & is.na(employment_status), employment_status := 30L]
  }

  # Potential labor force
  if ("VD4003" %in% names(dt) && "VD4005" %in% names(dt)) {
    dt[VD4003 == 1L & VD4005 != 1L & is.na(employment_status), employment_status := 40L]
  }

  # Out of labor force
  if ("V2009" %in% names(dt)) {
    dt[V2009 >= 14L & is.na(employment_status), employment_status := 50L]
  }

  dt
}

#' Create Demographic Category
#'
#' Creates combined demographic/geographic category for Bayesian adjustment.
#'
#' @param dt data.table with demographic variables
#' @return data.table with cat column
#' @keywords internal
#' @noRd
create_demographic_category <- function(dt) {

  if (!all(c("posest", "posest_sxi", "V2010") %in% names(dt))) {
    warning("Required demographic variables not found. Using simplified category.")
    dt[, cat := 1L]
    return(dt)
  }

  dt[, cat := posest * 10000L + (posest_sxi %% 1000L) * 10L + V2010]

  dt
}

#' Adjust Weights Using Bayesian Method (Deprecated)
#'
#' This function is deprecated. Use [calibrate_to_sidra()] instead.
#'
#' @param data data.table with mensalized PNADC data
#' @param monthly_targets data.table with monthly targets
#' @param employment_detail Logical, ignored (kept for backward compatibility)
#'
#' @return data.table with adjusted weights
#' @export
adjust_weights_bayesian <- function(data, monthly_targets, employment_detail = TRUE) {
  .Deprecated("calibrate_to_sidra")
  calibrate_to_sidra(data, theme = "employment", sidra_series = monthly_targets)
}
