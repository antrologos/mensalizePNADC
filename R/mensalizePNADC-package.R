#' mensalizePNADC: Convert Quarterly PNADC Survey Data to Monthly Time Series
#'
#' The mensalizePNADC package provides tools to identify the exact month of
#' Brazil's quarterly official household survey, PNADC (Pesquisa Nacional por
#' Amostra de Domicilios Continua - IBGE), allowing for analyzing the survey
#' data as a monthly time series.
#'
#' The package offers two main capabilities:
#' \enumerate{
#'   \item \strong{Reference month identification}: Determines which month within
#'     each quarter each survey observation refers to, using IBGE's "Parada Tecnica"
#'     rules and respondent birthdates
#'   \item \strong{Monthly weight computation}: Adjusts survey weights for monthly
#'     (instead of quarterly) estimates using hierarchical rake weighting
#' }
#'
#' The package is highly optimized for large datasets: approximately 1 minute to
#' process 28.4 million rows (450,000 rows/sec), with 97.0 percent determination
#' rate. Uses pre-computed lookup tables for 20x faster date creation.
#'
#' The main function is \code{\link{mensalizePNADC}}, which processes stacked
#' quarterly PNADC data and returns a crosswalk for joining with original data.
#'
#' @references
#' IBGE Manual Basico da Entrevista PNADC (methodology on "Parada Tecnica")
#'
#' @author Marcos Hecksher \email{mdhecksher@@gmail.com}
#' @author Rogerio Barbosa \email{rogerio.barbosa@@iesp.uerj.br}
#'
#' @import data.table
#' @importFrom checkmate assert_data_frame assert_int assert_logical assert_string assert_character
#' @importFrom stats median
#' @importFrom utils head setTxtProgressBar txtProgressBar
#' @keywords internal
"_PACKAGE"

# Prevent R CMD check notes about data.table syntax
utils::globalVariables(c(
  # PNADC variables
  "Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
  "V2008", "V20081", "V20082", "V2009",
  "V1028", "UF", "posest", "posest_sxi",
  # Computed variables - reference month identification
  "ref_month", "ref_month_in_quarter", "ref_month_yyyymm",
  "birthday", "first_sat_after_birthday",
  "visit_before_birthday", "month1", "month2", "month3",
  "first_sat_m1", "first_sat_m2", "first_sat_m3",
  "alt_sat_m1", "alt_sat_m2", "alt_sat_m3",
  "date_min", "date_max", "month_min_pos", "month_max_pos",
  "alt_date_min", "alt_date_max", "alt_month_min_pos", "alt_month_max_pos",
  "upa_month_min", "upa_month_max",
  "alt_upa_month_min", "alt_upa_month_max",
  "upa_month_min_final", "upa_month_max_final",
  "requires_exception", "requires_exc_m1", "requires_exc_m2", "requires_exc_m3",
  "trim_exc_m1", "trim_exc_m2", "trim_exc_m3",
  # Computed variables - weight calibration
  "celula1", "celula2", "celula3", "celula4",
  "weight_current", "weight_calibrated", "weight_monthly",
  "pop_quarter", "pop_month", "n_cells_quarter", "n_cells_month",
  "m_populacao", "z_populacao",
  # Computed variables - SIDRA population fetch
  "Valor", "anomesexato", "anomesfinaltrimmovel", "populacao",
  # Computed variables - smooth aggregates
  "month_pos", "weight_smoothed", "pop_current",
  "row_num", "row_num2", "d_pop", "quarter_yyyyq",
  # data.table special symbols and output column reference
  ".SD", ".N", ".I", "..output_cols", "."
))
