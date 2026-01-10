#' Pre-computed Labor Market Series for Vignette Examples
#'
#' These datasets contain pre-computed quarterly and monthly labor market
#' indicators from PNADC data (2012-Q1 to 2025-Q3). They are used in the
#' "Applied Examples" vignette to demonstrate the value of mensalization.
#'
#' @format Data tables with labor market indicators:
#'
#' **vignette_quarterly_total** and **vignette_monthly_total**:
#' \describe{
#'   \item{period}{Date of observation}
#'   \item{unemployment_rate}{Unemployment rate}
#'   \item{participation_rate}{Labor force participation rate}
#'   \item{formalization_rate}{Share of formal employment}
#'   \item{employment_level}{Employment-to-population ratio}
#'   \item{frequency}{Either "Quarterly" or "Monthly"}
#' }
#'
#' **vignette_quarterly_gender** and **vignette_monthly_gender**:
#' \describe{
#'   \item{period}{Date of observation}
#'   \item{sexo}{"Men" or "Women"}
#'   \item{participation_rate}{Labor force participation rate}
#'   \item{unemployment_rate}{Unemployment rate}
#'   \item{frequency}{Either "Quarterly" or "Monthly"}
#' }
#'
#' **vignette_quarterly_race** and **vignette_monthly_race**:
#' \describe{
#'   \item{period}{Date of observation}
#'   \item{raca}{"White", "Black", or "Brown"}
#'   \item{employment_level}{Employment-to-population ratio}
#'   \item{unemployment_rate}{Unemployment rate}
#'   \item{frequency}{Either "Quarterly" or "Monthly"}
#' }
#'
#' **vignette_monthly_mw**: Minimum wage analysis series
#' \describe{
#'   \item{period}{Date of observation}
#'   \item{ref_month_yyyymm}{Reference month in YYYYMM format}
#'   \item{n_workers}{Number of formal private sector workers (weighted)}
#'   \item{pct_at_mw_habitual}{Share earning within +/-5% of MW (habitual income)}
#'   \item{pct_at_mw_effective}{Share earning within +/-5% of MW (effective income)}
#'   \item{mw_value}{Minimum wage value in R$ for that month}
#'   \item{mw_change}{Logical: TRUE if MW changed from previous month}
#'   \item{is_adjustment_month}{Logical: TRUE if first month after MW adjustment}
#' }
#'
#' **vignette_mw_history**: Historical minimum wage values
#' \describe{
#'   \item{start_date}{Date when new MW value took effect}
#'   \item{mw_value}{Minimum wage value in R$}
#' }
#'
#' **vignette_metadata**: List with:
#' \describe{
#'   \item{determination_rate}{Proportion of observations assigned to months}
#'   \item{n_observations}{Total number of observations processed}
#'   \item{period_start}{First quarter in data}
#'   \item{period_end}{Last quarter in data}
#'   \item{generated_date}{Date when data was generated}
#' }
#'
#' @source Computed from PNADC microdata (IBGE) using mensalizePNADC.
#' Minimum wage values from IPEADATA (series 1739471028).
#'
#' @examples
#' # Load the data
#' data(vignette_series)
#'
#' # Check the quarterly unemployment series
#' head(vignette_quarterly_total)
#'
#' # Check minimum wage analysis data
#' head(vignette_monthly_mw)
#'
#' # Check metadata
#' vignette_metadata$determination_rate
#'
#' @name vignette_series
#' @aliases vignette_quarterly_total vignette_quarterly_gender vignette_quarterly_race
#'   vignette_monthly_total vignette_monthly_gender vignette_monthly_race
#'   vignette_monthly_mw vignette_mw_history vignette_metadata
#' @docType data
NULL
