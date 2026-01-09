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
#'
#' @examples
#' # Load the data
#' data(vignette_series)
#'
#' # Check the quarterly unemployment series
#' head(vignette_quarterly_total)
#'
#' # Check metadata
#' vignette_metadata$determination_rate
#'
#' @name vignette_series
#' @aliases vignette_quarterly_total vignette_quarterly_gender vignette_quarterly_race
#'   vignette_monthly_total vignette_monthly_gender vignette_monthly_race vignette_metadata
#' @docType data
NULL
