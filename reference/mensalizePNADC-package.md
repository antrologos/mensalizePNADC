# mensalizePNADC: Convert Quarterly PNADC Survey Data to Monthly Time Series

The mensalizePNADC package provides tools to convert Brazil's quarterly
PNADC (Pesquisa Nacional por Amostra de Domicilios Continua) survey data
into monthly time series.

The package offers two main capabilities:

1.  **Reference month identification**: Determines which month within
    each quarter each survey observation refers to, using IBGE's "Parada
    Tecnica" rules and respondent birthdates

2.  **Monthly weight computation**: Adjusts survey weights for monthly
    (instead of quarterly) estimates using hierarchical calibration and
    Bayesian methods

## Main Functions

- [`mensalizePNADC`](https://antrologos.github.io/mensalizePNADC/reference/mensalizePNADC.md):

  Main function that processes stacked quarterly PNADC data and returns
  a crosswalk for joining with original data

- [`identify_reference_month`](https://antrologos.github.io/mensalizePNADC/reference/identify_reference_month.md):

  Identifies which month each observation refers to (the core algorithm)

- [`calibrate_monthly_weights`](https://antrologos.github.io/mensalizePNADC/reference/calibrate_monthly_weights.md):

  Redistributes quarterly weights to monthly using hierarchical
  calibration

- [`smooth_monthly_aggregates`](https://antrologos.github.io/mensalizePNADC/reference/smooth_monthly_aggregates.md):

  Removes quarterly artifacts from monthly series

- [`adjust_weights_bayesian`](https://antrologos.github.io/mensalizePNADC/reference/calibrate_to_sidra.md):

  Final Bayesian weight adjustment

- [`compute_labor_indicators`](https://antrologos.github.io/mensalizePNADC/reference/compute_labor_indicators.md):

  Computes standard labor force indicators from weighted data

## Utility Functions

- [`validate_pnadc`](https://antrologos.github.io/mensalizePNADC/reference/validate_pnadc.md):

  Validates input data has required columns

- [`get_exception_quarters`](https://antrologos.github.io/mensalizePNADC/reference/get_exception_quarters.md):

  Returns quarters with non-standard IBGE timing rules

## Background

PNADC is a quarterly household survey conducted by IBGE (Brazilian
Institute of Geography and Statistics) that provides labor market and
demographic information. While the survey is published quarterly, each
interview actually refers to a specific week within the quarter.

This package implements the methodology developed by Marcos Hecksher to
identify reference months and compute monthly-appropriate survey
weights, enabling monthly time series analysis of PNADC data.

## Methodology

The reference month identification algorithm uses:

- IBGE's reference week timing rules ("Parada Tecnica")

- Respondent birthdates to constrain possible interview dates

- UPA-panel level aggregation (everyone interviewed together)

The monthly weight computation uses:

- Hierarchical calibration across demographic/geographic cells

- Moving average smoothing to remove quarterly artifacts

- Bayesian posterior adjustment to match employment targets

## References

IBGE Manual Basico da Entrevista PNADC (methodology on "Parada Tecnica")

## See also

Useful links:

- <https://github.com/PLACEHOLDER/mensalizePNADC>

- Report bugs at <https://github.com/PLACEHOLDER/mensalizePNADC/issues>

## Author

Marcos Hecksher <mdhecksher@gmail.com>
