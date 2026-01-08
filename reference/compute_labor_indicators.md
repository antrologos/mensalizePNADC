# Compute Labor Force Indicators

Computes employment rates, unemployment rates, and other labor market
indicators using survey weights. Can compute indicators by any grouping
variable (typically month).

## Usage

``` r
compute_labor_indicators(
  data,
  weight_var = "weight_monthly",
  by = "ref_month_yyyymm",
  indicators = "all"
)
```

## Arguments

- data:

  A data.frame or data.table with PNADC microdata and weights. Required
  columns depend on which indicators are requested.

- weight_var:

  Character. Name of the weight variable to use. Default is
  "weight_monthly".

- by:

  Character vector of grouping variables. Default is "ref_month_yyyymm"
  for monthly indicators.

- indicators:

  Character vector of indicators to compute. Options:

  - "population": Total population, age 14+, labor force, etc.

  - "employment": Employment by type (formal, informal, etc.)

  - "sector": Employment by economic sector

  - "rates": Unemployment rate, participation rate, etc

  - "income": Income aggregates and averages

  - "all": All indicators (default)

## Value

A data.table with requested indicators aggregated by the grouping
variables.

## Details

Calculates standard PNADC labor force indicators from weighted
microdata.

Population indicators:

- `pop_total`: Total population

- `pop_14plus`: Population age 14+

- `pop_employed`: Employed persons

- `pop_unemployed`: Unemployed persons

- `pop_labor_force`: Labor force (employed + unemployed)

- `pop_out_of_lf`: Out of labor force

Rate indicators:

- `rate_participation`: Labor force / pop 14+ (%)

- `rate_employment`: Employed / pop 14+ (%)

- `rate_unemployment`: Unemployed / labor force (%)

## Examples

``` r
if (FALSE) { # \dontrun{
# Monthly indicators
indicators <- compute_labor_indicators(weighted_data)

# Indicators by state
by_state <- compute_labor_indicators(weighted_data, by = c("ref_month_yyyymm", "UF"))
} # }
```
