# mensalizePNADC

<!-- badges: start -->
[![R-CMD-check](https://github.com/antrologos/mensalizePNADC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/antrologos/mensalizePNADC/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/antrologos/mensalizePNADC/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/antrologos/mensalizePNADC/actions/workflows/pkgdown.yaml)
[![codecov](https://codecov.io/gh/antrologos/mensalizePNADC/branch/master/graph/badge.svg)](https://codecov.io/gh/antrologos/mensalizePNADC)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

Convert Brazil's quarterly PNADC survey data into monthly time series.

## Overview

The `mensalizePNADC` package identifies reference months in Brazil's quarterly PNADC (Pesquisa Nacional por Amostra de Domicilios Continua) microdata and optionally computes monthly survey weights.

**Why mensalization?** PNADC quarterly statistics are actually moving averages of three months. Mensalization recovers the specific month each observation refers to, enabling true monthly labor market analysis.

- **97% determination rate** on 28.4M observations (2012-2025)
- **Identical results** to Stata implementation by Marcos Hecksher
- **Fast**: ~1 minute for 28M rows

## Installation

```r
# Install from GitHub
devtools::install_github("antrologos/mensalizePNADC")
```

## Quick Example

```r
library(mensalizePNADC)
library(data.table)

# Load stacked PNADC data
pnadc <- fread("pnadc_stacked.csv")

# Identify reference months
result <- mensalizePNADC(pnadc)
# Determination rate: 97.0%

# Use in analysis
result[, .(n = .N), by = ref_month_yyyymm]
```

For complete examples and required input columns, see the [Get Started guide](https://antrologos.github.io/mensalizePNADC/articles/getting-started.html).

## Documentation

| Guide | Description |
|-------|-------------|
| [Get Started](https://antrologos.github.io/mensalizePNADC/articles/getting-started.html) | Installation and first steps |
| [Download Data](https://antrologos.github.io/mensalizePNADC/articles/download-and-prepare.html) | Prepare PNADC microdata |
| [Applied Examples](https://antrologos.github.io/mensalizePNADC/articles/applied-examples.html) | COVID, recession, minimum wage validation |
| [How It Works](https://antrologos.github.io/mensalizePNADC/articles/how-it-works.html) | Algorithm details |
| [Reference](https://antrologos.github.io/mensalizePNADC/reference/index.html) | Function documentation |

## Authors

- **Marcos Hecksher** - Original methodology and Stata implementation
- **Rogerio Barbosa** - R package maintainer

## References

- [IBGE PNADC Documentation](https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html)
- Hecksher, M. (2024). Mensalizacao da PNADC. Working paper.

## License

MIT
