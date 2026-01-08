# Smooth Monthly Aggregates

When quarterly survey data is converted to monthly estimates, the
resulting time series often shows artificial quarterly patterns. This
function applies a smoothing algorithm that:

1.  Computes 3-month differences to capture trends

2.  Stratifies by month position within quarter (1st, 2nd, 3rd)

3.  Accumulates changes via cumulative sums

4.  Calibrates against a reference period

5.  Produces smooth monthly series that preserve genuine variation

## Usage

``` r
smooth_monthly_aggregates(
  monthly_aggregates,
  calibration_start = 201301L,
  calibration_end = 201912L
)
```

## Arguments

- monthly_aggregates:

  A data.table with monthly aggregated indicators. Required columns:

  - `ref_month_yyyymm`: Month identifier in YYYYMM format

  - `z_*`: Indicator columns with monthly totals (e.g., z_popocup)

- calibration_start:

  Integer YYYYMM. Start of calibration period. Default 201301.

- calibration_end:

  Integer YYYYMM. End of calibration period. Default 201912.

## Value

A data.table with smoothed monthly estimates. For each input variable
`z_varname`, the output includes:

- `m_varname`: Smoothed monthly estimate

## Details

Applies moving average methodology to remove quarterly artifacts from
monthly estimates derived from quarterly survey data.

The smoothing algorithm works as follows:

For each indicator variable:

1.  Compute 3-month difference: `d3 = 3 * (value[t] - value[t-1])`

2.  Split by month position (1, 2, 3 within quarter)

3.  Compute cumulative sum within each position

4.  Calculate residual from intermediate series during calibration
    period

5.  Apply position-specific adjustment

6.  Final smoothed value uses moving average of quarterly reference

## See also

[`calibrate_monthly_weights`](https://antrologos.github.io/mensalizePNADC/reference/calibrate_monthly_weights.md),
[`adjust_weights_bayesian`](https://antrologos.github.io/mensalizePNADC/reference/calibrate_to_sidra.md)
