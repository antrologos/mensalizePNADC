# Tests for pnadc_apply_periods() - apply crosswalk and optionally calibrate weights

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Create minimal PNADC data for apply tests
#' @param n_quarters Number of quarters to generate
#' @param n_upas Number of UPAs per quarter
#' @param persons_per_household Number of persons per household
create_test_pnadc_for_apply <- function(n_quarters = 4, n_upas = 10, persons_per_household = 3) {
  set.seed(42)

  quarters <- expand.grid(
    Ano = 2023L,
    Trimestre = 1:n_quarters
  )

  # Create UPA-V1014 combinations (rotating panel simulation)
  upa_panel <- data.table::CJ(
    UPA = 1:n_upas,
    V1014 = 1:8  # 8 panel groups
  )

  # Cross with quarters
  dt <- data.table::CJ(
    idx = 1:nrow(quarters),
    upa_idx = 1:nrow(upa_panel)
  )
  dt[, `:=`(
    Ano = quarters$Ano[idx],
    Trimestre = quarters$Trimestre[idx],
    UPA = upa_panel$UPA[upa_idx],
    V1014 = upa_panel$V1014[upa_idx]
  )]
  dt[, c("idx", "upa_idx") := NULL]

  # Add households per UPA
  dt <- dt[rep(1:.N, each = 2)]  # 2 households per UPA
  dt[, V1008 := rep(1:2, .N/2)]

  # Add persons per household
  dt <- dt[rep(1:.N, each = persons_per_household)]
  dt[, V2003 := rep(1:persons_per_household, .N/persons_per_household)]

  # Add birthday and age info
  n <- nrow(dt)
  dt[, `:=`(
    V2008 = sample(1:28, n, replace = TRUE),  # Birth day
    V20081 = sample(1:12, n, replace = TRUE), # Birth month
    V20082 = sample(1970:2000, n, replace = TRUE), # Birth year
    V2009 = sample(20:60, n, replace = TRUE)  # Age
  )]

  # Add weight variable
  dt[, V1028 := runif(n, 500, 2000)]

  # Add stratification variables for calibration
  dt[, `:=`(
    UF = sample(11:53, n, replace = TRUE),
    posest = sample(1:1000, n, replace = TRUE),
    posest_sxi = sample(10000:90000, n, replace = TRUE)
  )]

  dt
}

#' Create a mock crosswalk for testing
#' @param n_upas Number of UPAs
#' @param n_panels Number of panel groups (V1014)
#' @param n_quarters Number of quarters (to match test data)
#' @param n_households Number of households per UPA (V1008)
create_mock_crosswalk <- function(n_upas = 10, n_panels = 8, n_quarters = 1, n_households = 2) {
  # Create crosswalk at household-quarter level (Ano, Trimestre, UPA, V1008, V1014)
  # This matches the structure expected by pnadc_apply_periods
  dt <- data.table::CJ(
    Ano = 2023L,
    Trimestre = 1:n_quarters,
    UPA = 1:n_upas,
    V1008 = 1:n_households,
    V1014 = 1:n_panels
  )

  n <- nrow(dt)

  # Month info (high determination rate) - IBGE-based columns
  dt[, `:=`(
    ref_month_start = as.Date("2022-12-25") + ((.I - 1L) %% 3L) * 28L,  # Sunday (IBGE month start)
    ref_month_end = as.Date("2022-12-25") + ((.I - 1L) %% 3L) * 28L + 27L,  # Saturday (4 weeks)
    ref_month_in_quarter = ((.I - 1L) %% 3L) + 1L,
    ref_month_yyyymm = 202301L + ((.I - 1L) %% 3L),
    ref_month_weeks = 4L,
    determined_month = TRUE
  )]

  # Make some undetermined
  set.seed(123)  # For reproducibility
  undetermined_idx <- sample(1:n, size = ceiling(n * 0.1))
  dt[undetermined_idx, `:=`(
    ref_month_start = as.Date(NA),
    ref_month_end = as.Date(NA),
    ref_month_in_quarter = NA_integer_,
    ref_month_yyyymm = NA_integer_,
    ref_month_weeks = NA_integer_,
    determined_month = FALSE
  )]

  # Fortnight info (medium determination rate) - IBGE-based columns
  dt[, `:=`(
    ref_fortnight_start = as.Date("2022-12-25") + ((.I - 1L) %% 6L) * 14L,  # Sunday
    ref_fortnight_end = as.Date("2022-12-25") + ((.I - 1L) %% 6L) * 14L + 13L,  # Saturday (2 weeks)
    ref_fortnight_in_quarter = ((.I - 1L) %% 6L) + 1L,
    ref_fortnight_yyyyff = 202301L + ((.I - 1L) %% 6L),
    determined_fortnight = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.85, 0.15))
  )]
  dt[determined_fortnight == FALSE, `:=`(
    ref_fortnight_start = as.Date(NA),
    ref_fortnight_end = as.Date(NA),
    ref_fortnight_in_quarter = NA_integer_,
    ref_fortnight_yyyyff = NA_integer_
  )]

  # Week info (low determination rate) - IBGE-based columns
  # IBGE quarters always have exactly 12 reference weeks (4 weeks × 3 months)
  dt[, `:=`(
    ref_week_start = as.Date("2022-12-25") + ((.I - 1L) %% 12L) * 7L,  # Sunday
    ref_week_end = as.Date("2022-12-25") + ((.I - 1L) %% 12L) * 7L + 6L,  # Saturday
    ref_week_in_quarter = ((.I - 1L) %% 12L) + 1L,
    ref_week_yyyyww = 202301L + ((.I - 1L) %% 12L),
    determined_week = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.6, 0.4))
  )]
  dt[determined_week == FALSE, `:=`(
    ref_week_start = as.Date(NA),
    ref_week_end = as.Date(NA),
    ref_week_in_quarter = NA_integer_,
    ref_week_yyyyww = NA_integer_
  )]

  # Add determination_rates attribute
  attr(dt, "determination_rates") <- list(
    month = mean(dt$determined_month),
    fortnight = mean(dt$determined_fortnight),
    week = mean(dt$determined_week)
  )

  dt
}

#' Create monthly population totals for testing
create_monthly_totals <- function(year = 2023, quarters = 1:4) {
  months <- c()
  for (q in quarters) {
    months <- c(months, ((q - 1) * 3 + 1):((q - 1) * 3 + 3))
  }

  data.table::data.table(
    ref_month_yyyymm = year * 100L + months,
    m_populacao = 214000L + (months - 1L) * 100L  # In thousands
  )
}

# =============================================================================
# INPUT VALIDATION TESTS
# =============================================================================

test_that("pnadc_apply_periods requires weight_var argument", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 3)
  crosswalk <- create_mock_crosswalk(n_upas = 3)

  expect_error(
    pnadc_apply_periods(test_data, crosswalk, anchor = "quarter"),
    "weight_var"
  )
})

test_that("pnadc_apply_periods requires anchor argument", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 3)
  crosswalk <- create_mock_crosswalk(n_upas = 3)

  expect_error(
    pnadc_apply_periods(test_data, crosswalk, weight_var = "V1028"),
    "anchor"
  )
})

test_that("pnadc_apply_periods validates anchor values", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 3)
  crosswalk <- create_mock_crosswalk(n_upas = 3)

  expect_error(
    pnadc_apply_periods(test_data, crosswalk,
                        weight_var = "V1028",
                        anchor = "invalid"),
    "anchor"
  )
})

test_that("pnadc_apply_periods validates calibration_unit values", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 3)
  crosswalk <- create_mock_crosswalk(n_upas = 3)

  # match.arg error message format
  expect_error(
    pnadc_apply_periods(test_data, crosswalk,
                        weight_var = "V1028",
                        anchor = "quarter",
                        calibration_unit = "invalid"),
    "should be one of"
  )
})

test_that("pnadc_apply_periods validates weight_var exists", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 3)
  crosswalk <- create_mock_crosswalk(n_upas = 3)

  expect_error(
    pnadc_apply_periods(test_data, crosswalk,
                        weight_var = "NONEXISTENT",
                        anchor = "quarter",
                        calibrate = FALSE),
    "not found in data"
  )
})

test_that("pnadc_apply_periods validates crosswalk has required columns", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 3)
  # Crosswalk missing V1014 (required minimal key)
  bad_crosswalk <- data.frame(UPA = 1:3, something_else = 1:3)

  expect_error(
    pnadc_apply_periods(test_data, bad_crosswalk,
                        weight_var = "V1028",
                        anchor = "quarter",
                        calibrate = FALSE),
    "missing"  # More general pattern to match error
  )
})

# =============================================================================
# OUTPUT STRUCTURE TESTS (NO CALIBRATION)
# =============================================================================

test_that("pnadc_apply_periods without calibration returns correct structure", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 5)
  crosswalk <- create_mock_crosswalk(n_upas = 5)

  result <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # Should be data.table
  expect_s3_class(result, "data.table")

  # Should have reference period columns from crosswalk (IBGE-based)
  expect_true("ref_month_start" %in% names(result))
  expect_true("ref_month_in_quarter" %in% names(result))
  expect_true("ref_fortnight_start" %in% names(result))
  expect_true("ref_week_start" %in% names(result))

  # Should have determination flags
  expect_true("determined_month" %in% names(result))
  expect_true("determined_fortnight" %in% names(result))
  expect_true("determined_week" %in% names(result))

  # Should NOT have calibrated weight column when calibrate = FALSE
  expect_false("weight_monthly" %in% names(result))
})

test_that("pnadc_apply_periods preserves original data columns", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 5)
  test_data[, custom_column := "test_value"]
  crosswalk <- create_mock_crosswalk(n_upas = 5)

  result <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  expect_true("custom_column" %in% names(result))
  expect_true("Ano" %in% names(result))
  expect_true("Trimestre" %in% names(result))
  expect_true("V1028" %in% names(result))
})

# =============================================================================
# CALIBRATION TESTS
# =============================================================================

test_that("pnadc_apply_periods with calibration adds weight column", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 5)
  crosswalk <- create_mock_crosswalk(n_upas = 5)
  monthly_totals <- create_monthly_totals(quarters = 1)

  result <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    target_totals = monthly_totals,
    verbose = FALSE
  )

  expect_true("weight_monthly" %in% names(result))
  expect_type(result$weight_monthly, "double")
})

test_that("pnadc_apply_periods respects calibration_unit parameter", {
  skip_if_not(requireNamespace("sidrar", quietly = TRUE),
              "sidrar package not available for fetching population targets")
  skip_if_offline()

  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 5)
  crosswalk <- create_mock_crosswalk(n_upas = 5)
  monthly_totals <- create_monthly_totals(quarters = 1)

  # Month calibration (with explicit targets)
  result_month <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    target_totals = monthly_totals,
    verbose = FALSE
  )
  expect_true("weight_monthly" %in% names(result_month))

  # Fortnight calibration (let it derive targets from SIDRA)
  result_fortnight <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "fortnight",
    target_totals = NULL,  # Auto-derive from SIDRA
    verbose = FALSE
  )
  expect_true("weight_fortnight" %in% names(result_fortnight))

  # Week calibration (let it derive targets from SIDRA)
  result_week <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "week",
    target_totals = NULL,  # Auto-derive from SIDRA
    verbose = FALSE
  )
  expect_true("weight_weekly" %in% names(result_week))
})

# =============================================================================
# KEEP_ALL PARAMETER TESTS
# =============================================================================

test_that("pnadc_apply_periods keep_all = TRUE includes undetermined rows", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 5)
  crosswalk <- create_mock_crosswalk(n_upas = 5)
  monthly_totals <- create_monthly_totals(quarters = 1)

  result <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    target_totals = monthly_totals,
    keep_all = TRUE,
    verbose = FALSE
  )

  # Should include all rows from original data
  expect_equal(nrow(result), nrow(test_data))

  # Undetermined rows should have NA weight
  undetermined <- result[is.na(ref_month_in_quarter)]
  if (nrow(undetermined) > 0) {
    expect_true(all(is.na(undetermined$weight_monthly)))
  }
})

test_that("pnadc_apply_periods keep_all = FALSE excludes undetermined rows", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 5)
  crosswalk <- create_mock_crosswalk(n_upas = 5)
  monthly_totals <- create_monthly_totals(quarters = 1)

  result <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    target_totals = monthly_totals,
    keep_all = FALSE,
    verbose = FALSE
  )

  # Should only include determined rows
  expect_true(all(!is.na(result$ref_month_in_quarter)))
  expect_true(nrow(result) <= nrow(test_data))
})

# =============================================================================
# ANCHOR PARAMETER TESTS
# =============================================================================

test_that("pnadc_apply_periods accepts anchor = 'quarter'", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 5)
  crosswalk <- create_mock_crosswalk(n_upas = 5)
  monthly_totals <- create_monthly_totals(quarters = 1)

  expect_no_error(
    result <- pnadc_apply_periods(
      test_data, crosswalk,
      weight_var = "V1028",
      anchor = "quarter",
      calibrate = TRUE,
      target_totals = monthly_totals,
      verbose = FALSE
    )
  )
})

test_that("pnadc_apply_periods accepts anchor = 'year'", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 4, n_upas = 5)
  crosswalk <- create_mock_crosswalk(n_upas = 5, n_quarters = 4)
  monthly_totals <- create_monthly_totals(quarters = 1:4)

  expect_no_error(
    result <- pnadc_apply_periods(
      test_data, crosswalk,
      weight_var = "V1028",
      anchor = "year",
      calibrate = TRUE,
      target_totals = monthly_totals,
      verbose = FALSE
    )
  )
})

# =============================================================================
# VERBOSE OUTPUT TESTS
# =============================================================================

test_that("pnadc_apply_periods respects verbose parameter", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 3)
  crosswalk <- create_mock_crosswalk(n_upas = 3)

  # verbose = FALSE should produce no output
  expect_silent(
    result <- pnadc_apply_periods(
      test_data, crosswalk,
      weight_var = "V1028",
      anchor = "quarter",
      calibrate = FALSE,
      verbose = FALSE
    )
  )

  # verbose = TRUE should produce output
  expect_output(
    result <- pnadc_apply_periods(
      test_data, crosswalk,
      weight_var = "V1028",
      anchor = "quarter",
      calibrate = FALSE,
      verbose = TRUE
    ),
    "Applying|Joining|crosswalk"
  )
})

# =============================================================================
# EDGE CASE TESTS
# =============================================================================

test_that("pnadc_apply_periods handles empty data gracefully", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 5)
  test_data <- test_data[0]  # Empty data
  crosswalk <- create_mock_crosswalk(n_upas = 5)

  # Should not error, but return empty result
  result <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  expect_equal(nrow(result), 0)
})

test_that("pnadc_apply_periods handles unmatched UPA-V1014 combinations", {
  test_data <- create_test_pnadc_for_apply(n_quarters = 1, n_upas = 5)
  # Crosswalk only has UPAs 1-3
  crosswalk <- create_mock_crosswalk(n_upas = 3, n_panels = 8)

  result <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    keep_all = TRUE,
    verbose = FALSE
  )

  # Some rows should have NA for ref_month (those with UPA 4-5)
  unmatched <- result[UPA > 3]
  if (nrow(unmatched) > 0) {
    expect_true(all(is.na(unmatched$ref_month)))
  }
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("pnadc_apply_periods works with real identify function output", {
  # Create data and run the identify function
  test_data <- create_test_pnadc_for_apply(n_quarters = 2, n_upas = 5)

  # Use actual pnadc_identify_periods if available
  if (exists("pnadc_identify_periods")) {
    crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

    result <- pnadc_apply_periods(
      test_data, crosswalk,
      weight_var = "V1028",
      anchor = "quarter",
      calibrate = FALSE,
      verbose = FALSE
    )

    expect_s3_class(result, "data.table")
    expect_true("ref_month_start" %in% names(result))
    expect_true("ref_fortnight_start" %in% names(result))
    expect_true("ref_week_start" %in% names(result))
  }
})
