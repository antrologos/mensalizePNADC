# Tests for pnadc_identify_periods() - main crosswalk builder

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Create minimal PNADC data for identification tests
#' @param n_quarters Number of quarters to generate
#' @param n_upas Number of UPAs per quarter
#' @param persons_per_household Number of persons per household
create_test_pnadc <- function(n_quarters = 4, n_upas = 10, persons_per_household = 3) {
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

  dt
}

#' Create PNADC data with consistent UPA-V1014 patterns for determinable months
create_determinable_pnadc <- function() {
  set.seed(123)

  # Create data where all persons in same UPA-V1014 have birthdays

  # that constrain to the same month
  dt <- data.table::data.table(
    Ano = rep(2023L, 100),
    Trimestre = rep(c(1L, 2L, 3L, 4L), each = 25),
    UPA = rep(1:5, each = 20),
    V1008 = rep(rep(1:2, each = 10), 5),
    V1014 = rep(1:5, each = 20),
    V2003 = rep(1:10, 10)
  )

  # Give all persons in same UPA-V1014 similar birthdays
  # to maximize chance of determination
  dt[, `:=`(
    V2008 = 15L,  # All born mid-month
    V20081 = ((V1014 - 1L) %% 3L) + 1L,  # Birth month varies by panel
    V20082 = 1990L,
    V2009 = 33L  # Consistent age
  )]

  dt
}

# =============================================================================
# INPUT VALIDATION TESTS
# =============================================================================

test_that("pnadc_identify_periods validates required columns", {
  bad_data <- data.frame(Ano = 2023, Trimestre = 1)

  expect_error(
    pnadc_identify_periods(bad_data, verbose = FALSE),
    "missing"
  )
})

test_that("pnadc_identify_periods handles character columns", {
  test_data <- create_test_pnadc(n_quarters = 1, n_upas = 5)
  test_data[, Ano := as.character(Ano)]
  test_data[, Trimestre := as.character(Trimestre)]

  result <- pnadc_identify_periods(test_data, verbose = FALSE)
  expect_s3_class(result, "data.table")
})

# =============================================================================
# OUTPUT STRUCTURE TESTS
# =============================================================================

test_that("pnadc_identify_periods returns correct crosswalk structure", {
  test_data <- create_test_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Should be data.table

  expect_s3_class(result, "data.table")

  # Should have household-quarter level keys
  expect_true("Ano" %in% names(result))
  expect_true("Trimestre" %in% names(result))
  expect_true("UPA" %in% names(result))
  expect_true("V1008" %in% names(result))
  expect_true("V1014" %in% names(result))

  # Should NOT have person-level keys
  expect_false("V2003" %in% names(result))

  # Should have month columns (IBGE-based)
  expect_true("ref_month_start" %in% names(result))
  expect_true("ref_month_end" %in% names(result))
  expect_true("ref_month_in_quarter" %in% names(result))
  expect_true("ref_month_yyyymm" %in% names(result))
  expect_true("ref_month_weeks" %in% names(result))
  expect_true("determined_month" %in% names(result))

  # Should have fortnight columns (IBGE-based)
  expect_true("ref_fortnight_start" %in% names(result))
  expect_true("ref_fortnight_end" %in% names(result))
  expect_true("ref_fortnight_in_quarter" %in% names(result))
  expect_true("ref_fortnight_yyyyff" %in% names(result))
  expect_true("determined_fortnight" %in% names(result))

  # Should have week columns (IBGE-based)
  expect_true("ref_week_start" %in% names(result))
  expect_true("ref_week_end" %in% names(result))
  expect_true("ref_week_in_quarter" %in% names(result))
  expect_true("ref_week_yyyyww" %in% names(result))
  expect_true("determined_week" %in% names(result))
})

test_that("pnadc_identify_periods returns one row per household-quarter-panel", {
  test_data <- create_test_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Count unique household-quarter-panel combinations in input
  # V1014 is included because it's a key for the month identification
  n_expected <- nrow(unique(test_data[, .(Ano, Trimestre, UPA, V1008, V1014)]))

  # Result should have same number of observations
  expect_equal(nrow(result), n_expected)
})

test_that("pnadc_identify_periods has determination_rates attribute", {
  test_data <- create_test_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  rates <- attr(result, "determination_rates")
  expect_type(rates, "list")
  expect_true("month" %in% names(rates))
  expect_true("fortnight" %in% names(rates))
  expect_true("week" %in% names(rates))

  # Rates should be between 0 and 1
  expect_true(rates$month >= 0 && rates$month <= 1)
  expect_true(rates$fortnight >= 0 && rates$fortnight <= 1)
  expect_true(rates$week >= 0 && rates$week <= 1)
})

# =============================================================================
# DETERMINATION FLAG TESTS
# =============================================================================

test_that("determination flags are consistent with ref values", {
  test_data <- create_test_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # determined_month should be TRUE iff ref_month_in_quarter is not NA
  expect_equal(
    result$determined_month,
    !is.na(result$ref_month_in_quarter)
  )

  # determined_fortnight should be TRUE iff ref_fortnight_in_quarter is not NA
  expect_equal(
    result$determined_fortnight,
    !is.na(result$ref_fortnight_in_quarter)
  )

  # determined_week should be TRUE iff ref_week_in_quarter is not NA
  expect_equal(
    result$determined_week,
    !is.na(result$ref_week_in_quarter)
  )
})

# =============================================================================
# REFERENCE VALUE CONSISTENCY TESTS
# =============================================================================
test_that("ref_month_in_quarter values are 1, 2, 3, or NA", {
  test_data <- create_test_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  valid_values <- c(1L, 2L, 3L, NA_integer_)
  expect_true(all(result$ref_month_in_quarter %in% valid_values))
})

test_that("ref_fortnight_in_quarter values are 1-6 or NA", {
  test_data <- create_test_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  non_na <- result$ref_fortnight_in_quarter[!is.na(result$ref_fortnight_in_quarter)]
  if (length(non_na) > 0) {
    expect_true(all(non_na >= 1L & non_na <= 6L))
  }
})

test_that("ref_week_in_quarter values are 1-14 or NA", {
  test_data <- create_test_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  non_na <- result$ref_week_in_quarter[!is.na(result$ref_week_in_quarter)]
  if (length(non_na) > 0) {
    expect_true(all(non_na >= 1L & non_na <= 14L))
  }
})

test_that("ref_month_yyyymm is consistent with ref_month Date", {
  test_data <- create_test_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  determined <- result[determined_month == TRUE]
  if (nrow(determined) > 0) {
    # Extract year and month from ref_month Date
    expected_yyyymm <- data.table::year(determined$ref_month) * 100L +
                       data.table::month(determined$ref_month)
    expect_equal(determined$ref_month_yyyymm, expected_yyyymm)
  }
})

# =============================================================================
# FORTNIGHT FORMAT TESTS
# =============================================================================

test_that("ref_fortnight_yyyyff follows YYYY01-YYYY24 format", {
  test_data <- create_test_pnadc(n_quarters = 4, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  determined <- result[determined_fortnight == TRUE]
  if (nrow(determined) > 0) {
    years <- determined$ref_fortnight_yyyyff %/% 100L
    fortnights <- determined$ref_fortnight_yyyyff %% 100L

    # Fortnights should be 1-24
    expect_true(all(fortnights >= 1L & fortnights <= 24L))

    # Years should be reasonable
    expect_true(all(years >= 2000L & years <= 2100L))
  }
})

test_that("ref_fortnight Date is 1st or 16th of month", {
  test_data <- create_test_pnadc(n_quarters = 4, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  determined <- result[determined_fortnight == TRUE]
  if (nrow(determined) > 0) {
    days <- data.table::mday(determined$ref_fortnight)
    expect_true(all(days %in% c(1L, 16L)))
  }
})

# =============================================================================
# VERBOSE OUTPUT TESTS
# =============================================================================

test_that("pnadc_identify_periods respects verbose parameter", {
  test_data <- create_test_pnadc(n_quarters = 1, n_upas = 3)

  # verbose = FALSE should produce no output
  expect_silent(result <- pnadc_identify_periods(test_data, verbose = FALSE))

  # verbose = TRUE should produce output
  expect_output(
    result <- pnadc_identify_periods(test_data, verbose = TRUE),
    "Building|Step|determination"
  )
})

# =============================================================================
# DETERMINATION RATE HIERARCHY TESTS
# =============================================================================

test_that("month determination rate >= fortnight rate >= week rate", {
  # This tests the expected hierarchy: coarser granularity = higher determination
  test_data <- create_test_pnadc(n_quarters = 4, n_upas = 10)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)
  rates <- attr(result, "determination_rates")

  # Month should have highest (or equal) rate

  expect_true(rates$month >= rates$fortnight ||
              abs(rates$month - rates$fortnight) < 0.01)  # Allow small tolerance

  # Fortnight should have higher (or equal) rate than week
  expect_true(rates$fortnight >= rates$week ||
              abs(rates$fortnight - rates$week) < 0.01)
})
