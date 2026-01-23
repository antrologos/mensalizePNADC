# Tests for identify_reference_month()

test_that("identify_reference_month validates input", {
  # Missing required columns - should error
  bad_data <- data.frame(Ano = 2023, Trimestre = 1)
  expect_error(identify_reference_month(bad_data))

  # Note: Invalid quarter values (e.g., Trimestre = 5) are not explicitly

  # validated; the algorithm handles them by producing NA results.
  # This is acceptable as the input is expected to be valid PNADC data.
})

test_that("identify_reference_month returns correct structure", {
  # Create minimal valid data
  test_data <- data.frame(
    Ano = c(2023, 2023),
    Trimestre = c(1, 1),
    UPA = c(1, 1),
    V1008 = c(1, 1),
    V1014 = c(1, 1),
    V2003 = c(1, 2),
    V2008 = c(15, 20),
    V20081 = c(6, 3),
    V20082 = c(1990, 1985),
    V2009 = c(33, 38)
  )

  result <- identify_reference_month(test_data)

  # Check output columns exist (new IBGE-based column names)
  expect_true("ref_month_start" %in% names(result))
  expect_true("ref_month_end" %in% names(result))
  expect_true("ref_month_in_quarter" %in% names(result))
  expect_true("ref_month_yyyymm" %in% names(result))
  expect_true("ref_month_weeks" %in% names(result))

  # Check ref_month_in_quarter values are valid
  valid_values <- result$ref_month_in_quarter[!is.na(result$ref_month_in_quarter)]
  expect_true(all(valid_values %in% 1:3))

  # Output is a plain data.table (no custom class)
  expect_s3_class(result, "data.table")
})

test_that("identify_reference_month prints as data.table", {
  test_data <- data.frame(
    Ano = 2023, Trimestre = 1,
    UPA = 1, V1008 = 1, V1014 = 1, V2003 = 1,
    V2008 = 15, V20081 = 6, V20082 = 1990, V2009 = 33
  )

  result <- identify_reference_month(test_data)

  # Should print without error (standard data.table print)
  expect_output(print(result), "Ano")
})

# =============================================================================
# NA HANDLING TESTS
# =============================================================================

test_that("identify_reference_month handles NA values", {
  # Data with missing birthday info
  test_data <- data.frame(
    Ano = c(2023, 2023, 2023),
    Trimestre = c(1, 1, 1),
    UPA = c(1, 1, 2),
    V1008 = c(1, 1, 1),
    V1014 = c(1, 1, 1),
    V2003 = c(1, 2, 1),
    V2008 = c(15, 99, 20),      # 99 = unknown (converted to NA)
    V20081 = c(6, 99, 3),       # 99 = unknown (converted to NA)
    V20082 = c(1990, 9999, 1985), # 9999 = unknown (converted to NA)
    V2009 = c(33, NA, 38)
  )

  result <- identify_reference_month(test_data, verbose = FALSE)

  # Should complete without error
  expect_s3_class(result, "data.table")

  # Should have all output columns (new IBGE-based names)
  expect_true(all(c("ref_month_start", "ref_month_end", "ref_month_in_quarter",
                    "ref_month_yyyymm", "ref_month_weeks") %in% names(result)))
})

test_that("identify_reference_month handles all-NA groups", {
  # All persons in a UPA-panel have unknown birthdays
  test_data <- data.frame(
    Ano = c(2023, 2023),
    Trimestre = c(1, 1),
    UPA = c(1, 1),
    V1008 = c(1, 1),
    V1014 = c(1, 1),
    V2003 = c(1, 2),
    V2008 = c(99, 99),      # All unknown
    V20081 = c(99, 99),
    V20082 = c(9999, 9999),
    V2009 = c(NA, NA)
  )

  result <- identify_reference_month(test_data, verbose = FALSE)

  # Should complete without error (infinite values handled)
  expect_s3_class(result, "data.table")
})

# =============================================================================
# DETERMINATION RATE TESTS
# =============================================================================

test_that("identify_reference_month stores determination rate attribute", {
  test_data <- data.frame(
    Ano = c(2023, 2023),
    Trimestre = c(1, 1),
    UPA = c(1, 1),
    V1008 = c(1, 1),
    V1014 = c(1, 1),
    V2003 = c(1, 2),
    V2008 = c(15, 20),
    V20081 = c(6, 3),
    V20082 = c(1990, 1985),
    V2009 = c(33, 38)
  )

  result <- identify_reference_month(test_data, verbose = FALSE)

  # Should have determination_rate attribute
  expect_true(!is.null(attr(result, "determination_rate")))

  # Attribute should be numeric between 0 and 1
  rate <- attr(result, "determination_rate")
  expect_true(is.numeric(rate))
  expect_true(rate >= 0 && rate <= 1)
})

# =============================================================================
# OUTPUT UNIQUENESS TESTS
# =============================================================================

test_that("identify_reference_month returns data at expected granularity", {
  # Multiple persons in same household
  test_data <- data.frame(
    Ano = rep(2023, 4),
    Trimestre = rep(1, 4),
    UPA = c(1, 1, 1, 2),
    V1008 = c(1, 1, 1, 1),
    V1014 = c(1, 1, 1, 1),
    V2003 = c(1, 2, 3, 1),
    V2008 = c(15, 20, 10, 5),
    V20081 = c(6, 3, 9, 2),
    V20082 = c(1990, 1985, 2000, 1995),
    V2009 = c(33, 38, 23, 28)
  )

  result <- identify_reference_month(test_data, verbose = FALSE)

  # Result should be a data.table with expected columns
  expect_s3_class(result, "data.table")
  expect_true(all(c("ref_month_start", "ref_month_end", "ref_month_in_quarter",
                    "ref_month_yyyymm", "ref_month_weeks") %in% names(result)))

  # Should have at least one row
  expect_true(nrow(result) >= 1L)
})

# =============================================================================
# EDGE CASE TESTS
# =============================================================================

test_that("identify_reference_month handles year boundaries", {
  # Q4 data near end of year
  test_data <- data.frame(
    Ano = c(2023, 2023),
    Trimestre = c(4, 4),
    UPA = c(1, 1),
    V1008 = c(1, 1),
    V1014 = c(1, 1),
    V2003 = c(1, 2),
    V2008 = c(25, 10),
    V20081 = c(12, 11),     # Dec and Nov birthdays
    V20082 = c(1990, 1985),
    V2009 = c(33, 38)
  )

  result <- identify_reference_month(test_data, verbose = FALSE)

  # Should complete without error
  expect_s3_class(result, "data.table")

  # ref_month_yyyymm should be in valid range for Q4
  valid_values <- result$ref_month_yyyymm[!is.na(result$ref_month_yyyymm)]
  if (length(valid_values) > 0) {
    mm <- valid_values %% 100
    expect_true(all(mm >= 10 & mm <= 12))
  }
})

test_that("identify_reference_month aggregates across quarters at UPA-V1014 level", {
  # Data spanning multiple quarters (same UPA-V1014 across quarters)
  test_data <- data.frame(
    Ano = c(2023, 2023, 2023, 2023),
    Trimestre = c(1, 2, 3, 4),
    UPA = rep(1, 4),
    V1008 = rep(1, 4),
    V1014 = rep(1, 4),
    V2003 = rep(1, 4),
    V2008 = c(15, 15, 15, 15),
    V20081 = c(1, 4, 7, 10),  # Birthdays in each quarter's first month
    V20082 = rep(1990, 4),
    V2009 = rep(33, 4)
  )

  result <- identify_reference_month(test_data, verbose = FALSE)

  # identify_reference_month aggregates at UPA-V1014 level across quarters
  # With 4 quarters for same UPA-V1014, should produce one aggregated result
  # The result includes all join keys but ref_month values are determined from aggregation
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) >= 1L)

  # All rows should have same UPA-V1014
  expect_equal(length(unique(result$UPA)), 1L)
  expect_equal(length(unique(result$V1014)), 1L)
})

# =============================================================================
# IBGE WEEK BOUNDARY TESTS
# =============================================================================

test_that("ref_month_start is always a Sunday", {
  test_data <- data.frame(
    Ano = c(2023, 2023, 2024, 2024),
    Trimestre = c(1, 2, 3, 4),
    UPA = c(1, 2, 3, 4),
    V1008 = rep(1, 4),
    V1014 = rep(1, 4),
    V2003 = rep(1, 4),
    V2008 = c(15, 20, 10, 5),
    V20081 = c(1, 4, 7, 10),
    V20082 = rep(1990, 4),
    V2009 = rep(33, 4)
  )

  result <- identify_reference_month(test_data, verbose = FALSE)

  # Check that ref_month_start dates are Sundays
  valid_starts <- result$ref_month_start[!is.na(result$ref_month_start)]
  if (length(valid_starts) > 0) {
    dows <- as.integer(format(valid_starts, "%u"))
    expect_true(all(dows == 7L))  # Sunday = 7 in ISO
  }
})

test_that("ref_month_end is always a Saturday", {
  test_data <- data.frame(
    Ano = c(2023, 2023, 2024, 2024),
    Trimestre = c(1, 2, 3, 4),
    UPA = c(1, 2, 3, 4),
    V1008 = rep(1, 4),
    V1014 = rep(1, 4),
    V2003 = rep(1, 4),
    V2008 = c(15, 20, 10, 5),
    V20081 = c(1, 4, 7, 10),
    V20082 = rep(1990, 4),
    V2009 = rep(33, 4)
  )

  result <- identify_reference_month(test_data, verbose = FALSE)

  # Check that ref_month_end dates are Saturdays
  valid_ends <- result$ref_month_end[!is.na(result$ref_month_end)]
  if (length(valid_ends) > 0) {
    dows <- as.integer(format(valid_ends, "%u"))
    expect_true(all(dows == 6L))  # Saturday = 6 in ISO
  }
})

test_that("ref_month_weeks is always exactly 4", {
  # IBGE months ALWAYS have exactly 4 reference weeks (28 days)
  # There is no "week 5" - gaps between months are technical stops
  test_data <- data.frame(
    Ano = c(2023, 2023, 2024, 2024),
    Trimestre = c(1, 2, 3, 4),
    UPA = c(1, 2, 3, 4),
    V1008 = rep(1, 4),
    V1014 = rep(1, 4),
    V2003 = rep(1, 4),
    V2008 = c(15, 20, 10, 5),
    V20081 = c(1, 4, 7, 10),
    V20082 = rep(1990, 4),
    V2009 = rep(33, 4)
  )

  result <- identify_reference_month(test_data, verbose = FALSE)

  # Check that ref_month_weeks is always 4
  valid_weeks <- result$ref_month_weeks[!is.na(result$ref_month_weeks)]
  if (length(valid_weeks) > 0) {
    expect_true(all(valid_weeks == 4L))
  }
})

# =============================================================================
# HELPER FUNCTION TESTS
# =============================================================================

test_that("calculate_month_position_min works correctly", {
  # Q1: Jan=1, Feb=2, Mar=3
  # First month position for Jan 10 (day > 3)
  date1 <- as.Date("2024-01-10")
  expect_equal(PNADCperiods:::calculate_month_position_min(date1, 2024L, 1L), 1L)

  # First month position for Feb 2 (day <= 3, adjust back)
  date2 <- as.Date("2024-02-02")
  expect_equal(PNADCperiods:::calculate_month_position_min(date2, 2024L, 1L), 1L)

  # First month position for Mar 15
  date3 <- as.Date("2024-03-15")
  expect_equal(PNADCperiods:::calculate_month_position_min(date3, 2024L, 1L), 3L)
})

test_that("calculate_month_position_max works correctly", {
  # Q1: Jan=1, Feb=2, Mar=3
  # Max month position for Jan 31
  date1 <- as.Date("2024-01-31")
  expect_equal(PNADCperiods:::calculate_month_position_max(date1, 2024L, 1L), 1L)

  # Max month position for Feb 2 (day <= 3, adjust back)
  date2 <- as.Date("2024-02-02")
  expect_equal(PNADCperiods:::calculate_month_position_max(date2, 2024L, 1L), 1L)

  # Max month position for Mar 25
  date3 <- as.Date("2024-03-25")
  expect_equal(PNADCperiods:::calculate_month_position_max(date3, 2024L, 1L), 3L)
})

test_that("yyyymm utility calculates correctly", {
  # Test the yyyymm function used internally
  expect_equal(PNADCperiods:::yyyymm(2024L, 1L), 202401L)
  expect_equal(PNADCperiods:::yyyymm(2024L, 12L), 202412L)
  expect_equal(PNADCperiods:::yyyymm(2023L, 6L), 202306L)
})
