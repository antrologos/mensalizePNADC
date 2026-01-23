# Tests for identify_reference_fortnight()

# =============================================================================
# INPUT VALIDATION TESTS
# =============================================================================

test_that("identify_reference_fortnight validates input", {
  # Missing required columns - should error
  bad_data <- data.frame(Ano = 2023, Trimestre = 1)
  expect_error(identify_reference_fortnight(bad_data))
})

test_that("identify_reference_fortnight returns correct structure", {
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

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Check output columns exist (new IBGE-based column names)
  expect_true("ref_fortnight_start" %in% names(result))
  expect_true("ref_fortnight_end" %in% names(result))
  expect_true("ref_fortnight_in_quarter" %in% names(result))
  expect_true("ref_fortnight_yyyyff" %in% names(result))
  expect_true("ref_fortnight_weeks" %in% names(result))

  # Check ref_fortnight_in_quarter values are valid (1-6)
  valid_values <- result$ref_fortnight_in_quarter[!is.na(result$ref_fortnight_in_quarter)]
  expect_true(all(valid_values %in% 1:6))

  # Output is a data.table

  expect_s3_class(result, "data.table")
})

# =============================================================================
# IBGE FORTNIGHT HELPER FUNCTION TESTS
# =============================================================================

test_that("ibge_fortnight_in_quarter calculates correctly", {
  # Q1: Jan, Feb, Mar -> positions 1-6
  # Each month has 2 fortnights (fortnight 1 = weeks 1-2, fortnight 2 = weeks 3+)

  # Early January (should be in first fortnight of Q1)
  date1 <- as.Date("2024-01-10")
  result1 <- PNADCperiods:::ibge_fortnight_in_quarter(date1, 1L, 2024L, min_days = 4L)
  expect_true(result1 %in% 1:2)  # First month fortnights

  # Late January (second fortnight of first month)
  date2 <- as.Date("2024-01-25")
  result2 <- PNADCperiods:::ibge_fortnight_in_quarter(date2, 1L, 2024L, min_days = 4L)
  expect_true(result2 %in% 1:2)  # Still in January's range

  # Early March (fifth fortnight)
  date3 <- as.Date("2024-03-05")
  result3 <- PNADCperiods:::ibge_fortnight_in_quarter(date3, 1L, 2024L, min_days = 4L)
  expect_true(result3 %in% 5:6)  # Third month fortnights
})

test_that("ibge_fortnight_in_quarter works for all quarters", {
  # Q2: Apr, May, Jun
  result_q2 <- PNADCperiods:::ibge_fortnight_in_quarter(as.Date("2024-04-15"), 2L, 2024L, min_days = 4L)
  expect_true(result_q2 %in% 1:6)

  # Q3: Jul, Aug, Sep
  result_q3 <- PNADCperiods:::ibge_fortnight_in_quarter(as.Date("2024-07-10"), 3L, 2024L, min_days = 4L)
  expect_true(result_q3 %in% 1:6)

  # Q4: Oct, Nov, Dec
  result_q4 <- PNADCperiods:::ibge_fortnight_in_quarter(as.Date("2024-10-01"), 4L, 2024L, min_days = 4L)
  expect_true(result_q4 %in% 1:6)
})

test_that("ibge_fortnight_in_quarter_to_yyyyff calculates correctly", {
  # Q1 position 1 = fortnight 1 of year
  expect_equal(PNADCperiods:::ibge_fortnight_in_quarter_to_yyyyff(2024L, 1L, 1L), 202401L)

  # Q1 position 6 = fortnight 6 of year
  expect_equal(PNADCperiods:::ibge_fortnight_in_quarter_to_yyyyff(2024L, 1L, 6L), 202406L)

  # Q2 position 1 = fortnight 7 of year
  expect_equal(PNADCperiods:::ibge_fortnight_in_quarter_to_yyyyff(2024L, 2L, 1L), 202407L)

  # Q3 position 3 = fortnight 15 of year (12 + 3 = 15)
  expect_equal(PNADCperiods:::ibge_fortnight_in_quarter_to_yyyyff(2024L, 3L, 3L), 202415L)

  # Q4 position 6 = fortnight 24 of year
  expect_equal(PNADCperiods:::ibge_fortnight_in_quarter_to_yyyyff(2024L, 4L, 6L), 202424L)
})

test_that("ibge_yyyyff_to_date returns Sunday of first week", {
  # First fortnight of January: should be a Sunday
  result1 <- PNADCperiods:::ibge_yyyyff_to_date(202401L)
  expect_equal(as.integer(format(result1, "%u")), 7L)  # Sunday

  # Third fortnight = Feb first fortnight
  result3 <- PNADCperiods:::ibge_yyyyff_to_date(202403L)
  expect_equal(as.integer(format(result3, "%u")), 7L)  # Sunday

  # Last fortnight of year
  result24 <- PNADCperiods:::ibge_yyyyff_to_date(202424L)
  expect_equal(as.integer(format(result24, "%u")), 7L)  # Sunday
})

test_that("ibge_fortnight_start and ibge_fortnight_end are consistent", {
  # Test that fortnight 1 ends before fortnight 2 starts
  year <- 2024L
  month <- 3L

  f1_end <- PNADCperiods:::ibge_fortnight_end(year, month, 1L, min_days = 4L)
  f2_start <- PNADCperiods:::ibge_fortnight_start(year, month, 2L, min_days = 4L)

  # Fortnight 2 starts the day after fortnight 1 ends
  expect_equal(f2_start, f1_end + 1L)

  # Fortnight 1 start should be a Sunday, end should be a Saturday
  f1_start <- PNADCperiods:::ibge_fortnight_start(year, month, 1L, min_days = 4L)
  expect_equal(as.integer(format(f1_start, "%u")), 7L)  # Sunday
  expect_equal(as.integer(format(f1_end, "%u")), 6L)    # Saturday
})

test_that("ibge_fortnight_weeks returns correct values", {
  # IBGE fortnights ALWAYS have exactly 2 weeks (14 days)
  # Fortnight 1 = weeks 1-2, Fortnight 2 = weeks 3-4
  expect_equal(PNADCperiods:::ibge_fortnight_weeks(2024L, 1L, 1L, min_days = 4L), 2L)
  expect_equal(PNADCperiods:::ibge_fortnight_weeks(2024L, 6L, 1L, min_days = 4L), 2L)

  # Fortnight 2 also always has exactly 2 weeks
  expect_equal(PNADCperiods:::ibge_fortnight_weeks(2024L, 1L, 2L, min_days = 4L), 2L)
  expect_equal(PNADCperiods:::ibge_fortnight_weeks(2024L, 6L, 2L, min_days = 4L), 2L)
})

# =============================================================================
# NA HANDLING TESTS
# =============================================================================

test_that("identify_reference_fortnight handles NA values", {
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

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Should complete without error
  expect_s3_class(result, "data.table")

  # Should have all output columns (new IBGE-based names)
  expect_true(all(c("ref_fortnight_start", "ref_fortnight_end",
                    "ref_fortnight_in_quarter", "ref_fortnight_yyyyff") %in% names(result)))
})

test_that("identify_reference_fortnight handles all-NA groups", {
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

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Should complete without error (infinite values handled)
  expect_s3_class(result, "data.table")
})

# =============================================================================
# DETERMINATION RATE TESTS
# =============================================================================

test_that("identify_reference_fortnight stores determination rate attribute", {
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

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

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

test_that("identify_reference_fortnight returns unique rows per UPA-V1014", {
  # Multiple persons in same UPA-V1014
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

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Should have exactly 2 rows (one per UPA-V1014 combination)
  expect_equal(nrow(result), 2L)

  # Should be unique by UPA-V1014
  expect_equal(nrow(unique(result[, .(UPA, V1014)])), nrow(result))
})

# =============================================================================
# EDGE CASE TESTS
# =============================================================================

test_that("identify_reference_fortnight handles year boundaries", {
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

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Should complete without error
  expect_s3_class(result, "data.table")

  # ref_fortnight_yyyyff should be in valid range for Q4 (19-24)
  valid_values <- result$ref_fortnight_yyyyff[!is.na(result$ref_fortnight_yyyyff)]
  if (length(valid_values) > 0) {
    ff <- valid_values %% 100
    expect_true(all(ff >= 19 & ff <= 24))
  }
})

test_that("identify_reference_fortnight does NOT aggregate across quarters", {
  # Data spanning multiple quarters
  test_data <- data.frame(
    Ano = c(2023, 2023, 2023, 2023),
    Trimestre = c(1, 2, 3, 4),
    UPA = rep(1, 4),
    V1008 = rep(1, 4),
    V1014 = rep(1, 4),
    V2003 = rep(1, 4),
    V2008 = c(15, 15, 15, 15),
    V20081 = c(1, 4, 7, 10),  # Birthdays in each quarter
    V20082 = rep(1990, 4),
    V2009 = rep(33, 4)
  )

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Fortnights do NOT aggregate across quarters (unlike months)
  # Should have one row per household-quarter (4 quarters)
  expect_equal(nrow(result), 4L)
})

# =============================================================================
# IBGE WEEK BOUNDARY TESTS
# =============================================================================

test_that("ref_fortnight_start is always a Sunday", {
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

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Check that ref_fortnight_start dates are Sundays
  valid_starts <- result$ref_fortnight_start[!is.na(result$ref_fortnight_start)]
  if (length(valid_starts) > 0) {
    dows <- as.integer(format(valid_starts, "%u"))
    expect_true(all(dows == 7L))  # Sunday = 7 in ISO
  }
})

test_that("ref_fortnight_end is always a Saturday", {
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

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Check that ref_fortnight_end dates are Saturdays
  valid_ends <- result$ref_fortnight_end[!is.na(result$ref_fortnight_end)]
  if (length(valid_ends) > 0) {
    dows <- as.integer(format(valid_ends, "%u"))
    expect_true(all(dows == 6L))  # Saturday = 6 in ISO
  }
})
