# Tests for identify_reference_week()

test_that("identify_reference_week returns correct structure", {
  # Create minimal test data
  test_data <- data.table::data.table(
    Ano = c(2024L, 2024L),
    Trimestre = c(1L, 1L),
    UPA = c(1L, 1L),
    V1008 = c(1L, 1L),  # Same household
    V1014 = c(1L, 1L),
    V2003 = c(1L, 2L),   # Different persons
    V2008 = c(15L, 20L),  # Different birth days
    V20081 = c(1L, 1L),   # Same birth month (Jan)
    V20082 = c(1990L, 1985L),
    V2009 = c(34L, 39L)   # Ages in 2024
  )

  result <- identify_reference_week(test_data, verbose = FALSE)

  # Check output columns exist (new IBGE-based column names)
  expect_true("ref_week_start" %in% names(result))
  expect_true("ref_week_end" %in% names(result))
  expect_true("ref_week_in_quarter" %in% names(result))
  expect_true("ref_week_yyyyww" %in% names(result))

  # Check data types
  expect_s3_class(result$ref_week_start, "Date")
  expect_s3_class(result$ref_week_end, "Date")
  expect_type(result$ref_week_in_quarter, "integer")
  expect_type(result$ref_week_yyyyww, "integer")

  # Check ref_week_in_quarter values are valid (1-12 range for quarters)
  # IBGE quarters always have exactly 12 reference weeks (4 weeks × 3 months)
  valid_values <- result$ref_week_in_quarter[!is.na(result$ref_week_in_quarter)]
  if (length(valid_values) > 0) {
    expect_true(all(valid_values >= 1 & valid_values <= 12))
  }

  expect_s3_class(result, "data.table")
})

test_that("identify_reference_week handles unknown values", {
  # Test with unknown birth day (99)
  test_data <- data.table::data.table(
    Ano = c(2024L),
    Trimestre = c(1L),
    UPA = c(1L),
    V1008 = c(1L),
    V1014 = c(1L),
    V2003 = c(1L),
    V2008 = c(99L),  # Unknown birth day
    V20081 = c(1L),
    V20082 = c(1990L),
    V2009 = c(34L)
  )

  result <- identify_reference_week(test_data, verbose = FALSE)

  # Should not error and should return result (possibly NA)
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
})

test_that("household aggregation improves determination", {
  # Create a household where individual constraints don't determine week,
  # but combined constraints do

  # Person 1: Birthday Jan 15 -> interview after birthday narrows range
  # Person 2: Birthday Jan 20 -> interview after birthday narrows range further
  # The intersection should be narrower than either individual

  test_data <- data.table::data.table(
    Ano = c(2024L, 2024L),
    Trimestre = c(1L, 1L),
    UPA = c(1L, 1L),
    V1008 = c(1L, 1L),  # Same household
    V1014 = c(1L, 1L),
    V2003 = c(1L, 2L),
    V2008 = c(10L, 25L),   # Different birth days in January
    V20081 = c(1L, 1L),
    V20082 = c(1990L, 1990L),
    V2009 = c(34L, 34L)  # Both 34 in 2024 means interview was after birthday
  )

  result <- identify_reference_week(test_data, verbose = FALSE)

  # Both rows should have same determination (same household)
  expect_equal(result$ref_week_yyyyww[1], result$ref_week_yyyyww[2])
  expect_equal(result$ref_week_start[1], result$ref_week_start[2])
})

test_that("different households in same UPA can have different weeks", {
  # Two households in same UPA, different weeks
  test_data <- data.table::data.table(
    Ano = rep(2024L, 4),
    Trimestre = rep(1L, 4),
    UPA = rep(1L, 4),
    V1008 = c(1L, 1L, 2L, 2L),  # Two households
    V1014 = rep(1L, 4),
    V2003 = c(1L, 2L, 1L, 2L),
    V2008 = c(10L, 12L, 25L, 27L),  # HH1: early Jan, HH2: late Jan birthdays
    V20081 = c(1L, 1L, 2L, 2L),     # HH1: Jan, HH2: Feb
    V20082 = rep(1990L, 4),
    V2009 = c(34L, 34L, 33L, 33L)   # Different ages -> different birthday constraints
  )

  result <- identify_reference_week(test_data, verbose = FALSE)

  # Household 1 should have same week
  expect_equal(result$ref_week_yyyyww[1], result$ref_week_yyyyww[2])

  # Household 2 should have same week
  expect_equal(result$ref_week_yyyyww[3], result$ref_week_yyyyww[4])

  # Two households MAY have different weeks (depending on birthday constraints)
  # This is not guaranteed, just possible
})

test_that("IBGE year boundary is handled correctly", {
  # Test with Q4 data that might span year boundary
  test_data <- data.table::data.table(
    Ano = c(2024L),
    Trimestre = c(4L),
    UPA = c(1L),
    V1008 = c(1L),
    V1014 = c(1L),
    V2003 = c(1L),
    V2008 = c(30L),    # Dec 30 birthday
    V20081 = c(12L),
    V20082 = c(1990L),
    V2009 = c(34L)     # 34 in 2024 -> interview after birthday
  )

  result <- identify_reference_week(test_data, verbose = FALSE)

  # Should not error
  expect_s3_class(result, "data.table")

  # If determined, week should be in valid IBGE range for Q4
  if (!is.na(result$ref_week_yyyyww[1])) {
    ibge_yr <- result$ref_week_yyyyww[1] %/% 100L
    # IBGE year should be 2024 or 2025 for Q4 2024
    expect_true(ibge_yr %in% c(2024L, 2025L))
  }
})

test_that("determination rate attribute is set", {
  test_data <- data.table::data.table(
    Ano = rep(2024L, 10),
    Trimestre = rep(1L, 10),
    UPA = 1:10,
    V1008 = rep(1L, 10),
    V1014 = rep(1L, 10),
    V2003 = rep(1L, 10),
    V2008 = rep(15L, 10),
    V20081 = rep(1L, 10),
    V20082 = rep(1990L, 10),
    V2009 = rep(34L, 10)
  )

  result <- identify_reference_week(test_data, verbose = FALSE)

  det_rate <- attr(result, "determination_rate")
  expect_true(!is.null(det_rate))
  expect_true(det_rate >= 0 && det_rate <= 1)
})

test_that("ref_week_start is Sunday of the IBGE week", {
  test_data <- data.table::data.table(
    Ano = c(2024L),
    Trimestre = c(1L),
    UPA = c(1L),
    V1008 = c(1L),
    V1014 = c(1L),
    V2003 = c(1L),
    V2008 = c(15L),
    V20081 = c(2L),    # Feb birthday
    V20082 = c(1990L),
    V2009 = c(34L)
  )

  result <- identify_reference_week(test_data, verbose = FALSE)

  if (!is.na(result$ref_week_start[1])) {
    # ref_week_start should be a Sunday (day 7 in ISO %u format)
    dow <- as.integer(format(result$ref_week_start[1], "%u"))
    expect_equal(dow, 7L)  # Sunday
  }
})

test_that("ref_week_end is Saturday of the IBGE week", {
  test_data <- data.table::data.table(
    Ano = c(2024L),
    Trimestre = c(1L),
    UPA = c(1L),
    V1008 = c(1L),
    V1014 = c(1L),
    V2003 = c(1L),
    V2008 = c(15L),
    V20081 = c(2L),    # Feb birthday
    V20082 = c(1990L),
    V2009 = c(34L)
  )

  result <- identify_reference_week(test_data, verbose = FALSE)

  if (!is.na(result$ref_week_end[1])) {
    # ref_week_end should be a Saturday (day 6 in ISO %u format)
    dow <- as.integer(format(result$ref_week_end[1], "%u"))
    expect_equal(dow, 6L)  # Saturday
  }
})

test_that("ref_week_start and ref_week_end are 6 days apart", {
  test_data <- data.table::data.table(
    Ano = c(2024L),
    Trimestre = c(1L),
    UPA = c(1L),
    V1008 = c(1L),
    V1014 = c(1L),
    V2003 = c(1L),
    V2008 = c(15L),
    V20081 = c(1L),
    V20082 = c(1990L),
    V2009 = c(34L)
  )

  result <- identify_reference_week(test_data, verbose = FALSE)

  if (!is.na(result$ref_week_start[1]) && !is.na(result$ref_week_end[1])) {
    # Week is Sunday to Saturday = 6 days apart
    diff_days <- as.integer(result$ref_week_end[1] - result$ref_week_start[1])
    expect_equal(diff_days, 6L)
  }
})

test_that("ref_week_yyyyww matches ref_week_start", {
  test_data <- data.table::data.table(
    Ano = c(2024L),
    Trimestre = c(1L),
    UPA = c(1L),
    V1008 = c(1L),
    V1014 = c(1L),
    V2003 = c(1L),
    V2008 = c(15L),
    V20081 = c(1L),
    V20082 = c(1990L),
    V2009 = c(34L)
  )

  result <- identify_reference_week(test_data, verbose = FALSE)

  if (!is.na(result$ref_week_start[1]) && !is.na(result$ref_week_yyyyww[1])) {
    # Convert ref_week_start back to IBGE yyyyww and compare
    expected_yyyyww <- PNADCperiods:::date_to_ibge_yyyyww(result$ref_week_start[1])
    expect_equal(result$ref_week_yyyyww[1], expected_yyyyww)
  }
})
