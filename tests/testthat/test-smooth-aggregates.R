# Tests for smooth_monthly_aggregates()

# Helper to create monthly aggregate test data
create_monthly_aggregates <- function(n_months = 24, start_yyyymm = 201301L) {
  # Create a sequence of YYYYMM values
  year <- start_yyyymm %/% 100L
  month <- start_yyyymm %% 100L

  yyyymm <- integer(n_months)
  for (i in seq_len(n_months)) {
    yyyymm[i] <- year * 100L + month
    month <- month + 1L
    if (month > 12L) {
      month <- 1L
      year <- year + 1L
    }
  }

  # Create population with some trend
  base_pop <- 200000
  trend <- seq(0, 5000, length.out = n_months)
  noise <- rnorm(n_months, 0, 100)

  data.table::data.table(
    ref_month_yyyymm = yyyymm,
    z_populacao = base_pop + trend + noise
  )
}

test_that("smooth_monthly_aggregates validates input", {
  # Needs at least 3 rows
  small_data <- data.table::data.table(
    ref_month_yyyymm = c(202301L, 202302L),
    z_populacao = c(100, 101)
  )
  expect_error(smooth_monthly_aggregates(small_data))
})

test_that("smooth_monthly_aggregates validates calibration parameters", {
  test_data <- create_monthly_aggregates(24)

  expect_error(smooth_monthly_aggregates(test_data, calibration_start = "2013"))
  expect_error(smooth_monthly_aggregates(test_data, calibration_end = "2019"))
})

test_that("smooth_monthly_aggregates returns m_ prefixed columns", {
  test_data <- create_monthly_aggregates(24)

  result <- smooth_monthly_aggregates(test_data)

  # Should have m_populacao (from z_populacao)
  expect_true("m_populacao" %in% names(result))
  expect_type(result$m_populacao, "double")
})

test_that("smooth_monthly_aggregates preserves ref_month_yyyymm", {
  test_data <- create_monthly_aggregates(24)
  original_months <- test_data$ref_month_yyyymm

  result <- smooth_monthly_aggregates(test_data)

  expect_equal(result$ref_month_yyyymm, original_months)
})

test_that("smooth_monthly_aggregates warns when no z_ columns found", {
  test_data <- data.table::data.table(
    ref_month_yyyymm = c(202301L, 202302L, 202303L, 202304L),
    populacao = c(100, 101, 102, 103)  # No z_ prefix
  )

  expect_warning(
    result <- smooth_monthly_aggregates(test_data),
    "No indicator columns"
  )
})

test_that("smooth_monthly_aggregates handles multiple z_ columns", {
  test_data <- data.table::data.table(
    ref_month_yyyymm = seq(201301L, 201312L, by = 1L),
    z_pop = 200000 + seq(0, 1100, by = 100),
    z_employed = 100000 + seq(0, 550, by = 50)
  )
  # Fix YYYYMM sequence to be valid
  test_data[, ref_month_yyyymm := {
    year <- 2013L
    month <- 1:12
    year * 100L + month
  }]

  result <- smooth_monthly_aggregates(test_data)

  # Should have both m_ columns
  expect_true("m_pop" %in% names(result))
  expect_true("m_employed" %in% names(result))
})

test_that("smooth_monthly_aggregates handles all NA values", {
  test_data <- data.table::data.table(
    ref_month_yyyymm = c(202301L, 202302L, 202303L, 202304L),
    z_populacao = rep(NA_real_, 4)
  )

  result <- smooth_monthly_aggregates(test_data)

  # m_populacao should be all NA
  expect_true(all(is.na(result$m_populacao)))
})

test_that("smooth_monthly_aggregates removes temporary columns", {
  test_data <- create_monthly_aggregates(12)

  result <- smooth_monthly_aggregates(test_data)

  # Should not have internal temporary columns
  expect_false("year" %in% names(result))
  expect_false("month" %in% names(result))
  expect_false("month_pos" %in% names(result))
})

test_that("determine_scale returns correct factors", {
  # Large values (> 100000) -> base units (scale = 1)
  expect_equal(determine_scale(c(200000000, 201000000)), 1)
  expect_equal(determine_scale(c(200000, 201000)), 1)  # 200000 > 100000

  # Medium values (100 < x <= 100000) -> thousands
  expect_equal(determine_scale(c(50000, 51000)), 1000)

  # Small values (<= 100) -> millions
  expect_equal(determine_scale(c(50, 51)), 1000000)

  # Handle zero
  expect_equal(determine_scale(c(0, 0)), 1)

  # Handle NA
  expect_equal(determine_scale(c(NA, NA)), 1)
})

test_that("smooth_monthly_aggregates produces reasonable output", {
  # Create 24 months of data with artificial quarterly pattern
  set.seed(42)
  test_data <- data.table::data.table(
    ref_month_yyyymm = c(
      201301L, 201302L, 201303L, 201304L, 201305L, 201306L,
      201307L, 201308L, 201309L, 201310L, 201311L, 201312L,
      201401L, 201402L, 201403L, 201404L, 201405L, 201406L,
      201407L, 201408L, 201409L, 201410L, 201411L, 201412L
    ),
    z_populacao = 200000 + seq(0, 2300, by = 100) + rnorm(24, 0, 50)
  )

  result <- smooth_monthly_aggregates(test_data,
                                       calibration_start = 201301L,
                                       calibration_end = 201312L)

  # Output should have same number of rows
  expect_equal(nrow(result), 24)

  # Values should be in reasonable range
  expect_true(all(result$m_populacao > 150000, na.rm = TRUE))
  expect_true(all(result$m_populacao < 250000, na.rm = TRUE))
})

test_that("smooth_single_variable handles edge cases", {
  # Very short series (minimum 3 rows)
  short_data <- data.table::data.table(
    ref_month_yyyymm = c(202301L, 202302L, 202303L),
    z_test = c(100, 101, 102),
    year = c(2023L, 2023L, 2023L),
    month = c(1L, 2L, 3L),
    month_pos = c(1L, 2L, 3L)
  )

  result <- smooth_single_variable(short_data, "z_test", "m_test", 202301L, 202303L)

  expect_true("m_test" %in% names(result))
  expect_equal(length(result$m_test), 3)
})
