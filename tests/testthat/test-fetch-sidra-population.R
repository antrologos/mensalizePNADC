# Tests for fetch-sidra-population.R
# Note: Some tests require internet connection and sidrar package

test_that("fetch_monthly_population requires sidrar package", {
  skip_if(requireNamespace("sidrar", quietly = TRUE),
          "sidrar is installed, skipping missing package test")

  # Mock the requireNamespace to return FALSE
  # This test only runs if sidrar is not installed
  expect_error(fetch_monthly_population(), "Package 'sidrar' is required")
})

test_that("fetch_monthly_population returns expected structure", {
  skip_if_not(requireNamespace("sidrar", quietly = TRUE),
              "sidrar package not available")
  skip_if_offline()

  result <- fetch_monthly_population(verbose = FALSE)

  # Should be a data.table
  expect_s3_class(result, "data.table")

  # Should have required columns
  expect_true("ref_month_yyyymm" %in% names(result))
  expect_true("m_populacao" %in% names(result))

  # Should have reasonable values
  expect_true(all(result$ref_month_yyyymm >= 201201))
  expect_true(all(result$m_populacao > 0, na.rm = TRUE))
})

test_that("fetch_monthly_population respects date range", {
  skip_if_not(requireNamespace("sidrar", quietly = TRUE),
              "sidrar package not available")
  skip_if_offline()

  result <- fetch_monthly_population(
    start_yyyymm = 201501,
    end_yyyymm = 201512,
    verbose = FALSE
  )

  # Should only have 2015 months
  expect_true(all(result$ref_month_yyyymm >= 201501))
  expect_true(all(result$ref_month_yyyymm <= 201512))
  expect_equal(nrow(result), 12)
})

test_that("transform_moving_quarter_to_monthly transforms correctly", {
  # Create sample moving quarter data
  dt <- data.table::data.table(
    anomesfinaltrimmovel = c(201203L, 201204L, 201205L, 201206L),
    populacao = c(200000, 200100, 200200, 200300)
  )

  result <- transform_moving_quarter_to_monthly(dt, verbose = FALSE)

  # Should have anomesexato and m_populacao columns
  expect_true("anomesexato" %in% names(result))
  expect_true("m_populacao" %in% names(result))

  # Should have correct number of rows (original + 2 dummy rows)
  expect_equal(nrow(result), 6)

  # The shift logic: month N gets population from row N+1
  # So 201202 (Feb 2012) gets value from 201203 row = 200000
  expect_equal(result[anomesexato == 201202L, m_populacao], 200000)

  # First month (201201) should be NA (needs extrapolation)
  expect_true(is.na(result[anomesexato == 201201L, m_populacao]))
})

test_that("extrapolate_boundary_months fills in boundary values", {
  # Create a series with missing first and last values
  set.seed(42)
  n <- 30
  dt <- data.table::data.table(
    anomesexato = 201201L + 0:(n-1),
    m_populacao = c(NA, 200000 + cumsum(rnorm(n-2, 100, 10)), NA)
  )

  result <- extrapolate_boundary_months(dt)

  # Should fill in first and last values
  expect_false(is.na(result[1, m_populacao]))
  expect_false(is.na(result[n, m_populacao]))

  # Values should be reasonable (close to neighbors)
  expect_true(abs(result[1, m_populacao] - result[2, m_populacao]) < 500)
  expect_true(abs(result[n, m_populacao] - result[n-1, m_populacao]) < 500)
})

test_that("extrapolate_boundary_months handles short series", {
  # Series too short for extrapolation (< 26 rows)
  dt <- data.table::data.table(
    anomesexato = 201201L:201210L,
    m_populacao = c(NA, 200000:200007, NA)
  )

  result <- extrapolate_boundary_months(dt)

  # With only 10 rows, cannot extrapolate using 26-row regression
  # First and last should still be NA
  expect_true(is.na(result[1, m_populacao]))
  expect_true(is.na(result[10, m_populacao]))
})

test_that("extrapolate_boundary_months preserves middle values", {
  set.seed(42)
  n <- 30
  original_middle <- 200000 + cumsum(rnorm(n-2, 100, 10))

  dt <- data.table::data.table(
    anomesexato = 201201L + 0:(n-1),
    m_populacao = c(NA, original_middle, NA)
  )

  result <- extrapolate_boundary_months(dt)

  # Middle values should be unchanged
  expect_equal(result$m_populacao[2:(n-1)], original_middle)
})

test_that("extrapolate_boundary_months removes temporary columns", {
  set.seed(42)
  n <- 30
  dt <- data.table::data.table(
    anomesexato = 201201L + 0:(n-1),
    m_populacao = c(NA, 200000 + cumsum(rnorm(n-2, 100, 10)), NA)
  )

  result <- extrapolate_boundary_months(dt)

  # Should not have regression columns
  expect_false("row_num" %in% names(result))
  expect_false("row_num2" %in% names(result))
  expect_false("d_pop" %in% names(result))
})
