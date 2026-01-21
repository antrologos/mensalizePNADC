# Tests for validation utility functions

test_that("required_vars_ref_month returns expected columns", {
  vars <- required_vars_ref_month()
  expect_type(vars, "character")
  expect_true("Ano" %in% vars)
  expect_true("Trimestre" %in% vars)
  expect_true("UPA" %in% vars)
  expect_true("V2008" %in% vars)  # Birth day
  expect_true("V20081" %in% vars)  # Birth month
  expect_true("V20082" %in% vars)  # Birth year
  expect_true("V2009" %in% vars)   # Age
})

test_that("join_key_vars returns expected columns", {
  vars <- join_key_vars()
  expect_type(vars, "character")
  expect_true("Ano" %in% vars)
  expect_true("Trimestre" %in% vars)
  expect_true("UPA" %in% vars)
  expect_true("V1008" %in% vars)
  expect_true("V1014" %in% vars)
  expect_true("V2003" %in% vars)
})

test_that("required_vars_weights returns expected columns", {
  vars <- required_vars_weights()
  expect_type(vars, "character")
  expect_true("V1028" %in% vars)
  expect_true("UF" %in% vars)
  expect_true("posest" %in% vars)
  expect_true("posest_sxi" %in% vars)
})

test_that("validate_pnadc detects missing columns", {
  # Missing required columns
  bad_data <- data.frame(Ano = 2023, Trimestre = 1)
  expect_error(validate_pnadc(bad_data), "missing_ref_month")
})

test_that("validate_pnadc accepts valid minimal data", {
  valid_data <- data.frame(
    Ano = 2023,
    Trimestre = 1,
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )
  # Should not error
 expect_silent(validate_pnadc(valid_data))
})

test_that("validate_pnadc detects invalid year", {
  bad_year_data <- data.frame(
    Ano = 2000,  # Too early (< 2012)
    Trimestre = 1,
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )
  expect_error(validate_pnadc(bad_year_data), "invalid_years")
})

test_that("validate_pnadc detects invalid quarter", {
  bad_quarter_data <- data.frame(
    Ano = 2023,
    Trimestre = 5,  # Invalid quarter
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )
  expect_error(validate_pnadc(bad_quarter_data), "invalid_quarters")
})

test_that("validate_pnadc detects invalid birth day", {
  bad_day_data <- data.frame(
    Ano = 2023,
    Trimestre = 1,
    UPA = 1,
    V1014 = 1,
    V2008 = 35,  # Invalid day
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )
  expect_error(validate_pnadc(bad_day_data), "invalid_birth_days")
})

test_that("validate_pnadc detects invalid birth month", {
  bad_month_data <- data.frame(
    Ano = 2023,
    Trimestre = 1,
    UPA = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 15,  # Invalid month
    V20082 = 1990,
    V2009 = 33
  )
  expect_error(validate_pnadc(bad_month_data), "invalid_birth_months")
})

test_that("validate_pnadc returns report when stop_on_error = FALSE", {
  bad_data <- data.frame(Ano = 2023, Trimestre = 1)
  result <- validate_pnadc(bad_data, stop_on_error = FALSE)

  expect_type(result, "list")
  expect_false(result$valid)
  expect_true("missing_ref_month" %in% names(result$issues))
})

test_that("validate_pnadc reports valid data correctly", {
  valid_data <- data.frame(
    Ano = 2023,
    Trimestre = 1,
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )
  result <- validate_pnadc(valid_data, stop_on_error = FALSE)

  expect_type(result, "list")
  expect_true(result$valid)
  expect_equal(result$n_rows, 1)
})

test_that("validate_pnadc checks weight columns when requested", {
  # Valid ref month data but missing weight columns
  data_no_weights <- data.frame(
    Ano = 2023,
    Trimestre = 1,
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )

  # Should pass without check_weights
  expect_silent(validate_pnadc(data_no_weights, check_weights = FALSE))

  # Should fail with check_weights
  expect_error(validate_pnadc(data_no_weights, check_weights = TRUE), "missing_weights")
})

test_that("validate_monthly_totals accepts valid data", {
  valid_totals <- data.frame(
    ref_month_yyyymm = c(202301, 202302, 202303),
    m_populacao = c(200000, 200100, 200200)
  )
  # Should not error
  expect_silent(validate_monthly_totals(valid_totals))
})

test_that("validate_monthly_totals accepts anomesexato column", {
  valid_totals <- data.frame(
    anomesexato = c(202301, 202302, 202303),
    m_populacao = c(200000, 200100, 200200)
  )
  # Should not error
  expect_silent(validate_monthly_totals(valid_totals))
})

test_that("validate_monthly_totals detects missing date column", {
  bad_totals <- data.frame(
    m_populacao = c(200000, 200100, 200200)
  )
  expect_error(validate_monthly_totals(bad_totals), "missing_date")
})

test_that("validate_monthly_totals detects missing population column", {
  bad_totals <- data.frame(
    ref_month_yyyymm = c(202301, 202302, 202303)
  )
  expect_error(validate_monthly_totals(bad_totals), "missing_population")
})

test_that("ensure_data_table converts data.frame", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  result <- ensure_data_table(df)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
})

test_that("ensure_data_table returns data.table unchanged when copy = FALSE", {
  dt <- data.table::data.table(x = 1:3)
  result <- ensure_data_table(dt, copy = FALSE)

  # Should be the same object (not a copy)
  expect_identical(data.table::address(result), data.table::address(dt))
})

test_that("ensure_data_table returns copy when copy = TRUE", {
  dt <- data.table::data.table(x = 1:3)
  result <- ensure_data_table(dt, copy = TRUE)

  # Should be a different object
  expect_false(identical(data.table::address(result), data.table::address(dt)))
  expect_equal(result$x, dt$x)
})
