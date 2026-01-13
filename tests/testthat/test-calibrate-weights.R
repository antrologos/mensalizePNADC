# Tests for calibrate_monthly_weights()

# Helper function to create valid test data with reference months
create_test_data_with_ref_month <- function(n = 100) {
  set.seed(42)

  # Create test data for Q1 2023 with all required columns
  data.table::data.table(
    Ano = 2023L,
    Trimestre = 1L,
    UPA = rep(1:10, each = n/10),
    V1008 = rep(1:2, n/2),
    V1014 = rep(1:5, each = n/5),
    V2003 = 1:n,
    V2008 = sample(1:28, n, replace = TRUE),
    V20081 = sample(1:12, n, replace = TRUE),
    V20082 = sample(1970:2005, n, replace = TRUE),
    V2009 = sample(0:80, n, replace = TRUE),
    V1028 = runif(n, 500, 2000),
    UF = sample(11:53, n, replace = TRUE),
    posest = sample(1:1000, n, replace = TRUE),
    posest_sxi = sample(10000:90000, n, replace = TRUE),
    # Pre-computed reference month columns (simulating output from identify_reference_month)
    ref_month = as.Date("2023-01-15") + sample(0:59, n, replace = TRUE),
    ref_month_in_quarter = sample(1:3, n, replace = TRUE),
    ref_month_yyyymm = sample(c(202301L, 202302L, 202303L), n, replace = TRUE)
  )
}

# Helper to create monthly totals
create_monthly_totals <- function() {
  data.table::data.table(
    ref_month_yyyymm = c(202301L, 202302L, 202303L),
    m_populacao = c(214000, 214100, 214200)  # In thousands
  )
}

test_that("calibrate_monthly_weights validates n_cells parameter", {
  test_data <- create_test_data_with_ref_month()
  monthly_totals <- create_monthly_totals()

  expect_error(calibrate_monthly_weights(test_data, monthly_totals, n_cells = 0))
  expect_error(calibrate_monthly_weights(test_data, monthly_totals, n_cells = 5))
  expect_error(calibrate_monthly_weights(test_data, monthly_totals, n_cells = "two"))
})

test_that("calibrate_monthly_weights validates monthly_totals", {
  test_data <- create_test_data_with_ref_month()

  # Missing required columns
  bad_totals <- data.frame(x = 1:3)
  expect_error(calibrate_monthly_weights(test_data, bad_totals))
})

test_that("calibrate_monthly_weights returns weight_calibrated column", {
  test_data <- create_test_data_with_ref_month()
  monthly_totals <- create_monthly_totals()

  result <- calibrate_monthly_weights(test_data, monthly_totals, verbose = FALSE)

  expect_true("weight_calibrated" %in% names(result))
  expect_type(result$weight_calibrated, "double")
})

test_that("calibrate_monthly_weights creates cell columns", {
  test_data <- create_test_data_with_ref_month()
  monthly_totals <- create_monthly_totals()

  result <- calibrate_monthly_weights(test_data, monthly_totals, n_cells = 4, verbose = FALSE)

  expect_true("celula1" %in% names(result))
  expect_true("celula2" %in% names(result))
  expect_true("celula3" %in% names(result))
  expect_true("celula4" %in% names(result))
})

test_that("calibrate_monthly_weights respects n_cells parameter", {
  test_data <- create_test_data_with_ref_month()
  monthly_totals <- create_monthly_totals()

  result1 <- calibrate_monthly_weights(test_data, monthly_totals, n_cells = 1, verbose = FALSE)
  result4 <- calibrate_monthly_weights(test_data, monthly_totals, n_cells = 4, verbose = FALSE)

  # Both should produce weight_calibrated
  expect_true("weight_calibrated" %in% names(result1))
  expect_true("weight_calibrated" %in% names(result4))

  # With more cells, we expect different calibration results
  # (not strictly testing the values, just that the algorithm runs)
  expect_true(all(!is.na(result1$weight_calibrated[!is.na(result1$ref_month_in_quarter)])))
})

test_that("calibrate_monthly_weights handles keep_all = TRUE", {
  test_data <- create_test_data_with_ref_month()
  # Add some indeterminate observations
  test_data[1:10, ref_month_in_quarter := NA]
  monthly_totals <- create_monthly_totals()

  result <- calibrate_monthly_weights(test_data, monthly_totals, keep_all = TRUE, verbose = FALSE)

  # Should include all rows
  expect_equal(nrow(result), nrow(test_data))

  # Indeterminate rows should have NA weights
  expect_true(all(is.na(result$weight_calibrated[is.na(result$ref_month_in_quarter)])))
})

test_that("calibrate_monthly_weights handles keep_all = FALSE", {
  test_data <- create_test_data_with_ref_month()
  # Add some indeterminate observations
  n_indet <- 10
  test_data[1:n_indet, ref_month_in_quarter := NA]
  monthly_totals <- create_monthly_totals()

  result <- calibrate_monthly_weights(test_data, monthly_totals, keep_all = FALSE, verbose = FALSE)

  # Should exclude indeterminate rows
  expect_equal(nrow(result), nrow(test_data) - n_indet)
  expect_true(all(!is.na(result$ref_month_in_quarter)))
})

test_that("calibrate_monthly_weights handles all indeterminate data", {
  test_data <- create_test_data_with_ref_month()
  test_data[, ref_month_in_quarter := NA]
  monthly_totals <- create_monthly_totals()

  expect_warning(
    result <- calibrate_monthly_weights(test_data, monthly_totals, keep_all = TRUE, verbose = FALSE),
    "No observations with determined reference month"
  )
})

test_that("calibrate_monthly_weights accepts anomesexato column name", {
  test_data <- create_test_data_with_ref_month()
  monthly_totals <- data.table::data.table(
    anomesexato = c(202301L, 202302L, 202303L),
    m_populacao = c(214000, 214100, 214200)
  )

  result <- calibrate_monthly_weights(test_data, monthly_totals, verbose = FALSE)
  expect_true("weight_calibrated" %in% names(result))
})

test_that("create_calibration_cells creates correct age groups", {
  dt <- data.table::data.table(
    V2009 = c(5, 15, 35, 65),
    posest_sxi = c(10100, 10100, 10100, 10100),
    UF = c(11, 11, 11, 11),
    posest = c(100, 100, 100, 100)
  )

  result <- create_calibration_cells(dt)

  # celula1 age groups: 0=0-13, 1=14-29, 2=30-59, 3=60+
  expect_equal(result$celula1, c(0L, 1L, 2L, 3L))
})

test_that("create_calibration_cells handles character columns", {
  dt <- data.table::data.table(
    V2009 = as.character(c(5, 15, 35, 65)),  # Character age
    posest_sxi = as.character(c(10100, 10100, 10100, 10100)),
    UF = as.character(c(11, 11, 11, 11)),
    posest = as.character(c(100, 100, 100, 100))
  )

  result <- create_calibration_cells(dt)

  # Should convert and calculate correctly
  expect_equal(result$celula1, c(0L, 1L, 2L, 3L))
})
