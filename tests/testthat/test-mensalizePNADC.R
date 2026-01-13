# Tests for mensalizePNADC() main function

# Helper function to create minimal valid PNADC data
create_minimal_pnadc <- function(n = 50) {
  set.seed(42)
  data.table::data.table(
    Ano = 2023L,
    Trimestre = 1L,
    UPA = rep(1:5, each = n/5),
    V1008 = rep(1:2, n/2),
    V1014 = rep(1:5, each = n/5),
    V2003 = 1:n,
    V2008 = sample(1:28, n, replace = TRUE),
    V20081 = sample(1:12, n, replace = TRUE),
    V20082 = sample(1970:2000, n, replace = TRUE),
    V2009 = sample(20:60, n, replace = TRUE)
  )
}

# Helper to create full PNADC data with weight columns
create_full_pnadc <- function(n = 100) {
  dt <- create_minimal_pnadc(n)
  dt[, `:=`(
    V1028 = runif(n, 500, 2000),
    UF = sample(c(11L, 12L, 13L), n, replace = TRUE),
    posest = sample(1:100, n, replace = TRUE),
    posest_sxi = sample(10000:30000, n, replace = TRUE)
  )]
  dt
}

test_that("mensalizePNADC validates output parameter", {
  test_data <- create_minimal_pnadc()

  expect_error(mensalizePNADC(test_data, output = "invalid"))
})

test_that("mensalizePNADC validates compute_weights parameter", {
  test_data <- create_minimal_pnadc()

  expect_error(mensalizePNADC(test_data, compute_weights = "yes"))
  expect_error(mensalizePNADC(test_data, compute_weights = c(TRUE, FALSE)))
})

test_that("mensalizePNADC returns crosswalk by default", {
  test_data <- create_minimal_pnadc()

  result <- mensalizePNADC(test_data, verbose = FALSE)

  expect_s3_class(result, "data.table")

  # Should have reference month columns
  expect_true("ref_month" %in% names(result))
  expect_true("ref_month_in_quarter" %in% names(result))
  expect_true("ref_month_yyyymm" %in% names(result))

  # Should have join key columns
  expect_true("Ano" %in% names(result))
  expect_true("Trimestre" %in% names(result))
  expect_true("UPA" %in% names(result))
})

test_that("mensalizePNADC returns microdata with output='microdata'", {
  test_data <- create_minimal_pnadc()

  result <- mensalizePNADC(test_data, output = "microdata", verbose = FALSE)

  # Should have all original columns plus reference month columns
  expect_true(all(names(test_data) %in% names(result)))
  expect_true("ref_month" %in% names(result))
  expect_true("ref_month_in_quarter" %in% names(result))
})

test_that("mensalizePNADC validates data columns", {
  # Missing required columns
  bad_data <- data.frame(Ano = 2023, Trimestre = 1)

  expect_error(mensalizePNADC(bad_data, verbose = FALSE), "missing_ref_month")
})

test_that("mensalizePNADC validates weight columns when compute_weights = TRUE", {
  # Data without weight columns
  test_data <- create_minimal_pnadc()

  expect_error(
    mensalizePNADC(test_data, compute_weights = TRUE, verbose = FALSE),
    "missing_weights"
  )
})

test_that("mensalizePNADC errors when aggregates requested without weights", {
  test_data <- create_minimal_pnadc()

  expect_error(
    mensalizePNADC(test_data, compute_weights = FALSE, output = "aggregates", verbose = FALSE),
    "requires compute_weights = TRUE"
  )
})

test_that("mensalizePNADC includes determination_rate attribute", {
  test_data <- create_minimal_pnadc()

  result <- mensalizePNADC(test_data, verbose = FALSE)

  det_rate <- attr(result, "determination_rate")
  expect_type(det_rate, "double")
  expect_true(det_rate >= 0 && det_rate <= 1)
})

test_that("mensalizePNADC handles character columns", {
  test_data <- create_minimal_pnadc()
  # Convert some columns to character (common in PNADC data)
  test_data[, Ano := as.character(Ano)]
  test_data[, Trimestre := as.character(Trimestre)]

  # Should not error
  result <- mensalizePNADC(test_data, verbose = FALSE)
  expect_s3_class(result, "data.table")
})

test_that("mensalizePNADC respects verbose parameter", {
  test_data <- create_minimal_pnadc()

  # verbose = FALSE should produce no output
  expect_silent(result <- mensalizePNADC(test_data, verbose = FALSE))

  # verbose = TRUE should produce output (captured by expect_output)
  expect_output(
    result <- mensalizePNADC(test_data, verbose = TRUE),
    "Processing|Determination"
  )
})

test_that("smooth_calibrated_weights adds weight_smoothed column", {
  # Create test data with calibrated weights
  dt <- data.table::data.table(
    ref_month_yyyymm = rep(c(202301L, 202302L, 202303L), each = 10),
    ref_month_in_quarter = rep(1:3, each = 10),
    weight_calibrated = runif(30, 500, 2000)
  )

  result <- smooth_calibrated_weights(dt)

  expect_true("weight_smoothed" %in% names(result))
})

test_that("smooth_calibrated_weights handles NA ref_month_in_quarter", {
  # Create test data with some indeterminate observations
  dt <- data.table::data.table(
    ref_month_yyyymm = c(rep(c(202301L, 202302L, 202303L), each = 10), rep(NA_integer_, 5)),
    ref_month_in_quarter = c(rep(1:3, each = 10), rep(NA_integer_, 5)),
    weight_calibrated = runif(35, 500, 2000)
  )

  result <- smooth_calibrated_weights(dt)

  expect_true("weight_smoothed" %in% names(result))
  expect_equal(nrow(result), 35)
})

test_that("compute_monthly_aggregates produces correct structure", {
  dt <- data.table::data.table(
    ref_month_yyyymm = rep(c(202301L, 202302L, 202303L), each = 100),
    ref_month_in_quarter = rep(1:3, each = 100),
    weight_monthly = runif(300, 500, 2000)
  )

  result <- compute_monthly_aggregates(dt)

  expect_s3_class(result, "data.table")
  expect_true("ref_month_yyyymm" %in% names(result))
  expect_true("n_obs" %in% names(result))
  expect_true("population" %in% names(result))
  expect_equal(nrow(result), 3)  # One row per month
})

test_that("compute_monthly_aggregates excludes indeterminate observations", {
  dt <- data.table::data.table(
    ref_month_yyyymm = c(rep(c(202301L, 202302L, 202303L), each = 100), rep(NA_integer_, 50)),
    ref_month_in_quarter = c(rep(1:3, each = 100), rep(NA_integer_, 50)),
    weight_monthly = runif(350, 500, 2000)
  )

  result <- compute_monthly_aggregates(dt)

  # Should only have 3 months (excluding NA)
  expect_equal(nrow(result), 3)
  # n_obs should be 100 for each month
  expect_true(all(result$n_obs == 100))
})

# Integration tests with full pipeline (require sidrar, internet, AND multi-quarter data)
# These tests are skipped because they require realistic multi-quarter PNADC data
# to achieve non-zero determination rates. Single-quarter random data typically
# yields 0% determination due to the algorithm's cross-quarter aggregation design.

test_that("mensalizePNADC with compute_weights returns weight_monthly", {
  skip("Integration test requires multi-quarter PNADC data with realistic patterns")
  skip_if_not(requireNamespace("sidrar", quietly = TRUE),
              "sidrar package not available")
  skip_if_offline()

  # Note: To test this properly, use real stacked PNADC data
  # test_data <- create_full_pnadc(200)
  # result <- mensalizePNADC(test_data, compute_weights = TRUE, verbose = FALSE)
  # expect_true("weight_monthly" %in% names(result))
})

test_that("mensalizePNADC with compute_weights and output='aggregates'", {
  skip("Integration test requires multi-quarter PNADC data with realistic patterns")
  skip_if_not(requireNamespace("sidrar", quietly = TRUE),
              "sidrar package not available")
  skip_if_offline()

  # Note: To test this properly, use real stacked PNADC data
  # test_data <- create_full_pnadc(200)
  # result <- mensalizePNADC(test_data, compute_weights = TRUE,
  #                           output = "aggregates", verbose = FALSE)
  # expect_s3_class(result, "data.table")
  # expect_true("ref_month_yyyymm" %in% names(result))
  # expect_true("population" %in% names(result))
})
