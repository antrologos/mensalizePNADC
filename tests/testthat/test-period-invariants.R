# Tests for IBGE Period Calendar Invariants
#
# These tests verify that period identification functions NEVER produce
# invalid IBGE calendar values. All invariants must hold by construction.
#
# IBGE Calendar Invariants:
# - Weeks per month: Always exactly 4
# - Fortnights per month: Always exactly 2
# - Weeks per quarter: Always exactly 12
# - Fortnights per quarter: Always exactly 6
# - Fortnight 1 composition: Weeks 1 + 2 of the month
# - Fortnight 2 composition: Weeks 3 + 4 of the month
# - Week uniqueness: No week belongs to multiple months
# - Fortnight uniqueness: No fortnight spans multiple months
# - Valid ranges: ref_week_in_quarter in [1,12], ref_fortnight_in_quarter in [1,6],
#                 ref_month_in_quarter in [1,3]

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Create test crosswalk with specific invariant violations for testing
#' @param violation_type Type of violation to introduce
create_crosswalk_with_violation <- function(violation_type = c(
  "invalid_week", "invalid_fortnight", "invalid_month",
  "week_without_fortnight", "fortnight_without_month",
  "week_fortnight_mismatch", "fortnight_month_mismatch",
  "week_month_mismatch"
)) {
  violation_type <- match.arg(violation_type)

  # Create a minimal valid crosswalk
  crosswalk <- data.table::data.table(
    Ano = rep(2023L, 10),
    Trimestre = rep(1L, 10),
    UPA = 1:10,
    V1008 = rep(1L, 10),
    V1014 = rep(1L, 10),
    ref_month_in_quarter = c(1L, 1L, 2L, 2L, 3L, 3L, NA_integer_, NA_integer_, 1L, 2L),
    ref_fortnight_in_quarter = c(1L, 2L, 3L, 4L, 5L, 6L, NA_integer_, NA_integer_, NA_integer_, 4L),
    ref_week_in_quarter = c(1L, 4L, 5L, 8L, 9L, 12L, NA_integer_, NA_integer_, NA_integer_, NA_integer_)
  )

  switch(violation_type,
    "invalid_week" = {
      crosswalk[1, ref_week_in_quarter := 13L]  # Invalid: > 12
    },
    "invalid_fortnight" = {
      crosswalk[1, ref_fortnight_in_quarter := 7L]  # Invalid: > 6
    },
    "invalid_month" = {
      crosswalk[1, ref_month_in_quarter := 4L]  # Invalid: > 3
    },
    "week_without_fortnight" = {
      crosswalk[1, ref_fortnight_in_quarter := NA_integer_]  # Nesting violation
    },
    "fortnight_without_month" = {
      crosswalk[1, ref_month_in_quarter := NA_integer_]  # Nesting violation
    },
    "week_fortnight_mismatch" = {
      # Week 1-2 should be in fortnight 1, but we put week 1 in fortnight 2
      crosswalk[1, `:=`(ref_week_in_quarter = 1L, ref_fortnight_in_quarter = 2L)]
    },
    "fortnight_month_mismatch" = {
      # Fortnight 1-2 should be in month 1, but we put fortnight 1 in month 2
      crosswalk[1, `:=`(ref_fortnight_in_quarter = 1L, ref_month_in_quarter = 2L)]
    },
    "week_month_mismatch" = {
      # Week 1-4 should be in month 1, but we put week 1 in month 2
      crosswalk[1, `:=`(ref_week_in_quarter = 1L, ref_month_in_quarter = 2L)]
    }
  )

  crosswalk
}

# =============================================================================
# VALIDATION FUNCTION TESTS
# =============================================================================

test_that("validate_period_invariants detects invalid week values", {
  crosswalk <- create_crosswalk_with_violation("invalid_week")

  # Should fail with strict=TRUE
  expect_error(
    validate_period_invariants(crosswalk, strict = TRUE),
    regexp = "ref_week_in_quarter must be 1-12"
  )

  # Should return violations with strict=FALSE
  result <- validate_period_invariants(crosswalk, strict = FALSE)
  expect_false(result$valid)
  expect_true("invalid_week_values" %in% names(result$violations))
})

test_that("validate_period_invariants detects invalid fortnight values", {
  crosswalk <- create_crosswalk_with_violation("invalid_fortnight")

  expect_error(
    validate_period_invariants(crosswalk, strict = TRUE),
    regexp = "ref_fortnight_in_quarter must be 1-6"
  )

  result <- validate_period_invariants(crosswalk, strict = FALSE)
  expect_false(result$valid)
  expect_true("invalid_fortnight_values" %in% names(result$violations))
})

test_that("validate_period_invariants detects invalid month values", {
  crosswalk <- create_crosswalk_with_violation("invalid_month")

  expect_error(
    validate_period_invariants(crosswalk, strict = TRUE),
    regexp = "ref_month_in_quarter must be 1-3"
  )

  result <- validate_period_invariants(crosswalk, strict = FALSE)
  expect_false(result$valid)
  expect_true("invalid_month_values" %in% names(result$violations))
})

test_that("validate_period_invariants detects week without fortnight", {
  crosswalk <- create_crosswalk_with_violation("week_without_fortnight")

  expect_error(
    validate_period_invariants(crosswalk, strict = TRUE),
    regexp = "Nesting violation.*week determined but fortnight is NA"
  )

  result <- validate_period_invariants(crosswalk, strict = FALSE)
  expect_false(result$valid)
  expect_true("week_without_fortnight" %in% names(result$violations))
})

test_that("validate_period_invariants detects fortnight without month", {
  crosswalk <- create_crosswalk_with_violation("fortnight_without_month")

  expect_error(
    validate_period_invariants(crosswalk, strict = TRUE),
    regexp = "Nesting violation.*fortnight determined but month is NA"
  )

  result <- validate_period_invariants(crosswalk, strict = FALSE)
  expect_false(result$valid)
  expect_true("fortnight_without_month" %in% names(result$violations))
})

test_that("validate_period_invariants detects week-fortnight mismatch", {
  crosswalk <- create_crosswalk_with_violation("week_fortnight_mismatch")

  expect_error(
    validate_period_invariants(crosswalk, strict = TRUE),
    regexp = "Week-fortnight inconsistency"
  )

  result <- validate_period_invariants(crosswalk, strict = FALSE)
  expect_false(result$valid)
  expect_true("week_fortnight_mismatch" %in% names(result$violations))
})

test_that("validate_period_invariants detects fortnight-month mismatch", {
  crosswalk <- create_crosswalk_with_violation("fortnight_month_mismatch")

  expect_error(
    validate_period_invariants(crosswalk, strict = TRUE),
    regexp = "Fortnight-month inconsistency"
  )

  result <- validate_period_invariants(crosswalk, strict = FALSE)
  expect_false(result$valid)
  expect_true("fortnight_month_mismatch" %in% names(result$violations))
})

test_that("validate_period_invariants detects week-month mismatch", {
  crosswalk <- create_crosswalk_with_violation("week_month_mismatch")

  expect_error(
    validate_period_invariants(crosswalk, strict = TRUE),
    regexp = "Week-month inconsistency"
  )

  result <- validate_period_invariants(crosswalk, strict = FALSE)
  expect_false(result$valid)
  expect_true("week_month_mismatch" %in% names(result$violations))
})

test_that("validate_period_invariants passes for valid crosswalk", {
  # Create a valid crosswalk
  crosswalk <- data.table::data.table(
    Ano = rep(2023L, 6),
    Trimestre = rep(1L, 6),
    UPA = 1:6,
    V1008 = rep(1L, 6),
    V1014 = rep(1L, 6),
    ref_month_in_quarter = c(1L, 1L, 2L, 2L, 3L, NA_integer_),
    ref_fortnight_in_quarter = c(1L, 2L, 3L, 4L, NA_integer_, NA_integer_),
    ref_week_in_quarter = c(1L, 4L, 5L, NA_integer_, NA_integer_, NA_integer_)
  )

  # Should pass without error
  expect_silent(validate_period_invariants(crosswalk, strict = TRUE))

  result <- validate_period_invariants(crosswalk, strict = FALSE)
  expect_true(result$valid)
  expect_length(result$violations, 0)
})

# =============================================================================
# STRICT IDENTIFICATION INVARIANT TESTS
# =============================================================================

test_that("strict identification never produces invalid week values", {
  test_data <- create_stacked_pnadc(n_quarters = 4, n_upas = 15)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Check ref_week_in_quarter is always 1-12 or NA
  invalid_weeks <- result[!is.na(ref_week_in_quarter) &
                           (ref_week_in_quarter < 1L | ref_week_in_quarter > 12L)]
  expect_equal(nrow(invalid_weeks), 0L,
               info = paste("Found", nrow(invalid_weeks), "invalid week values"))
})

test_that("strict identification never produces invalid fortnight values", {
  test_data <- create_stacked_pnadc(n_quarters = 4, n_upas = 15)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Check ref_fortnight_in_quarter is always 1-6 or NA
  invalid_fortnights <- result[!is.na(ref_fortnight_in_quarter) &
                                 (ref_fortnight_in_quarter < 1L | ref_fortnight_in_quarter > 6L)]
  expect_equal(nrow(invalid_fortnights), 0L,
               info = paste("Found", nrow(invalid_fortnights), "invalid fortnight values"))
})

test_that("strict identification never produces invalid month values", {
  test_data <- create_stacked_pnadc(n_quarters = 4, n_upas = 15)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Check ref_month_in_quarter is always 1-3 or NA
  invalid_months <- result[!is.na(ref_month_in_quarter) &
                            (ref_month_in_quarter < 1L | ref_month_in_quarter > 3L)]
  expect_equal(nrow(invalid_months), 0L,
               info = paste("Found", nrow(invalid_months), "invalid month values"))
})

test_that("strict identification maintains week-fortnight consistency", {
  test_data <- create_stacked_pnadc(n_quarters = 4, n_upas = 15)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Check: week 1-2 -> fortnight 1, week 3-4 -> fortnight 2, etc.
  # Expected fortnight = ((week - 1) %/% 2) + 1
  inconsistent <- result[
    !is.na(ref_week_in_quarter) & !is.na(ref_fortnight_in_quarter) &
      (((ref_week_in_quarter - 1L) %/% 2L) + 1L) != ref_fortnight_in_quarter
  ]
  expect_equal(nrow(inconsistent), 0L,
               info = paste("Found", nrow(inconsistent), "week-fortnight inconsistencies"))
})

test_that("strict identification maintains fortnight-month consistency", {
  test_data <- create_stacked_pnadc(n_quarters = 4, n_upas = 15)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Check: fortnight 1-2 -> month 1, fortnight 3-4 -> month 2, fortnight 5-6 -> month 3
  # Expected month = ((fortnight - 1) %/% 2) + 1
  inconsistent <- result[
    !is.na(ref_fortnight_in_quarter) & !is.na(ref_month_in_quarter) &
      (((ref_fortnight_in_quarter - 1L) %/% 2L) + 1L) != ref_month_in_quarter
  ]
  expect_equal(nrow(inconsistent), 0L,
               info = paste("Found", nrow(inconsistent), "fortnight-month inconsistencies"))
})

test_that("strict identification maintains week-month consistency", {
  test_data <- create_stacked_pnadc(n_quarters = 4, n_upas = 15)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Check: weeks 1-4 -> month 1, weeks 5-8 -> month 2, weeks 9-12 -> month 3
  # Expected month = ((week - 1) %/% 4) + 1
  inconsistent <- result[
    !is.na(ref_week_in_quarter) & !is.na(ref_month_in_quarter) &
      (((ref_week_in_quarter - 1L) %/% 4L) + 1L) != ref_month_in_quarter
  ]
  expect_equal(nrow(inconsistent), 0L,
               info = paste("Found", nrow(inconsistent), "week-month inconsistencies"))
})

# =============================================================================
# EXPERIMENTAL STRATEGIES INVARIANT TESTS
# =============================================================================

test_that("experimental strategies never produce invalid week values", {
  test_data <- create_stacked_pnadc(n_quarters = 4, n_upas = 15)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE, store_date_bounds = TRUE)

  # Test probabilistic strategy
  result_prob <- pnadc_experimental_periods(
    crosswalk, test_data,
    strategy = "probabilistic",
    confidence_threshold = 0.9,
    verbose = FALSE
  )

  # Check ref_week_exp is always 1-12 or NA
  invalid_weeks <- result_prob[!is.na(ref_week_exp) &
                                 (ref_week_exp < 1L | ref_week_exp > 12L)]
  expect_equal(nrow(invalid_weeks), 0L,
               info = paste("Probabilistic strategy produced", nrow(invalid_weeks), "invalid week values"))

  # Test UPA aggregation strategy
  result_upa <- pnadc_experimental_periods(
    crosswalk,
    strategy = "upa_aggregation",
    upa_proportion_threshold = 0.5,
    verbose = FALSE
  )

  invalid_weeks_upa <- result_upa[!is.na(ref_week_exp) &
                                    (ref_week_exp < 1L | ref_week_exp > 12L)]
  expect_equal(nrow(invalid_weeks_upa), 0L,
               info = paste("UPA aggregation produced", nrow(invalid_weeks_upa), "invalid week values"))
})

test_that("experimental strategies never produce invalid fortnight values", {
  test_data <- create_stacked_pnadc(n_quarters = 4, n_upas = 15)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE, store_date_bounds = TRUE)

  # Test probabilistic strategy
  result_prob <- pnadc_experimental_periods(
    crosswalk, test_data,
    strategy = "probabilistic",
    confidence_threshold = 0.9,
    verbose = FALSE
  )

  # Check ref_fortnight_exp is always 1-6 or NA
  invalid_fortnights <- result_prob[!is.na(ref_fortnight_exp) &
                                      (ref_fortnight_exp < 1L | ref_fortnight_exp > 6L)]
  expect_equal(nrow(invalid_fortnights), 0L,
               info = paste("Probabilistic strategy produced", nrow(invalid_fortnights),
                           "invalid fortnight values"))

  # Test UPA aggregation strategy
  result_upa <- pnadc_experimental_periods(
    crosswalk,
    strategy = "upa_aggregation",
    upa_proportion_threshold = 0.5,
    verbose = FALSE
  )

  invalid_fortnights_upa <- result_upa[!is.na(ref_fortnight_exp) &
                                         (ref_fortnight_exp < 1L | ref_fortnight_exp > 6L)]
  expect_equal(nrow(invalid_fortnights_upa), 0L,
               info = paste("UPA aggregation produced", nrow(invalid_fortnights_upa),
                           "invalid fortnight values"))
})

test_that("experimental strategies never produce invalid month values", {
  test_data <- create_stacked_pnadc(n_quarters = 4, n_upas = 15)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE, store_date_bounds = TRUE)

  # Test probabilistic strategy
  result_prob <- pnadc_experimental_periods(
    crosswalk, test_data,
    strategy = "probabilistic",
    confidence_threshold = 0.9,
    verbose = FALSE
  )

  # Check ref_month_exp is always 1-3 or NA
  invalid_months <- result_prob[!is.na(ref_month_exp) &
                                  (ref_month_exp < 1L | ref_month_exp > 3L)]
  expect_equal(nrow(invalid_months), 0L,
               info = paste("Probabilistic strategy produced", nrow(invalid_months),
                           "invalid month values"))

  # Test UPA aggregation strategy
  result_upa <- pnadc_experimental_periods(
    crosswalk,
    strategy = "upa_aggregation",
    upa_proportion_threshold = 0.5,
    verbose = FALSE
  )

  invalid_months_upa <- result_upa[!is.na(ref_month_exp) &
                                     (ref_month_exp < 1L | ref_month_exp > 3L)]
  expect_equal(nrow(invalid_months_upa), 0L,
               info = paste("UPA aggregation produced", nrow(invalid_months_upa),
                           "invalid month values"))
})

test_that("combined strategy maintains all invariants", {
  test_data <- create_stacked_pnadc(n_quarters = 4, n_upas = 15)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE, store_date_bounds = TRUE)

  # Test combined "both" strategy
  result <- pnadc_experimental_periods(
    crosswalk, test_data,
    strategy = "both",
    confidence_threshold = 0.9,
    upa_proportion_threshold = 0.5,
    verbose = FALSE
  )

  # Check all ranges
  expect_equal(
    nrow(result[!is.na(ref_month_exp) & (ref_month_exp < 1L | ref_month_exp > 3L)]),
    0L,
    info = "Invalid month values in combined strategy"
  )
  expect_equal(
    nrow(result[!is.na(ref_fortnight_exp) & (ref_fortnight_exp < 1L | ref_fortnight_exp > 6L)]),
    0L,
    info = "Invalid fortnight values in combined strategy"
  )
  expect_equal(
    nrow(result[!is.na(ref_week_exp) & (ref_week_exp < 1L | ref_week_exp > 12L)]),
    0L,
    info = "Invalid week values in combined strategy"
  )
})

# =============================================================================
# EDGE CASE TESTS
# =============================================================================

test_that("invariants hold for single-quarter data", {
  test_data <- create_stacked_pnadc(n_quarters = 1, n_upas = 10)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # All invariants should still hold
  result_check <- validate_period_invariants(result, strict = FALSE)
  expect_true(result_check$valid,
              info = paste("Violations found:",
                          paste(names(result_check$violations), collapse = ", ")))
})

test_that("invariants hold for many-quarter data", {
  test_data <- create_stacked_pnadc(n_quarters = 12, n_upas = 10)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # All invariants should still hold
  result_check <- validate_period_invariants(result, strict = FALSE)
  expect_true(result_check$valid,
              info = paste("Violations found:",
                          paste(names(result_check$violations), collapse = ", ")))
})

test_that("invariants hold per quarter", {
  test_data <- create_stacked_pnadc(n_quarters = 8, n_upas = 10)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Check invariants per quarter
  for (q in unique(result$Trimestre)) {
    quarter_data <- result[Trimestre == q]
    result_check <- validate_period_invariants(quarter_data, strict = FALSE)
    expect_true(result_check$valid,
                info = paste("Quarter", q, "violations:",
                            paste(names(result_check$violations), collapse = ", ")))
  }
})
