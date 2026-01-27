# Tests for nesting enforcement in period identification
#
# These tests verify the fundamental guarantee of the nested algorithm:
# - Fortnights can ONLY be determined for observations with determined months
# - Weeks can ONLY be determined for observations with determined fortnights
#
# This nesting is enforced BY CONSTRUCTION in the algorithm, not by post-hoc cleanup.

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Create test PNADC data with varying birthday patterns
#' @param n_quarters Number of quarters to generate
#' @param n_upas Number of UPAs per quarter
#' @param persons_per_household Number of persons per household
create_test_pnadc_for_nesting <- function(n_quarters = 4, n_upas = 10, persons_per_household = 3) {
  set.seed(42)

  # Generate valid year-quarter combinations (quarters must be 1-4)
  years <- 2023L + ((1:n_quarters - 1L) %/% 4L)
  qtrs <- ((1:n_quarters - 1L) %% 4L) + 1L
  quarters <- data.frame(Ano = years, Trimestre = qtrs)

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

# =============================================================================
# NESTING ENFORCEMENT TESTS
# =============================================================================

test_that("fortnight determination requires month determination", {
  # This is the KEY test: no observation should have determined_fortnight = TRUE

  # while determined_month = FALSE
  test_data <- create_test_pnadc_for_nesting(n_quarters = 4, n_upas = 10)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Count violations
  n_violations <- sum(result$determined_fortnight & !result$determined_month, na.rm = TRUE)

  expect_equal(
    n_violations, 0L,
    info = paste(
      "Found", n_violations, "observations with determined fortnight but undetermined month.",
      "This violates the nesting requirement."
    )
  )
})

test_that("week determination requires fortnight determination", {
  # No observation should have determined_week = TRUE while determined_fortnight = FALSE
  test_data <- create_test_pnadc_for_nesting(n_quarters = 4, n_upas = 10)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Count violations
  n_violations <- sum(result$determined_week & !result$determined_fortnight, na.rm = TRUE)

  expect_equal(
    n_violations, 0L,
    info = paste(
      "Found", n_violations, "observations with determined week but undetermined fortnight.",
      "This violates the nesting requirement."
    )
  )
})

test_that("week determination implies month determination (transitive)", {
  # By transitivity: determined_week => determined_fortnight => determined_month
  test_data <- create_test_pnadc_for_nesting(n_quarters = 4, n_upas = 10)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Count violations
  n_violations <- sum(result$determined_week & !result$determined_month, na.rm = TRUE)

  expect_equal(
    n_violations, 0L,
    info = paste(
      "Found", n_violations, "observations with determined week but undetermined month.",
      "This violates the transitive nesting requirement."
    )
  )
})

# =============================================================================
# NESTING CONSISTENCY TESTS
# =============================================================================

test_that("fortnight value is consistent with month value when both determined", {
  # When fortnight is determined, it should fall within the determined month
  # Month 1 = fortnights 1-2, Month 2 = fortnights 3-4, Month 3 = fortnights 5-6
  test_data <- create_test_pnadc_for_nesting(n_quarters = 4, n_upas = 10)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  both_determined <- result[determined_month & determined_fortnight]

  if (nrow(both_determined) > 0) {
    # Calculate expected fortnight range for determined month
    both_determined[, `:=`(
      expected_fortnight_min = (ref_month_in_quarter - 1L) * 2L + 1L,
      expected_fortnight_max = ref_month_in_quarter * 2L
    )]

    # Check that actual fortnight falls within range
    inconsistent <- both_determined[
      ref_fortnight_in_quarter < expected_fortnight_min |
      ref_fortnight_in_quarter > expected_fortnight_max
    ]

    expect_equal(
      nrow(inconsistent), 0L,
      info = paste(
        "Found", nrow(inconsistent), "observations where fortnight is outside determined month range."
      )
    )
  }
})

test_that("week value is consistent with fortnight value when both determined", {
  # When week is determined, it should fall within the determined fortnight
  test_data <- create_test_pnadc_for_nesting(n_quarters = 4, n_upas = 10)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  both_determined <- result[determined_fortnight & determined_week]

  if (nrow(both_determined) > 0) {
    # Week should correspond to dates within the fortnight
    # Fortnight 1 = weeks 1-2, Fortnight 2 = weeks 3-4 of each month
    # This is a weaker check - just verify week exists for fortnight-determined obs
    expect_true(all(!is.na(both_determined$ref_week_in_quarter)))
  }
})

# =============================================================================
# NESTING BY CONSTRUCTION TESTS
# =============================================================================

test_that("determination rates follow nesting hierarchy", {
  # By construction: month_rate >= fortnight_rate >= week_rate
  test_data <- create_test_pnadc_for_nesting(n_quarters = 4, n_upas = 10)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Compute determination rates from result
  n_total <- nrow(result)
  month_rate <- sum(result$determined_month) / n_total
  fortnight_rate <- sum(result$determined_fortnight) / n_total
  week_rate <- sum(result$determined_week) / n_total

  expect_true(
    month_rate >= fortnight_rate,
    info = paste(
      "Month rate", round(month_rate, 4), "should be >= fortnight rate", round(fortnight_rate, 4)
    )
  )

  expect_true(
    fortnight_rate >= week_rate,
    info = paste(
      "Fortnight rate", round(fortnight_rate, 4), "should be >= week rate", round(week_rate, 4)
    )
  )
})

test_that("all determined fortnights have valid month reference", {
  # Every observation with determined fortnight should have valid month columns
  test_data <- create_test_pnadc_for_nesting(n_quarters = 4, n_upas = 10)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  fortnight_det <- result[determined_fortnight == TRUE]

  if (nrow(fortnight_det) > 0) {
    # All should have valid month values
    expect_true(all(!is.na(fortnight_det$ref_month_in_quarter)))
    expect_true(all(!is.na(fortnight_det$ref_month_in_year)))
    expect_true(all(!is.na(fortnight_det$ref_month_yyyymm)))
  }
})

test_that("all determined weeks have valid fortnight reference", {
  # Every observation with determined week should have valid fortnight columns
  test_data <- create_test_pnadc_for_nesting(n_quarters = 4, n_upas = 10)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  week_det <- result[determined_week == TRUE]

  if (nrow(week_det) > 0) {
    # All should have valid fortnight values
    expect_true(all(!is.na(week_det$ref_fortnight_in_quarter)))
    expect_true(all(!is.na(week_det$ref_fortnight_in_month)))
    expect_true(all(!is.na(week_det$ref_fortnight_yyyyff)))
  }
})

# =============================================================================
# EDGE CASE TESTS
# =============================================================================

test_that("single quarter data respects nesting", {
  # Even with single quarter (lower determination rate), nesting should hold
  test_data <- create_test_pnadc_for_nesting(n_quarters = 1, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Fortnight requires month
  n_violations_fm <- sum(result$determined_fortnight & !result$determined_month, na.rm = TRUE)
  expect_equal(n_violations_fm, 0L)

  # Week requires fortnight
  n_violations_wf <- sum(result$determined_week & !result$determined_fortnight, na.rm = TRUE)
  expect_equal(n_violations_wf, 0L)
})

test_that("nesting holds across all quarters when stacked", {
  # Test with more quarters to stress the algorithm
  test_data <- create_test_pnadc_for_nesting(n_quarters = 8, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Check nesting per quarter
  for (q in unique(result$Trimestre)) {
    quarter_data <- result[Trimestre == q]

    n_violations_fm <- sum(quarter_data$determined_fortnight & !quarter_data$determined_month, na.rm = TRUE)
    n_violations_wf <- sum(quarter_data$determined_week & !quarter_data$determined_fortnight, na.rm = TRUE)

    expect_equal(
      n_violations_fm, 0L,
      info = paste("Quarter", q, "has fortnight without month violations")
    )
    expect_equal(
      n_violations_wf, 0L,
      info = paste("Quarter", q, "has week without fortnight violations")
    )
  }
})

# =============================================================================
# EXPERIMENTAL STRATEGIES NESTING TESTS
# =============================================================================

test_that("experimental fortnight requires month (strict or experimental)", {
  # When ref_fortnight_exp is assigned, either ref_month_in_quarter or ref_month_exp
  # must exist - the experimental fortnight cannot be assigned without month context
  test_data <- create_test_pnadc_for_nesting(n_quarters = 4, n_upas = 10)

  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)
  # Use upa_aggregation strategy which doesn't require original data
  result <- pnadc_experimental_periods(crosswalk, strategy = "upa_aggregation", verbose = FALSE)

  # Check that experimental fortnight requires either strict or experimental month
  has_exp_fortnight <- !is.na(result$ref_fortnight_exp)
  has_any_month <- !is.na(result$ref_month_in_quarter) | !is.na(result$ref_month_exp)

  n_violations <- sum(has_exp_fortnight & !has_any_month, na.rm = TRUE)

  expect_equal(
    n_violations, 0L,
    info = paste(
      "Found", n_violations, "observations with experimental fortnight but no month (strict or exp).",
      "This violates experimental nesting."
    )
  )
})

test_that("experimental week requires fortnight (strict or experimental)", {
  # When ref_week_exp is assigned, either ref_fortnight_in_quarter or ref_fortnight_exp
  # must exist - the experimental week cannot be assigned without fortnight context
  test_data <- create_test_pnadc_for_nesting(n_quarters = 4, n_upas = 10)

  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)
  # Use upa_aggregation strategy which doesn't require original data
  result <- pnadc_experimental_periods(crosswalk, strategy = "upa_aggregation", verbose = FALSE)

  # Check that experimental week requires either strict or experimental fortnight
  has_exp_week <- !is.na(result$ref_week_exp)
  has_any_fortnight <- !is.na(result$ref_fortnight_in_quarter) | !is.na(result$ref_fortnight_exp)

  n_violations <- sum(has_exp_week & !has_any_fortnight, na.rm = TRUE)

  expect_equal(
    n_violations, 0L,
    info = paste(
      "Found", n_violations, "observations with experimental week but no fortnight (strict or exp).",
      "This violates experimental nesting."
    )
  )
})

test_that("experimental fortnight is consistent with month bounds", {
  # When experimental fortnight is assigned, it should fall within the month's fortnight range
  # Month 1 = fortnights 1-2, Month 2 = fortnights 3-4, Month 3 = fortnights 5-6
  test_data <- create_test_pnadc_for_nesting(n_quarters = 4, n_upas = 10)

  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)
  # Use upa_aggregation strategy which doesn't require original data
  result <- pnadc_experimental_periods(crosswalk, strategy = "upa_aggregation", verbose = FALSE)

  # Get observations with experimental fortnight
  has_exp_fortnight <- result[!is.na(ref_fortnight_exp)]

  if (nrow(has_exp_fortnight) > 0) {
    # Determine effective month (strict or experimental)
    has_exp_fortnight[, effective_month := fifelse(
      !is.na(ref_month_in_quarter), ref_month_in_quarter, ref_month_exp
    )]

    has_exp_fortnight[!is.na(effective_month), `:=`(
      expected_fortnight_min = (effective_month - 1L) * 2L + 1L,
      expected_fortnight_max = effective_month * 2L
    )]

    # Check that experimental fortnight falls within expected range
    inconsistent <- has_exp_fortnight[
      !is.na(expected_fortnight_min) &
      (ref_fortnight_exp < expected_fortnight_min | ref_fortnight_exp > expected_fortnight_max)
    ]

    expect_equal(
      nrow(inconsistent), 0L,
      info = paste(
        "Found", nrow(inconsistent), "observations where experimental fortnight is outside month range."
      )
    )
  }
})

test_that("experimental strategies preserve strict determination columns", {
  # The experimental function should NOT modify the strict determination columns
  test_data <- create_test_pnadc_for_nesting(n_quarters = 4, n_upas = 10)

  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Save original strict values
  original_month <- crosswalk$ref_month_in_quarter
  original_fortnight <- crosswalk$ref_fortnight_in_quarter
  original_week <- crosswalk$ref_week_in_quarter

  # Use upa_aggregation strategy which doesn't require original data
  result <- pnadc_experimental_periods(crosswalk, strategy = "upa_aggregation", verbose = FALSE)

  # Strict columns should be unchanged
  expect_identical(result$ref_month_in_quarter, original_month)
  expect_identical(result$ref_fortnight_in_quarter, original_fortnight)
  expect_identical(result$ref_week_in_quarter, original_week)
})

test_that("experimental determination rates follow nesting hierarchy", {
  # Combined rates (strict OR experimental) should follow: month >= fortnight >= week
  test_data <- create_test_pnadc_for_nesting(n_quarters = 4, n_upas = 10)

  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)
  # Use upa_aggregation strategy which doesn't require original data
  result <- pnadc_experimental_periods(crosswalk, strategy = "upa_aggregation", verbose = FALSE)

  # Calculate combined rates
  combined_month_rate <- mean(!is.na(result$ref_month_in_quarter) | !is.na(result$ref_month_exp))
  combined_fortnight_rate <- mean(!is.na(result$ref_fortnight_in_quarter) | !is.na(result$ref_fortnight_exp))
  combined_week_rate <- mean(!is.na(result$ref_week_in_quarter) | !is.na(result$ref_week_exp))

  expect_true(
    combined_month_rate >= combined_fortnight_rate,
    info = paste(
      "Combined month rate", round(combined_month_rate, 4),
      "should be >= combined fortnight rate", round(combined_fortnight_rate, 4)
    )
  )

  expect_true(
    combined_fortnight_rate >= combined_week_rate,
    info = paste(
      "Combined fortnight rate", round(combined_fortnight_rate, 4),
      "should be >= combined week rate", round(combined_week_rate, 4)
    )
  )
})
