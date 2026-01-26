#' @title Input Validation Utilities
#' @description Internal helper functions for validating PNADC input data.
#' @name utils-validation
#' @keywords internal
NULL

# ============================================================================
# IBGE PERIOD INVARIANT VALIDATION
# ============================================================================

#' Validate IBGE Period Crosswalk Invariants
#'
#' Ensures that period identification functions never produce invalid IBGE
#' calendar values. This function checks all invariants that must hold for
#' valid period assignments.
#'
#' @param crosswalk A data.table crosswalk with period columns
#' @param strict Logical. If TRUE (default), stops with an error on violations.
#'   If FALSE, issues warnings and returns a validation report.
#' @param context Character string describing where validation is being called
#'   from (for error messages). Default is "crosswalk".
#'
#' @return If \code{strict = TRUE}, returns invisibly if valid or stops with error.
#'   If \code{strict = FALSE}, returns a list with:
#'   \itemize{
#'     \item \code{valid}: Logical indicating if all invariants passed
#'     \item \code{violations}: Named list of violations found (empty if none)
#'   }
#'
#' @details
#' The following IBGE calendar invariants are checked:
#' \itemize{
#'   \item \strong{Valid ranges}: ref_week_in_quarter in [1,12] or NA,
#'     ref_fortnight_in_quarter in [1,6] or NA, ref_month_in_quarter in [1,3] or NA
#'   \item \strong{Nesting}: week requires fortnight, fortnight requires month
#'   \item \strong{Fortnight-month consistency}: fortnights 1-2 belong to month 1,
#'     3-4 to month 2, 5-6 to month 3
#'   \item \strong{Week-fortnight consistency}: weeks 1-2 belong to fortnight 1,
#'     3-4 to fortnight 2, etc.
#' }
#'
#' @keywords internal
#' @noRd
validate_period_invariants <- function(crosswalk, strict = TRUE, context = "crosswalk") {

  violations <- list()

  # -------------------------------------------------------------------------
  # INVARIANT 1: Valid ranges
  # -------------------------------------------------------------------------

  # Check ref_month_in_quarter in [1,3] or NA
  if ("ref_month_in_quarter" %in% names(crosswalk)) {
    invalid_months <- crosswalk[!is.na(ref_month_in_quarter) &
                                  (ref_month_in_quarter < 1L | ref_month_in_quarter > 3L)]
    if (nrow(invalid_months) > 0) {
      violations$invalid_month_values <- list(
        count = nrow(invalid_months),
        values = unique(invalid_months$ref_month_in_quarter),
        message = sprintf(
          "ref_month_in_quarter must be 1-3 or NA. Found %d rows with invalid values: %s",
          nrow(invalid_months),
          paste(unique(invalid_months$ref_month_in_quarter), collapse = ", ")
        )
      )
    }
  }

  # Check ref_fortnight_in_quarter in [1,6] or NA
  if ("ref_fortnight_in_quarter" %in% names(crosswalk)) {
    invalid_fortnights <- crosswalk[!is.na(ref_fortnight_in_quarter) &
                                      (ref_fortnight_in_quarter < 1L | ref_fortnight_in_quarter > 6L)]
    if (nrow(invalid_fortnights) > 0) {
      violations$invalid_fortnight_values <- list(
        count = nrow(invalid_fortnights),
        values = unique(invalid_fortnights$ref_fortnight_in_quarter),
        message = sprintf(
          "ref_fortnight_in_quarter must be 1-6 or NA. Found %d rows with invalid values: %s",
          nrow(invalid_fortnights),
          paste(unique(invalid_fortnights$ref_fortnight_in_quarter), collapse = ", ")
        )
      )
    }
  }

  # Check ref_week_in_quarter in [1,12] or NA
  if ("ref_week_in_quarter" %in% names(crosswalk)) {
    invalid_weeks <- crosswalk[!is.na(ref_week_in_quarter) &
                                 (ref_week_in_quarter < 1L | ref_week_in_quarter > 12L)]
    if (nrow(invalid_weeks) > 0) {
      violations$invalid_week_values <- list(
        count = nrow(invalid_weeks),
        values = unique(invalid_weeks$ref_week_in_quarter),
        message = sprintf(
          "ref_week_in_quarter must be 1-12 or NA. Found %d rows with invalid values: %s",
          nrow(invalid_weeks),
          paste(unique(invalid_weeks$ref_week_in_quarter), collapse = ", ")
        )
      )
    }
  }

  # -------------------------------------------------------------------------
  # INVARIANT 2: Nesting - week requires fortnight, fortnight requires month
  # -------------------------------------------------------------------------

  # Check: if week is determined, fortnight must be determined
  if (all(c("ref_week_in_quarter", "ref_fortnight_in_quarter") %in% names(crosswalk))) {
    week_without_fortnight <- crosswalk[!is.na(ref_week_in_quarter) &
                                          is.na(ref_fortnight_in_quarter)]
    if (nrow(week_without_fortnight) > 0) {
      violations$week_without_fortnight <- list(
        count = nrow(week_without_fortnight),
        message = sprintf(
          "Nesting violation: %d rows have week determined but fortnight is NA",
          nrow(week_without_fortnight)
        )
      )
    }
  }

  # Check: if fortnight is determined, month must be determined
  if (all(c("ref_fortnight_in_quarter", "ref_month_in_quarter") %in% names(crosswalk))) {
    fortnight_without_month <- crosswalk[!is.na(ref_fortnight_in_quarter) &
                                           is.na(ref_month_in_quarter)]
    if (nrow(fortnight_without_month) > 0) {
      violations$fortnight_without_month <- list(
        count = nrow(fortnight_without_month),
        message = sprintf(
          "Nesting violation: %d rows have fortnight determined but month is NA",
          nrow(fortnight_without_month)
        )
      )
    }
  }

  # -------------------------------------------------------------------------
  # INVARIANT 3: Fortnight-month consistency
  # Fortnights 1-2 belong to month 1, 3-4 to month 2, 5-6 to month 3
  # -------------------------------------------------------------------------

  if (all(c("ref_fortnight_in_quarter", "ref_month_in_quarter") %in% names(crosswalk))) {
    # Calculate expected month from fortnight: ((fortnight - 1) %/% 2) + 1
    fortnight_month_inconsistent <- crosswalk[
      !is.na(ref_fortnight_in_quarter) & !is.na(ref_month_in_quarter) &
        (((ref_fortnight_in_quarter - 1L) %/% 2L) + 1L) != ref_month_in_quarter
    ]
    if (nrow(fortnight_month_inconsistent) > 0) {
      violations$fortnight_month_mismatch <- list(
        count = nrow(fortnight_month_inconsistent),
        message = sprintf(
          "Fortnight-month inconsistency: %d rows have fortnight that doesn't match month",
          nrow(fortnight_month_inconsistent)
        )
      )
    }
  }

  # -------------------------------------------------------------------------
  # INVARIANT 4: Week-fortnight consistency
  # Weeks 1-2 belong to fortnight 1, 3-4 to fortnight 2, etc.
  # Formula: expected_fortnight = ((week - 1) %/% 2) + 1
  # -------------------------------------------------------------------------

  if (all(c("ref_week_in_quarter", "ref_fortnight_in_quarter") %in% names(crosswalk))) {
    # Calculate expected fortnight from week
    week_fortnight_inconsistent <- crosswalk[
      !is.na(ref_week_in_quarter) & !is.na(ref_fortnight_in_quarter) &
        (((ref_week_in_quarter - 1L) %/% 2L) + 1L) != ref_fortnight_in_quarter
    ]
    if (nrow(week_fortnight_inconsistent) > 0) {
      violations$week_fortnight_mismatch <- list(
        count = nrow(week_fortnight_inconsistent),
        message = sprintf(
          "Week-fortnight inconsistency: %d rows have week that doesn't match fortnight",
          nrow(week_fortnight_inconsistent)
        )
      )
    }
  }

  # -------------------------------------------------------------------------
  # INVARIANT 5: Week-month consistency (derived from above, but direct check)
  # Weeks 1-4 belong to month 1, 5-8 to month 2, 9-12 to month 3
  # -------------------------------------------------------------------------

  if (all(c("ref_week_in_quarter", "ref_month_in_quarter") %in% names(crosswalk))) {
    # Calculate expected month from week: ((week - 1) %/% 4) + 1
    week_month_inconsistent <- crosswalk[
      !is.na(ref_week_in_quarter) & !is.na(ref_month_in_quarter) &
        (((ref_week_in_quarter - 1L) %/% 4L) + 1L) != ref_month_in_quarter
    ]
    if (nrow(week_month_inconsistent) > 0) {
      violations$week_month_mismatch <- list(
        count = nrow(week_month_inconsistent),
        message = sprintf(
          "Week-month inconsistency: %d rows have week that doesn't match month",
          nrow(week_month_inconsistent)
        )
      )
    }
  }

  # -------------------------------------------------------------------------
  # RETURN RESULT
  # -------------------------------------------------------------------------

  result <- list(
    valid = length(violations) == 0,
    violations = violations
  )

  if (strict && !result$valid) {
    # Build error message
    error_msgs <- vapply(violations, function(v) v$message, character(1))
    msg <- paste0(
      "IBGE period invariant violations in ", context, ":\n",
      paste("  - ", error_msgs, collapse = "\n")
    )
    stop(msg, call. = FALSE)
  }

  if (!strict && !result$valid) {
    # Issue warnings
    for (violation in violations) {
      warning(paste0(context, ": ", violation$message), call. = FALSE)
    }
  }

  invisible(result)
}

#' Required Variables for Reference Period Identification
#'
#' Returns the minimum required column names for identifying reference periods
#' (month, fortnight, and week).
#'
#' @return Character vector of required column names
#' @keywords internal
#' @noRd
required_vars_ref_month <- function() {
  # V1008 is required for week identification (household-level aggregation)
  c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2009")
}

#' Required Variables for Crosswalk Join Keys
#'
#' Returns the column names used as join keys in the output crosswalk.
#' The crosswalk is at household-quarter level (not person level),
#' so V2003 (person sequence) is NOT included.
#'
#' @return Character vector of join key column names
#' @keywords internal
#' @noRd
join_key_vars <- function() {
  # Note: Crosswalk is at household level (V1008), not person level (V2003)
  # All persons in a household share the same reference period
  c("Ano", "Trimestre", "UPA", "V1008", "V1014")
}

#' Required Variables for Weight Calibration
#'
#' Returns the additional column names required for computing calibrated weights.
#'
#' Note: Weight calibration operates at **person level** (not household level)
#' because calibration cells are based on individual attributes like age (V2009).
#' Although the crosswalk assigns reference periods at household level (all
#' household members share the same reference period), each person receives
#' a potentially different calibrated weight based on their age group.
#'
#' @return Character vector of required column names
#' @keywords internal
#' @noRd
required_vars_weights <- function() {
  c(
    # Survey design - V1028 (quarterly) or V1032 (annual) weights
    "V1028", "UF", "posest", "posest_sxi"
    # V2009 (age) is already required for ref_month identification
    # and is also used to define calibration cells (person-level)
  )
}

#' Validate PNADC Input Data
#'
#' Checks that input data has required columns for the specified processing.
#'
#' @param data A data.frame or data.table with PNADC microdata
#' @param check_weights Logical. If TRUE, also check for weight-related variables.
#' @param stop_on_error Logical. If TRUE, stops with an error. If FALSE, returns
#'   a validation report list.
#'
#' @return If \code{stop_on_error = TRUE}, returns invisibly if valid or stops with error.
#'   If \code{stop_on_error = FALSE}, returns a list with:
#'   \itemize{
#'     \item \code{valid}: Logical indicating if data passed all validations
#'     \item \code{issues}: Named list of validation issues found (empty if none)
#'     \item \code{n_rows}: Number of rows in input data
#'     \item \code{n_cols}: Number of columns in input data
#'     \item \code{join_keys_available}: Character vector of available join key columns
#'   }
#'
#' @details
#' The function performs the following validations:
#' \itemize{
#'   \item Checks for required columns for reference period identification:
#'     \code{Ano}, \code{Trimestre}, \code{UPA}, \code{V1008}, \code{V1014},
#'     \code{V2008}, \code{V20081}, \code{V20082}, \code{V2009}
#'   \item Validates year range (2012-2100 for PNADC coverage)
#'   \item Validates quarter values (must be 1-4)
#'   \item Validates birth day values (must be 1-31 or 99 for unknown)
#'   \item Validates birth month values (must be 1-12 or 99 for unknown)
#'   \item Warns about unusual ages (outside 0-130 range)
#'   \item If \code{check_weights = TRUE}, also validates weight-related columns:
#'     \code{V1028}, \code{UF}, \code{posest}, \code{posest_sxi}
#' }
#'
#' @seealso \code{\link{pnadc_identify_periods}} which calls this function
#'   internally to validate input data.
#'
#' @examples
#' \dontrun{
#' validate_pnadc(my_data)
#' validate_pnadc(my_data, check_weights = TRUE)
#' }
#'
#' @export
validate_pnadc <- function(data, check_weights = FALSE, stop_on_error = TRUE) {
  checkmate::assert_data_frame(data, min.rows = 1)

  issues <- list()

  # Check required columns for reference month
  required <- PNADCperiods:::required_vars_ref_month()
  missing  <- setdiff(required, names(data))
  if (length(missing) > 0) {
    issues$missing_ref_month <- missing
  }

  # Check weight-related columns if requested
  if (check_weights) {
    required_wt <- PNADCperiods:::required_vars_weights()
    missing_wt  <- setdiff(required_wt, names(data))
    if (length(missing_wt) > 0) {
      issues$missing_weights <- missing_wt
    }
  }

  # Check join key columns (V1008, V2003 may be optional in some cases)
  join_keys      <- PNADCperiods:::join_key_vars()
  join_available <- intersect(join_keys, names(data))

  # Validate data types and ranges
  # Note: PNADC data may have character columns that need conversion
  if ("Ano" %in% names(data)) {
    years <- suppressWarnings(as.integer(unique(data$Ano)))
    years <- years[!is.na(years)]
    invalid_years <- years[years < 2012 | years > 2100]
    if (length(invalid_years) > 0) {
      issues$invalid_years <- invalid_years
    }
  }

  if ("Trimestre" %in% names(data)) {
    quarters <- suppressWarnings(as.integer(unique(data$Trimestre)))
    quarters <- quarters[!is.na(quarters)]
    invalid_quarters <- quarters[!quarters %in% 1:4]
    if (length(invalid_quarters) > 0) {
      issues$invalid_quarters <- invalid_quarters
    }
  }

  if ("V2008" %in% names(data)) {
    days <- suppressWarnings(as.integer(unique(data$V2008)))
    # Exclude NA and special codes (99 = unknown)
    days <- days[!is.na(days) & days != 99]
    invalid_days <- days[days < 1 | days > 31]
    if (length(invalid_days) > 0) {
      issues$invalid_birth_days <- invalid_days
    }
  }

  if ("V20081" %in% names(data)) {
    months <- suppressWarnings(as.integer(unique(data$V20081)))
    # Exclude NA and special codes (99 = unknown)
    months <- months[!is.na(months) & months != 99]
    invalid_months <- months[months < 1 | months > 12]
    if (length(invalid_months) > 0) {
      issues$invalid_birth_months <- invalid_months
    }
  }

  if ("V2009" %in% names(data)) {
    ages <- suppressWarnings(as.numeric(unique(data$V2009)))
    ages <- ages[!is.na(ages)]
    # Allow ages up to 130 (rare but possible; higher values are likely data errors)
    # These extreme values will be handled gracefully during processing
    invalid_ages <- ages[ages < 0 | ages > 130]
    if (length(invalid_ages) > 0) {
      issues$warning_ages <- paste("Unusual ages found:", paste(head(invalid_ages, 5), collapse = ", "),
                                    "- these will be processed but may affect results")
    }
  }

  # Prepare result
  result <- list(
    valid = length(issues) == 0 || all(grepl("^warning", names(issues))),
    issues = issues,
    n_rows = nrow(data),
    n_cols = ncol(data),
    join_keys_available = join_available
  )

  if (stop_on_error && !result$valid) {
    error_issues <- issues[!grepl("^warning", names(issues))]
    msg <- paste0(
      "PNADC data validation failed:\n",
      paste(
        vapply(names(error_issues), function(nm) {
          paste0("  - ", nm, ": ", paste(error_issues[[nm]], collapse = ", "))
        }, character(1)),
        collapse = "\n"
      )
    )
    stop(msg, call. = FALSE)
  }

  if (stop_on_error) {
    # Print warnings if any
    warnings <- issues[grepl("^warning", names(issues))]
    if (length(warnings) > 0) {
      for (nm in names(warnings)) {
        warning(warnings[[nm]], call. = FALSE)
      }
    }
    invisible(result)
  } else {
    result
  }
}

#' Validate Monthly Totals Data
#'
#' Checks that monthly totals data has required structure.
#'
#' @param monthly_totals A data.frame with monthly population totals
#' @param stop_on_error Logical. If TRUE, stops with an error.
#'
#' @return Validation result or stops with error
#' @keywords internal
#' @noRd
validate_monthly_totals <- function(monthly_totals, stop_on_error = TRUE) {
  checkmate::assert_data_frame(monthly_totals, min.rows = 1)

  issues <- list()

  # Check for required column (either ref_month_yyyymm or anomesexato)
  has_yyyymm <- "ref_month_yyyymm" %in% names(monthly_totals)
  has_anomesexato <- "anomesexato" %in% names(monthly_totals)

  if (!has_yyyymm && !has_anomesexato) {
    issues$missing_date <- "Need either 'ref_month_yyyymm' or 'anomesexato' column"
  }

  # Check for population column
  has_pop <- "m_populacao" %in% names(monthly_totals)
  if (!has_pop) {
    issues$missing_population <- "Need 'm_populacao' column with monthly population totals"
  }

  if (stop_on_error && length(issues) > 0) {
    msg <- paste0(
      "Monthly totals validation failed:\n",
      paste(
        vapply(names(issues), function(nm) {
          paste0("  - ", nm, ": ", issues[[nm]])
        }, character(1)),
        collapse = "\n"
      )
    )
    stop(msg, call. = FALSE)
  }

  invisible(length(issues) == 0)
}

#' Check for Data.table
#'
#' Ensures data is a data.table, converting if necessary.
#'
#' @param data A data.frame or data.table
#' @param copy Logical. If TRUE, always returns a copy. If FALSE, may return
#'   the original object if already a data.table.
#' @return data.table
#' @keywords internal
#' @noRd
ensure_data_table <- function(data, copy = FALSE) {
  if (data.table::is.data.table(data)) {
    if (copy) {
      data.table::copy(data)
    } else {
      data
    }
  } else {
    data.table::as.data.table(data)
  }
}

# ============================================================================
# PERFORMANCE OPTIMIZATION: Helper functions for memory-efficient operations
# ============================================================================

#' Subset Data to Required Columns and Copy
#'
#' OPTIMIZATION: Instead of copying the entire data.table (potentially 50+ columns),
#' this function first subsets to only the required columns, then copies.
#' This can reduce memory usage by 80-90% for typical PNADC datasets.
#'
#' @param data A data.frame or data.table
#' @param required_cols Character vector of required column names
#' @param optional_cols Character vector of optional column names (included if present)
#' @return data.table with only the specified columns (always a copy)
#' @keywords internal
#' @noRd
subset_and_copy <- function(data, required_cols, optional_cols = NULL) {
  # Check required columns exist
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  # Build column list: required + available optional
  cols_to_keep <- required_cols
  if (!is.null(optional_cols)) {
    cols_to_keep <- unique(c(cols_to_keep, intersect(optional_cols, names(data))))
  }

  # Subset and copy in one operation
  # Use .SDcols to avoid R CMD check NOTE about ..cols_to_keep
  if (data.table::is.data.table(data)) {
    data.table::copy(data[, .SD, .SDcols = cols_to_keep])
  } else {
    data.table::as.data.table(data[, cols_to_keep, drop = FALSE])
  }
}

#' Convert PNADC Columns to Appropriate Types
#'
#' Consolidates repeated type conversion code from multiple functions.
#' Uses data.table::set() for in-place modification without copy overhead.
#'
#' @param dt A data.table (modified in place)
#' @param int_cols Character vector of columns to convert to integer
#' @param num_cols Character vector of columns to convert to integer (not numeric - saves memory)
#' @param na_codes Named list of NA codes per column (e.g., list(V2008 = 99L))
#' @return Invisibly returns dt (modified in place)
#' @keywords internal
#' @noRd
convert_pnadc_columns <- function(dt,
                                  num_cols = c("Ano", "Trimestre", "V2008", "V20081", "V20082", "V2009"),
                                  na_codes = list(V2008 = 99L, V20081 = 99L, V20082 = 9999L)) {


  # Convert to numeric - Multiple columns in place
  dt[ , names(.SD) := lapply(.SD, as.numeric), .SDcols = num_cols]

  # Replace NA codes with actual NA
  for (col in names(na_codes)) {
      na_val <- na_codes[[col]]
      dt[get(col) == na_val, (col) := NA_integer_]
  }

  invisible(dt)
}

#' Fix Infinite Values in Columns
#'
#' OPTIMIZATION: Replaces infinite values (from max/min on empty groups) with NA.
#' Uses data.table's efficient subsetting instead of which() + set() pattern.
#'
#' @param dt A data.table (modified in place)
#' @param cols Character vector of column names to check
#' @param replacement Value to replace infinites with (default NA_integer_)
#' @return Invisibly returns dt (modified in place)
#' @keywords internal
#' @noRd
fix_infinite_values <- function(dt, cols, replacement = NA_integer_) {

  dt[, (cols) := lapply(.SD, function(x) {
      x[is.infinite(x)] <- replacement
      x
    }), .SDcols = cols]

  invisible(dt)
}

#' Apply Birthday Constraints to Date Bounds
#'
#' OPTIMIZATION: Consolidates repeated birthday constraint code.
#' Applies constraints to narrow the interview date window based on birthday.
#'
#' @param dt A data.table with visit_before_birthday, first_sat_after_birthday columns
#' @param date_min_col Name of the minimum date column
#' @param date_max_col Name of the maximum date column
#' @return Invisibly returns dt (modified in place)
#' @keywords internal
#' @noRd
apply_birthday_constraints <- function(dt, date_min_col, date_max_col) {
  # If visited AFTER birthday, interview must be on/after first Saturday after birthday
  dt[visit_before_birthday == 0L & !is.na(first_sat_after_birthday) &
       first_sat_after_birthday > get(date_min_col) &
       first_sat_after_birthday <= get(date_max_col),
     (date_min_col) := first_sat_after_birthday]

  # If visited BEFORE birthday, interview must be before birthday
  dt[visit_before_birthday == 1L & !is.na(birthday) &
       birthday <= get(date_max_col) &
       birthday > get(date_min_col),
     (date_max_col) := birthday - 1L]

  invisible(dt)
}

#' Compute Determination Statistics Efficiently
#'
#' OPTIMIZATION: Uses uniqueN with direct column access instead of
#' aggregation with get() (which is extremely slow in data.table).
#'
#' @param dt A data.table
#' @param by_cols Grouping columns for counting unique combinations
#' @param determined_col Column name containing the determination result (NA = undetermined)
#' @return List with n_total and n_determined
#' @keywords internal
#' @noRd
compute_determination_stats <- function(dt, by_cols, determined_col) {
  # Count total unique combinations - use uniqueN for efficiency
  n_total <- dt[, data.table::uniqueN(.SD), .SDcols = by_cols]

  # Count determined (non-NA) - filter first, then count unique
  # Access column directly without get() for speed
  det_values <- dt[[determined_col]]
  determined_rows <- !is.na(det_values)
  n_determined <- dt[determined_rows, data.table::uniqueN(.SD), .SDcols = by_cols]

  list(
    n_total = n_total,
    n_determined = n_determined
  )
}
