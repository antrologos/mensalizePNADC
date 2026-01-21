#' @title Input Validation Utilities
#' @description Internal helper functions for validating PNADC input data.
#' @name utils-validation
#' @keywords internal
NULL

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

#' Required Variables for Join Keys
#'
#' Returns the column names used as join keys in the output crosswalk.
#'
#' @return Character vector of join key column names
#' @keywords internal
#' @noRd
join_key_vars <- function() {
  c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003")
}

#' Required Variables for Weight Calibration
#'
#' Returns the additional column names required for computing monthly weights.
#'
#' @return Character vector of required column names
#' @keywords internal
#' @noRd
required_vars_weights <- function() {
  c(
    # Survey design
    "V1028", "UF", "posest", "posest_sxi"
    # V2009 (age) is already required for ref_month identification
  )
}

#' Required Variables for Monthly Totals
#'
#' Returns the required column names for the monthly population totals data.
#'
#' @return Character vector of required column names
#' @keywords internal
#' @noRd
required_vars_monthly_totals <- function() {
  c("ref_month_yyyymm", "m_populacao")
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
  required <- required_vars_ref_month()
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    issues$missing_ref_month <- missing
  }

  # Check weight-related columns if requested
  if (check_weights) {
    required_wt <- required_vars_weights()
    missing_wt <- setdiff(required_wt, names(data))
    if (length(missing_wt) > 0) {
      issues$missing_weights <- missing_wt
    }
  }

  # Check join key columns (V1008, V2003 may be optional in some cases)
  join_keys <- join_key_vars()
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

#' Required Variables for Reference Week Identification
#'
#' Returns the minimum required column names for identifying reference weeks.
#' Same as monthly, but V1008 (household) is essential for aggregation.
#'
#' @return Character vector of required column names
#' @keywords internal
#' @noRd
required_vars_ref_week <- function() {
  c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2008", "V20081", "V20082", "V2009")
}

#' Required Variables for Weekly Totals
#'
#' Returns the required column names for the weekly population totals data.
#'
#' @return Character vector of required column names
#' @keywords internal
#' @noRd
required_vars_weekly_totals <- function() {
  c("ref_week_yyyyww", "w_populacao")
}

#' Validate Weekly Totals Data
#'
#' Checks that weekly totals data has required structure.
#'
#' @param weekly_totals A data.frame with weekly population totals
#' @param stop_on_error Logical. If TRUE, stops with an error.
#'
#' @return Validation result or stops with error
#' @keywords internal
#' @noRd
validate_weekly_totals <- function(weekly_totals, stop_on_error = TRUE) {
  checkmate::assert_data_frame(weekly_totals, min.rows = 1)

  issues <- list()

  # Check for required columns (accept either column name for compatibility)
  has_yyyyww <- "ref_week_yyyyww" %in% names(weekly_totals) ||
                "ref_week_iso_yyyyww" %in% names(weekly_totals)
  if (!has_yyyyww) {
    issues$missing_date <- "Need 'ref_week_yyyyww' column (or 'ref_week_iso_yyyyww')"
  }

  has_pop <- "w_populacao" %in% names(weekly_totals)
  if (!has_pop) {
    issues$missing_population <- "Need 'w_populacao' column with weekly population totals"
  }

  if (stop_on_error && length(issues) > 0) {
    msg <- paste0(
      "Weekly totals validation failed:\n",
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
#' OPTIMIZATION: Consolidates repeated type conversion code from multiple functions.
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
                                   int_cols = c("Ano", "Trimestre", "V2008", "V20081", "V20082"),
                                   num_cols = "V2009",
                                   na_codes = list(V2008 = 99L, V20081 = 99L, V20082 = 9999L)) {
  # Convert integer columns
  for (col in int_cols) {
    if (col %in% names(dt) && !is.integer(dt[[col]])) {
      data.table::set(dt, j = col, value = as.integer(dt[[col]]))
    }
  }

  # Convert numeric columns to INTEGER (OPTIMIZATION: 4 bytes vs 8 bytes for double)
  # Age (V2009) is always a whole number, so integer is sufficient
  for (col in num_cols) {
    if (col %in% names(dt) && !is.integer(dt[[col]])) {
      data.table::set(dt, j = col, value = as.integer(dt[[col]]))
    }
  }

  # Replace NA codes with actual NA
  for (col in names(na_codes)) {
    if (col %in% names(dt)) {
      na_val <- na_codes[[col]]
      dt[get(col) == na_val, (col) := NA_integer_]
    }
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
  for (col in cols) {
    if (col %in% names(dt)) {
      dt[is.infinite(get(col)), (col) := replacement]
    }
  }
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
#' OPTIMIZATION: Computes total and determined counts in a single pass
#' instead of two separate uniqueN operations.
#'
#' @param dt A data.table
#' @param by_cols Grouping columns for counting unique combinations
#' @param determined_col Column name containing the determination result (NA = undetermined)
#' @return List with n_total and n_determined
#' @keywords internal
#' @noRd
compute_determination_stats <- function(dt, by_cols, determined_col) {
  # Single aggregation to get both counts
  stats <- dt[, .(
    is_determined = !is.na(get(determined_col)[1L])
  ), by = by_cols]

  list(
    n_total = nrow(stats),
    n_determined = sum(stats$is_determined)
  )
}
