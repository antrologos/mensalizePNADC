#' @title Input Validation Utilities
#' @description Internal helper functions for validating PNADC input data.
#' @name utils-validation
#' @keywords internal
NULL

#' Required Variables for Reference Month Identification
#'
#' Returns the minimum required column names for identifying reference months.
#'
#' @return Character vector of required column names
#' @keywords internal
#' @noRd
required_vars_ref_month <- function() {
  c("Ano", "Trimestre", "UPA", "V1014", "V2008", "V20081", "V20082", "V2009")
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
#' @return If stop_on_error is TRUE, returns invisibly if valid or stops with error.
#'   If FALSE, returns a list with validation results.
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
