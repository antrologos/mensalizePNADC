#' @title Date Utility Functions
#' @description Internal helper functions for date calculations, designed to
#'   match Stata's date handling conventions. Optimized with pre-computed
#'   lookup tables for fast date creation (~20x faster than ISOdate).
#' @name utils-dates
#' @keywords internal
NULL

# ============================================================================
# Pre-computed lookup tables for fast date creation
# These are computed once at package load and used by make_date()
# ============================================================================

# Days since 1970-01-01 for January 1st of each year (2000-2050)
.YEAR_BASE <- local({
  years <- 2000:2050
  setNames(
    sapply(years, function(y) as.integer(as.Date(paste0(y, "-01-01")))),
    as.character(years)
  )
})

# Cumulative days at the start of each month (0-indexed from Jan 1)
# Regular year (non-leap)
.MONTH_OFFSET_REGULAR <- c(0L, 31L, 59L, 90L, 120L, 151L, 181L, 212L, 243L, 273L, 304L, 334L)

# Leap year (Feb has 29 days)
.MONTH_OFFSET_LEAP <- c(0L, 31L, 60L, 91L, 121L, 152L, 182L, 213L, 244L, 274L, 305L, 335L)

#' Day of Week (Stata-compatible)
#'
#' Returns the day of week for a date, matching Stata's `dow()` function.
#' Uses fast integer arithmetic instead of format().
#'
#' @param date A Date object or vector of Dates
#' @return Integer vector: 0 = Sunday, 1 = Monday, ..., 6 = Saturday
#' @keywords internal
#' @noRd
dow <- function(date) {
  # R's origin is 1970-01-01 which was a Thursday (dow=4)
  # Use modular arithmetic: (days_since_origin + 4) %% 7
  (as.integer(date) + 4L) %% 7L
}

#' First Day of Month
#'
#' Returns the first day of the month for a given date.
#'
#' @param date A Date object or vector of Dates
#' @return Date vector with day set to 1
#' @keywords internal
#' @noRd
first_of_month <- function(date) {
  as.Date(paste(format(date, "%Y-%m"), "01", sep = "-"))
}

#' Create Date from Year, Month, Day (Optimized)
#'
#' Creates Date objects using pre-computed lookup tables for speed.
#' This is ~20x faster than ISOdate for large vectors.
#'
#' Note: For invalid dates (e.g., Feb 29 in non-leap years), this returns
#' an incorrect date rather than NA. This is acceptable because the
#' mensalization algorithm only creates valid dates by construction.
#'
#' @param year Integer vector of years (must be in range 2000-2050)
#' @param month Integer vector of months (1-12)
#' @param day Integer vector of days (1-31)
#' @return Date vector
#' @keywords internal
#' @noRd
make_date <- function(year, month, day) {
  year <- as.integer(year)
  month <- as.integer(month)

  day <- as.integer(day)

  n <- max(length(year), length(month), length(day))
  year <- rep_len(year, n)
  month <- rep_len(month, n)
  day <- rep_len(day, n)

  # Handle NAs
  valid <- !is.na(year) & !is.na(month) & !is.na(day)

  # Initialize result with NAs
  result <- rep(NA_integer_, n)

  if (any(valid)) {
    y <- year[valid]
    m <- month[valid]
    d <- day[valid]

    # Year base lookup (days since 1970-01-01 for Jan 1 of each year)
    year_base <- .YEAR_BASE[as.character(y)]

    # Leap year check (vectorized)
    is_leap <- (y %% 4L == 0L & y %% 100L != 0L) | (y %% 400L == 0L)

    # Month offset using data.table::fifelse for speed
    month_offset <- data.table::fifelse(
      is_leap,
      .MONTH_OFFSET_LEAP[m],
      .MONTH_OFFSET_REGULAR[m]
    )

    # Total days since epoch
    result[valid] <- year_base + month_offset + d - 1L
  }

  structure(result, class = "Date")
}

#' Handle February 29 Birthdays
#'
#' For leap year birthdays (Feb 29), returns March 1 in non-leap years.
#' This matches the Stata code's handling of these edge cases.
#'
#' Note: With the optimized make_date(), Feb 29 on non-leap years
#' automatically becomes March 1 via the lookup math, so this function
#' is now just a direct wrapper around make_date().
#'
#' @param birth_month Integer vector of birth months
#' @param birth_day Integer vector of birth days
#' @param year Integer vector of years to create dates for
#' @return Date vector of birthdays in the given year
#' @keywords internal
#' @noRd
make_birthday <- function(birth_month, birth_day, year) {
  # The optimized make_date() automatically handles Feb 29 on non-leap years

  # by returning March 1 (via the lookup arithmetic), which is the desired behavior
  make_date(year, birth_month, birth_day)
}

#' Is Leap Year
#'
#' Check if a year is a leap year.
#'
#' @param year Integer vector of years
#' @return Logical vector
#' @keywords internal
#' @noRd
is_leap_year <- function(year) {
  (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
}

#' Get Quarter Months
#'
#' Returns the three months of a quarter.
#'
#' @param quarter Integer quarter (1-4), must be scalar
#' @return Integer vector of length 3 with month numbers
#' @keywords internal
#' @noRd
quarter_months <- function(quarter) {
  # This function is designed for scalar input
  # For vectorized use, call with quarter[1] or use quarter_first_month() / quarter_month_n()
  if (length(quarter) != 1) {
    stop("quarter_months() expects a scalar, got length ", length(quarter))
  }
  (quarter - 1L) * 3L + 1L:3L
}

#' Get First Month of Quarter (vectorized)
#'
#' @param quarter Integer vector of quarters (1-4)
#' @return Integer vector of first month numbers
#' @keywords internal
#' @noRd
quarter_first_month <- function(quarter) {
  (quarter - 1L) * 3L + 1L
}

#' Get N-th Month of Quarter (vectorized)
#'
#' @param quarter Integer vector of quarters (1-4)
#' @param n Which month in quarter (1, 2, or 3)
#' @return Integer vector of month numbers
#' @keywords internal
#' @noRd
quarter_month_n <- function(quarter, n) {
  (quarter - 1L) * 3L + n
}

#' First Day of Quarter
#'
#' Returns the first day of a quarter.
#'
#' @param year Integer year
#' @param quarter Integer quarter (1-4)
#' @return Date
#' @keywords internal
#' @noRd
first_of_quarter <- function(year, quarter) {
  make_date(year, quarter_months(quarter)[1], 1L)
}

#' Create Year-Month Integer (YYYYMM format)
#'
#' Creates an integer in YYYYMM format from year and month.
#'
#' @param year Integer year
#' @param month Integer month (1-12)
#' @return Integer in YYYYMM format (e.g., 202301 for January 2023)
#' @keywords internal
#' @noRd
yyyymm <- function(year, month) {
  as.integer(year * 100L + month)
}

#' Extract Year-Month from Date
#'
#' Extracts YYYYMM integer from a Date.
#'
#' @param date Date vector
#' @return Integer vector in YYYYMM format
#' @keywords internal
#' @noRd
date_to_yyyymm <- function(date) {
  year <- fast_year(date)
  month <- fast_month(date)
  yyyymm(year, month)
}

#' Fast Year Extraction
#'
#' Extract year from Date using data.table's optimized function.
#' Note: data.table is a package dependency, so no need to check availability.
#'
#' @param date Date vector
#' @return Integer vector of years
#' @keywords internal
#' @noRd
fast_year <- function(date) {
  data.table::year(date)
}

#' Fast Month Extraction
#'
#' Extract month from Date using data.table's optimized function.
#' Note: data.table is a package dependency, so no need to check availability.
#'
#' @param date Date vector
#' @return Integer vector of months
#' @keywords internal
#' @noRd
fast_month <- function(date) {
  data.table::month(date)
}

#' Fast Day Extraction
#'
#' Extract day of month from Date using data.table's optimized function.
#' Note: data.table is a package dependency, so no need to check availability.
#'
#' @param date Date vector
#' @return Integer vector of days
#' @keywords internal
#' @noRd
fast_mday <- function(date) {
  data.table::mday(date)
}

#' First Saturday of Month (with minimum days constraint)
#'
#' Calculates the day of the month for the first Saturday that has at least
#' `min_days` in the reference week within that month. This implements IBGE's
#' "Parada Tecnica" rule.
#'
#' The reference week ends on Saturday. If Saturday falls on day X of the month,
#' then X days of that week are within the month (days 1 through X). For the
#' first week to be valid, X must be >= min_days.
#'
#' @param year Integer year
#' @param month Integer month (1-12)
#' @param min_days Integer minimum days required (default 4, use 3 for exception quarters)
#' @return Integer day of month for the first valid Saturday
#' @keywords internal
#' @noRd
first_valid_saturday <- function(year, month, min_days = 4L) {
  # Get first day of the month
  first_day <- make_date(year, month, 1L)

  # Day of week for first of month (0=Sun, 6=Sat)
  first_dow <- dow(first_day)

  # Calculate first Saturday
  # If first_dow = 0 (Sunday), first Saturday is day 7
  # If first_dow = 1 (Monday), first Saturday is day 6
  # ...
  # If first_dow = 6 (Saturday), first Saturday is day 1
  days_to_saturday <- (6L - first_dow) %% 7L
  first_saturday_day <- 1L + days_to_saturday

  # How many days of the reference week (ending on Saturday) are in this month?
  # If Saturday is on day X, then X days (1 through X) are in the month.
  # We need X >= min_days for the week to be valid.
  #
  # Examples for min_days = 4:
  #   If Saturday is day 7: 7 >= 4, valid
  #   If Saturday is day 4: 4 >= 4, valid
  #   If Saturday is day 3: 3 < 4, skip to next week (day 10)
  #   If Saturday is day 1: 1 < 4, skip to next week (day 8)
  #
  # Examples for min_days = 3:
  #   If Saturday is day 3: 3 >= 3, valid
  #   If Saturday is day 2: 2 < 3, skip to next week (day 9)

  # If first Saturday has enough days, use it; otherwise use second Saturday
  ifelse(first_saturday_day >= min_days, first_saturday_day, first_saturday_day + 7L)
}

#' First Saturday After or On a Date
#'
#' Finds the first Saturday that is on or after the given date.
#'
#' @param date Date vector
#' @return Date vector of Saturdays
#' @keywords internal
#' @noRd
first_saturday_on_or_after <- function(date) {
  dow_date <- dow(date)
  days_to_add <- (6L - dow_date) %% 7L
  date + days_to_add
}

#' Month Position in Quarter
#'
#' Returns which month within the quarter (1, 2, or 3) a date falls in.
#'
#' @param date Date vector
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
month_in_quarter <- function(date) {
  month <- fast_month(date)
  ((month - 1L) %% 3L) + 1L
}

#' Quarter from Month
#'
#' Returns the quarter (1-4) for a given month.
#'
#' @param month Integer month (1-12)
#' @return Integer quarter (1-4)
#' @keywords internal
#' @noRd
month_to_quarter <- function(month) {
  ((month - 1L) %/% 3L) + 1L
}
