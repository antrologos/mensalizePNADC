#' @title Date Utility Functions
#' @description Internal helper functions for date calculations.
#'
#'   This file includes:
#'   \itemize{
#'     \item Day of week calculation (dow)
#'     \item ISO 8601 week utilities (Monday-Sunday weeks)
#'     \item IBGE first Saturday calculation for reference weeks
#'     \item Month position calculation for quarter mapping
#'   }
#'
#' @name utils-dates
#' @keywords internal
NULL

# ============================================================================
# Pre-computed lookup tables for fast date creation
# These are computed once at package load
# ============================================================================

# Days since 1970-01-01 for January 1st of each year (2000-2050)
# Use plain integer vector with offset indexing instead of named lookup
.YEAR_BASE_START <- 2000L
.YEAR_BASE <- local({
  years <- 2000:2050

  # Vectorized approach instead of sapply
  as.integer(as.Date(paste0(years, "-01-01")))
})

# Days since 1970-01-01 for January 4th of each year (2000-2050)
# Used for fast ISO week calculation (Jan 4 is always in ISO week 1)
.JAN4_BASE <- local({
  years <- 2000:2050

  # Vectorized approach instead of sapply
  as.integer(as.Date(paste0(years, "-01-04")))
})

# Cumulative days at the start of each month (0-indexed from Jan 1)
# Regular year (non-leap)
.MONTH_OFFSET_REGULAR <- c(0L, 31L, 59L, 90L, 120L, 151L, 181L, 212L, 243L, 273L, 304L, 334L)

# Leap year (Feb has 29 days)
.MONTH_OFFSET_LEAP <- c(0L, 31L, 60L, 91L, 121L, 152L, 182L, 213L, 244L, 274L, 305L, 335L)

#' Day of Week
#'
#' Returns the day of week for a date.
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
  first_day <- lubridate::ymd(paste(year = year, month = month, day = 1L, sep = "-"))

  # Day of week for first of month (0=Sun, 6=Sat)
  first_dow <- PNADCperiods:::dow(first_day)

  # Calculate first Saturday
  # If first_dow = 0 (Sunday), first Saturday is day 7
  # If first_dow = 1 (Monday), first Saturday is day 6
  # ...
  # If first_dow = 6 (Saturday), first Saturday is day 1
  days_to_saturday   <- (6L - first_dow) %% 7L
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

  data.table::fifelse(first_saturday_day >= min_days, first_saturday_day, first_saturday_day + 7L)
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
  dow_date    <- dow(date)
  days_to_add <- (6L - dow_date) %% 7L
  date + days_to_add
}


# ============================================================================
# ISO 8601 Week Utilities
# ISO week 1 is the week containing January 4th (equivalently, the first week
# with at least 4 days in the new year). Weeks start on Monday.
# ============================================================================

#' ISO Week and Year Combined
#'
#' Returns both ISO 8601 week number and week-year for a date in a single pass.
#' This avoids redundant computation when both values are needed.
#'
#' Computes Thursday of the week only once, then derives both
#' week number and year. This is 2x faster than calling iso_week() and iso_year()
#' separately when both values are needed.
#'
#' @param date Date vector
#' @return List with two integer vectors: \code{year} (ISO week-year) and
#'   \code{week} (ISO week number 1-53)
#' @keywords internal
#' @noRd
iso_week_year <- function(date) {
  # Days since epoch
  days <- as.integer(date)

  # Day of week (0=Sun, 1=Mon, ..., 6=Sat) using R's Thursday origin
  date_dow <- (days + 4L) %% 7L

  # ISO weekday (Mon=1, ..., Sun=7)
  date_iso_wday <- data.table::fifelse(date_dow == 0L, 7L, as.integer(date_dow))

  # Thursday of the week containing this date (COMPUTED ONCE)
  thursday_days <- days + (4L - date_iso_wday)

  # ISO year = year of the Thursday
  thursday_date <- structure(thursday_days, class = "Date")
  iso_yr <- data.table::year(thursday_date)

  # Find Monday of week 1 of the ISO year
  # Jan 4 is always in week 1, so find its Monday
  jan4_days <- .JAN4_BASE[iso_yr - .YEAR_BASE_START + 1L]

  # Day of week of Jan 4
  jan4_dow <- (jan4_days + 4L) %% 7L
  jan4_iso_wday <- data.table::fifelse(jan4_dow == 0L, 7L, as.integer(jan4_dow))

  # Monday of week 1
  week1_monday_days <- jan4_days - (jan4_iso_wday - 1L)

  # Monday of the target date's week
  date_monday_days <- days - (date_iso_wday - 1L)

  # ISO week number
  iso_wk <- as.integer((date_monday_days - week1_monday_days) / 7L) + 1L

  list(year = iso_yr, week = iso_wk)
}

#' ISO Week Number
#'
#' Returns the ISO 8601 week number for a date. Week 1 is the week containing
#' January 4th (equivalently, the first week with at least 4 days in January).
#'
#' Uses pure integer arithmetic with pre-computed lookup tables.
#' This is ~300x faster than data.table::isoweek() for large vectors.
#'
#' Note: If you need both week and year, use \code{iso_week_year()} instead
#' to avoid redundant computation.
#'
#' @param date Date vector
#' @return Integer vector of ISO week numbers (1-53)
#' @keywords internal
#' @noRd
iso_week <- function(date) {
  PNADCperiods:::iso_week_year(date)$week
}

#' ISO Week-Year
#'
#' Returns the ISO 8601 week-year for a date. This may differ from the calendar
#' year for dates near year boundaries (e.g., Dec 31 may be in week 1 of the
#' next year, and Jan 1-3 may be in week 52/53 of the previous year).
#'
#' Note: If you need both week and year, use \code{iso_week_year()} instead
#' to avoid redundant computation.
#'
#' @param date Date vector
#' @return Integer vector of ISO week-years
#' @keywords internal
#' @noRd
iso_year <- function(date) {
  PNADCperiods:::iso_week_year(date)$year
}


# ============================================================================
# Month Position Calculation for Quarter Mapping
# ============================================================================

#' Calculate Month Position in Quarter (for date_min)
#'
#' Converts a date_min to its month position within the quarter (1, 2, or 3).
#' For date_min: if day <= threshold and not in first month of quarter, subtract 1.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param quarter Integer quarter vector (1-4)
#' @param day_threshold Integer threshold for adjustment (3 for standard, 2 for exception)
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
calculate_month_position_min <- function(date, year, quarter, day_threshold = 3L) {
  first_month <- PNADCperiods:::quarter_first_month(quarter)
  date_month  <- data.table::month(date)
  date_day    <- data.table::mday(date)

  pos <- date_month - first_month + 1L

  # Adjust if day <= threshold and not in first month of quarter
  adjust <- (date_day <= day_threshold & date_month > first_month)
  pos <- pos - as.integer(adjust)

  pmin(pmax(pos, 1L), 3L)
}


#' Calculate Month Position in Quarter (for date_max)
#'
#' Converts a date_max to its month position within the quarter (1, 2, or 3).
#' For date_max: if day <= threshold, use the month of (date - 3 days) instead.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param quarter Integer quarter vector (1-4)
#' @param day_threshold Integer threshold for adjustment (3 for standard, 2 for exception)
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
calculate_month_position_max <- function(date, year, quarter, day_threshold = 3L) {
  first_month <- PNADCperiods:::quarter_first_month(quarter)
  date_day    <- data.table::mday(date)

  # When day <= threshold, use the month of (date - 3 days)
  needs_adjust   <- date_day <= day_threshold
  adjusted_date  <- date - needs_adjust * 3L
  adjusted_month <- data.table::month(adjusted_date)

  pos <- adjusted_month - first_month + 1L

  pmin(pmax(pos, 1L), 3L)
}


#' Calculate Month Position (date_min) with Dynamic Per-Month Thresholds
#'
#' Applies different thresholds based on which month the date falls in and
#' whether that month has exception rules enabled.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param quarter Integer quarter vector (1-4)
#' @param exc_m1 Integer: exception flag for month 1 of quarter (0 or 1)
#' @param exc_m2 Integer: exception flag for month 2 of quarter (0 or 1)
#' @param exc_m3 Integer: exception flag for month 3 of quarter (0 or 1)
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
calculate_month_position_min_dynamic <- function(date, year, quarter, exc_m1, exc_m2, exc_m3) {
  first_month <- quarter_first_month(quarter)
  date_month <- data.table::month(date)
  date_day <- data.table::mday(date)

  pos <- date_month - first_month + 1L

  # Determine threshold based on which month the date falls in
  # Month 1 of quarter: months 1, 4, 7, 10
  # Month 2 of quarter: months 2, 5, 8, 11
  # Month 3 of quarter: months 3, 6, 9, 12
  # Use modular arithmetic to identify position in quarter
  month_in_quarter <- ((date_month - 1L) %% 3L) + 1L  # 1, 2, or 3

  # Threshold is 2 if exception applies for this month, 3 otherwise
  threshold <- fifelse(
    (month_in_quarter == 1L & exc_m1 == 1L) |
      (month_in_quarter == 2L & exc_m2 == 1L) |
      (month_in_quarter == 3L & exc_m3 == 1L),
    2L, 3L
  )

  # Adjust if day <= threshold and not in first month of quarter
  adjust <- (date_day <= threshold & date_month > first_month)
  pos <- pos - as.integer(adjust)

  pmin(pmax(pos, 1L), 3L)
}


#' Calculate Month Position (date_max) with Dynamic Per-Month Thresholds
#'
#' Applies different thresholds based on which month the date falls in and
#' whether that month has exception rules enabled.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param quarter Integer quarter vector (1-4)
#' @param exc_m1 Integer: exception flag for month 1 of quarter (0 or 1)
#' @param exc_m2 Integer: exception flag for month 2 of quarter (0 or 1)
#' @param exc_m3 Integer: exception flag for month 3 of quarter (0 or 1)
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
calculate_month_position_max_dynamic <- function(date, year, quarter, exc_m1, exc_m2, exc_m3) {
  first_month <- quarter_first_month(quarter)
  date_month  <- data.table::month(date)
  date_day    <- data.table::mday(date)

  # Determine threshold based on which month the date falls in
  # Use modular arithmetic to identify position in quarter
  month_in_quarter <- ((date_month - 1L) %% 3L) + 1L  # 1, 2, or 3

  # Threshold is 2 if exception applies for this month, 3 otherwise
  threshold <- fifelse(
    (month_in_quarter == 1L & exc_m1 == 1L) |
      (month_in_quarter == 2L & exc_m2 == 1L) |
      (month_in_quarter == 3L & exc_m3 == 1L),
    2L, 3L
  )

  # When day <= threshold, use the month of (date - 3 days)
  needs_adjust <- date_day <= threshold
  adjusted_date <- date - needs_adjust * 3L
  adjusted_month <- data.table::month(adjusted_date)

  pos <- adjusted_month - first_month + 1L

  pmin(pmax(pos, 1L), 3L)
}
