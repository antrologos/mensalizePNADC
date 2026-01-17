#' @title Date Utility Functions
#' @description Internal helper functions for date calculations. Optimized with
#'   pre-computed lookup tables for fast date creation (~20x faster than ISOdate).
#' @name utils-dates
#' @keywords internal
NULL

# ============================================================================
# Pre-computed lookup tables for fast date creation
# These are computed once at package load and used by make_date()
# ============================================================================

# Days since 1970-01-01 for January 1st of each year (2000-2050)
# OPTIMIZATION: Use plain integer vector with offset indexing instead of named lookup
.YEAR_BASE_START <- 2000L
.YEAR_BASE <- local({
  years <- 2000:2050
  sapply(years, function(y) as.integer(as.Date(paste0(y, "-01-01"))))
})

# Days since 1970-01-01 for January 4th of each year (2000-2050)
# Used for fast ISO week calculation (Jan 4 is always in ISO week 1)
.JAN4_BASE <- local({
  years <- 2000:2050
  sapply(years, function(y) as.integer(as.Date(paste0(y, "-01-04"))))
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

    # Check for years outside lookup table range (2000-2050)
    # Mark out-of-range years as invalid
    in_range <- y >= 2000L & y <= 2050L
    if (!all(in_range)) {
      # Warn once about out-of-range years
      n_out <- sum(!in_range)
      warning(sprintf(
        "make_date(): %d year(s) outside supported range (2000-2050). These will return NA.",
        n_out
      ), call. = FALSE)

      # Update valid mask to exclude out-of-range years
      valid_idx <- which(valid)
      valid[valid_idx[!in_range]] <- FALSE

      # Subset to only in-range values
      y <- y[in_range]
      m <- m[in_range]
      d <- d[in_range]
    }

    if (length(y) > 0) {
      # OPTIMIZATION: Use integer offset indexing instead of character lookup
      year_base <- .YEAR_BASE[y - .YEAR_BASE_START + 1L]

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
  }

  structure(result, class = "Date")
}

#' Handle February 29 Birthdays
#'
#' For leap year birthdays (Feb 29), returns March 1 in non-leap years.
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

#' Fast Year Extraction
#'
#' Extract year from Date using data.table's optimized function.
#' OPTIMIZATION: Direct alias to avoid function call overhead.
#'
#' @param date Date vector
#' @return Integer vector of years
#' @keywords internal
#' @noRd
fast_year <- data.table::year

#' Fast Month Extraction
#'
#' Extract month from Date using data.table's optimized function.
#' OPTIMIZATION: Direct alias to avoid function call overhead.
#'
#' @param date Date vector
#' @return Integer vector of months
#' @keywords internal
#' @noRd
fast_month <- data.table::month

#' Fast Day Extraction
#'
#' Extract day of month from Date using data.table's optimized function.
#' OPTIMIZATION: Direct alias to avoid function call overhead.
#'
#' @param date Date vector
#' @return Integer vector of days
#' @keywords internal
#' @noRd
fast_mday <- data.table::mday

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

# ============================================================================
# ISO 8601 Week Utilities (Optimized with integer arithmetic)
# ISO week 1 is the week containing January 4th (equivalently, the first week
# with at least 4 days in the new year). Weeks start on Monday.
#
# OPTIMIZATION: data.table::isoweek() is extremely slow (~50s for 4M dates).
# We use pure integer arithmetic with lookup tables for 300x speedup.
# ============================================================================

#' ISO Week and Year Combined (Optimized)
#'
#' Returns both ISO 8601 week number and week-year for a date in a single pass.
#' This avoids redundant computation when both values are needed.
#'
#' OPTIMIZATION: Computes Thursday of the week only once, then derives both
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

#' ISO Week Number (Optimized)
#'
#' Returns the ISO 8601 week number for a date. Week 1 is the week containing
#' January 4th (equivalently, the first week with at least 4 days in January).
#'
#' OPTIMIZATION: Uses pure integer arithmetic with pre-computed lookup tables.
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
  iso_week_year(date)$week
}

#' ISO Week-Year (Optimized)
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
  iso_week_year(date)$year
}

#' Create ISO Week Integer (YYYYWW format)
#'
#' Creates an integer in YYYYWW format from ISO year and week.
#'
#' @param iso_yr Integer ISO week-year
#' @param iso_wk Integer ISO week number (1-53)
#' @return Integer in YYYYWW format (e.g., 202301 for week 1 of 2023)
#' @keywords internal
#' @noRd
yyyyww <- function(iso_yr, iso_wk) {
  as.integer(iso_yr * 100L + iso_wk)
}

#' Convert Date to YYYYWW Format
#'
#' Optimized to compute ISO year and week in a single pass.
#'
#' @param date Date vector
#' @return Integer vector in YYYYWW format
#' @keywords internal
#' @noRd
date_to_yyyyww <- function(date) {
  iwy <- iso_week_year(date)
  yyyyww(iwy$year, iwy$week)
}

#' Monday of ISO Week (Optimized)
#'
#' Returns the Monday (first day) of the ISO week containing the given date.
#' Uses integer arithmetic for speed.
#'
#' @param date Date vector
#' @return Date vector of Mondays
#' @keywords internal
#' @noRd
iso_week_monday <- function(date) {
  # Days since epoch
  days <- as.integer(date)

  # Day of week (0=Sun, 1=Mon, ..., 6=Sat)
  date_dow <- (days + 4L) %% 7L

  # ISO weekday (Mon=1, ..., Sun=7)
  date_iso_wday <- data.table::fifelse(date_dow == 0L, 7L, as.integer(date_dow))

  # Monday = date - (iso_wday - 1)
  structure(days - (date_iso_wday - 1L), class = "Date")
}

#' Number of ISO Weeks in Year
#'
#' Returns 52 or 53 depending on the year's structure.
#' A year has 53 weeks if:
#' - January 1 is a Thursday, OR
#' - January 1 is a Wednesday and it's a leap year
#'
#' @param year Integer year vector
#' @return Integer vector (52 or 53)
#' @keywords internal
#' @noRd
iso_weeks_in_year <- function(year) {
  jan1 <- make_date(year, 1L, 1L)
  jan1_dow <- dow(jan1)  # 0=Sun, 1=Mon, ..., 6=Sat

  # Thursday = dow 4, Wednesday = dow 3
  is_thursday <- (jan1_dow == 4L)
  is_wednesday <- (jan1_dow == 3L)
  is_leap <- is_leap_year(year)

  data.table::fifelse(is_thursday | (is_wednesday & is_leap), 53L, 52L)
}

#' Week Position in Quarter
#'
#' Returns which week within the quarter (1-14 typically) a date falls in.
#' The position is calculated relative to the first Monday of the quarter.
#'
#' @param date Date vector
#' @param quarter Integer vector of quarters (1-4)
#' @param year Integer vector of years
#' @return Integer vector (1-14, or NA if date is before quarter start)
#' @keywords internal
#' @noRd
week_in_quarter <- function(date, quarter, year) {
  # Get first day of the quarter
  first_month <- quarter_first_month(quarter)
  quarter_start <- make_date(year, first_month, 1L)

  # Find the Monday of the week containing quarter_start
  quarter_start_monday <- iso_week_monday(quarter_start)

  # If that Monday is before the quarter starts, use the next Monday
  # This ensures week 1 is the first full or partial week IN the quarter
  first_monday <- data.table::fifelse(
    quarter_start_monday < quarter_start,
    quarter_start_monday + 7L,
    quarter_start_monday
  )

  # Get Monday of the target date's week
  date_monday <- iso_week_monday(date)

  # Calculate week position (1-indexed)
  week_pos <- as.integer((as.integer(date_monday) - as.integer(first_monday)) / 7L) + 1L

  # Return NA for dates before the first Monday of the quarter
  data.table::fifelse(date_monday < first_monday, NA_integer_, week_pos)
}

#' Convert YYYYWW to Date (Monday of that week)
#'
#' @param yyyyww Integer in YYYYWW format
#' @return Date vector (Monday of the specified ISO week)
#' @keywords internal
#' @noRd
yyyyww_to_date <- function(yyyyww) {
  iso_yr <- yyyyww %/% 100L
  iso_wk <- yyyyww %% 100L


  # Find January 4 of the ISO year (always in week 1)
  jan4 <- make_date(iso_yr, 1L, 4L)

  # Find Monday of week 1
  week1_monday <- iso_week_monday(jan4)

  # Add weeks to get target Monday
  week1_monday + (iso_wk - 1L) * 7L
}

#' Count Weeks in Month
#'
#' Returns the number of ISO weeks that have at least one day in a given month.
#' Used for distributing monthly population to weekly targets.
#'
#' @param year Integer year
#' @param month Integer month (1-12)
#' @return Integer count of weeks touching this month
#' @keywords internal
#' @noRd
weeks_in_month <- function(year, month) {
  # First and last day of month
  first_day <- make_date(year, month, 1L)

  # Calculate last day of month
  days_in_month <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
  last_day_num <- days_in_month[month]
  # Adjust for leap year February
  last_day_num <- data.table::fifelse(
    month == 2L & is_leap_year(year),
    29L,
    last_day_num
  )
  last_day <- make_date(year, month, last_day_num)

  # Get ISO weeks of first and last day
  first_week <- date_to_yyyyww(first_day)
  last_week <- date_to_yyyyww(last_day)

  # Handle year boundary (e.g., Dec having weeks in next ISO year)
  # We need to count distinct weeks, accounting for year wrap
  first_yr <- first_week %/% 100L
  first_wk <- first_week %% 100L
  last_yr <- last_week %/% 100L
  last_wk <- last_week %% 100L

  # If same ISO year, simple subtraction
  # If different years (Dec spanning into next year's week 1), adjust
  data.table::fifelse(
    first_yr == last_yr,
    last_wk - first_wk + 1L,
    # Different years: weeks from first to end of first_yr, plus weeks 1 to last_wk
    (iso_weeks_in_year(first_yr) - first_wk + 1L) + last_wk
  )
}

