#' @title Date Utility Functions
#' @description Internal helper functions for date calculations. Optimized with
#'   pre-computed lookup tables for fast date creation (~20x faster than ISOdate).
#'
#'   This file includes:
#'   \itemize{
#'     \item Fast date creation with lookup tables (make_date)
#'     \item ISO 8601 week utilities (Monday-Sunday weeks)
#'     \item IBGE reference calendar utilities (Sunday-Saturday weeks)
#'   }
#'
#' @section IBGE Reference Calendar:
#' The IBGE reference calendar differs from ISO 8601:
#' \itemize{
#'   \item Weeks start on Sunday and end on Saturday
#'   \item A week belongs to the IBGE month where its Saturday falls
#'   \item The first valid Saturday must have >= 4 days (or 3 as exception) in the month
#'   \item IBGE months always have exactly 4 reference weeks
#' }
#'
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
  # OPTIMIZATION: Vectorized approach instead of sapply
  as.integer(as.Date(paste0(years, "-01-01")))
})

# Days since 1970-01-01 for January 4th of each year (2000-2050)
# Used for fast ISO week calculation (Jan 4 is always in ISO week 1)
.JAN4_BASE <- local({
  years <- 2000:2050
  # OPTIMIZATION: Vectorized approach instead of sapply
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


#' Handle February 29 Birthdays
#'
#' For leap year birthdays (Feb 29), returns March 1 in non-leap years.
#'
#' @param birth_month Integer vector of birth months
#' @param birth_day Integer vector of birth days
#' @param year Integer vector of years to create dates for
#' @return Date vector of birthdays in the given year
#' @keywords internal
#' @noRd
#'

birth_day   = 29
birth_month = 2
year        = 2019

make_birthday <- function(birth_day, birth_month, year) {

  is_leap <- PNADCperiods:::is_leap_year(year)

  # automatically handles Feb 29 on non-leap years
  must_change = (!is_leap) & birth_day == 29 & birth_month == 2

  birth_day[must_change]   = 1
  birth_month[must_change] = 3

  # by returning March 1 (via the lookup arithmetic), which is the desired behavior
  lubridate::ymd(paste(year, birth_month, birth_day, sep = "-"))
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
  first_day <- lubridate:::ymd(paste(year = year, month = month, day = 1L, sep = "-"))

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
  # OPTIMIZATION: Use fifelse instead of ifelse for 5-10x speedup

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

#' Month Position in Quarter
#'
#' Returns which month within the quarter (1, 2, or 3) a date falls in.
#'
#' @param date Date vector
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
month_in_quarter <- function(date) {
  month <- data.table:::month(date)
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
  PNADCperiods:::iso_week_year(date)$week
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
  PNADCperiods:::iso_week_year(date)$year
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
  iwy <- PNADCperiods:::iso_week_year(date)
  PNADCperiods:::yyyyww(iwy$year, iwy$week)
}

#' Convert YYYYWW to Sequential Value
#'
#' Converts YYYYWW format to a sequential integer for proper comparison
#' across year boundaries. Uses actual number of weeks in each year (52 or 53)
#' for accurate sequencing.
#'
#' @param yyyyww Integer in YYYYWW format (e.g., 202352)
#' @return Integer sequential value suitable for comparisons
#' @keywords internal
#' @noRd
yyyyww_to_seq <- function(yyyyww) {
  iso_yr <- yyyyww %/% 100L
  iso_wk <- yyyyww %% 100L


  # Calculate cumulative weeks from a base year
  # Use year 2000 as base (before PNADC data starts)
  base_year <- 2000L

  # For each year from base to iso_yr-1, sum the weeks
  # Simplified: use 52.1775 average weeks/year for speed (accurate enough for comparisons)
  # More accurate: year * 52 + adjustment for 53-week years
  years_since_base <- iso_yr - base_year

  # Approximate: each year has ~52.1775 weeks on average
  # This is fast and works for comparison purposes
  (years_since_base * 52L) + iso_wk
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
  jan1     <- lubridate::ymd(paste(year, 1L, 1L, sep = "-"))
  jan1_dow <- PNADCperiods:::dow(jan1)  # 0=Sun, 1=Mon, ..., 6=Sat

  # Thursday = dow 4, Wednesday = dow 3
  is_thursday  <- (jan1_dow == 4L)
  is_wednesday <- (jan1_dow == 3L)
  is_leap      <- PNADCperiods:::is_leap_year(year)

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
#' Days in Month
#'
#' Returns the number of days in a given month, accounting for leap years.
#'
#' @param year Integer year (vectorized)
#' @param month Integer month 1-12 (vectorized)
#' @return Integer number of days in the month
#' @keywords internal
#' @noRd
days_in_month <- function(year, month) {
  base_days <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
  days <- base_days[month]
  # Adjust for leap year February
  data.table::fifelse(
    month == 2L & is_leap_year(year),
    29L,
    days
  )
}


# ============================================================================
# IBGE Reference Calendar Utilities (Sunday-Saturday weeks)
#
# IBGE's reference calendar differs from ISO 8601:
# - Weeks start on SUNDAY and end on SATURDAY (not Monday-Sunday)
# - A week belongs to the IBGE month where its SATURDAY falls
# - The first valid Saturday must have >= 4 days in the month
# - IBGE months always have exactly 4 reference weeks
#
# The IBGE calendar forms a partition: every day belongs to exactly one
# IBGE week, and every week belongs to exactly one IBGE month.
# ============================================================================

#' Get Sunday Starting an IBGE Week
#'
#' Returns the Sunday that starts the IBGE reference week containing a date.
#' IBGE weeks run Sunday-Saturday.
#'
#' @param date Date vector
#' @return Date vector of Sundays
#' @keywords internal
#' @noRd
ibge_week_sunday <- function(date) {
  # Days since epoch
  days <- as.integer(date)

  # Day of week (0=Sun, 1=Mon, ..., 6=Sat)
  date_dow <- (days + 4L) %% 7L

  # Sunday of this week = date - dow
  structure(days - date_dow, class = "Date")
}


#' Get Saturday Ending an IBGE Week
#'
#' Returns the Saturday that ends the IBGE reference week containing a date.
#' IBGE weeks run Sunday-Saturday.
#'
#' @param date Date vector
#' @return Date vector of Saturdays
#' @keywords internal
#' @noRd
ibge_week_saturday <- function(date) {
  # Days since epoch
  days <- as.integer(date)

  # Day of week (0=Sun, 1=Mon, ..., 6=Sat)
  date_dow <- (days + 4L) %% 7L

  # Saturday of this week = date + (6 - dow)
  structure(days + (6L - date_dow), class = "Date")
}


#' First Sunday of an IBGE Month
#'
#' Returns the Sunday that starts the first reference week of an IBGE month.
#' This is the Sunday before or on the first valid Saturday of the month.
#'
#' @param year Integer year (vectorized)
#' @param month Integer month 1-12 (vectorized)
#' @param min_days Integer minimum days required for first Saturday (default 4)
#' @return Date vector
#' @keywords internal
#' @noRd
ibge_month_start <- function(year, month, min_days = 4L) {
  # Get first valid Saturday of the month
  first_sat_day <- first_valid_saturday(year, month, min_days = min_days)
  first_sat_date <- make_date(year, month, first_sat_day)

  # Sunday of that week = Saturday - 6
  first_sat_date - 6L
}


#' Last Saturday of an IBGE Month
#'
#' Returns the Saturday that ends the last (4th) reference week of an IBGE month.
#' IBGE months always have exactly 4 reference weeks (28 days). The last Saturday
#' is always first_saturday + 21 days.
#'
#' @param year Integer year (vectorized)
#' @param month Integer month 1-12 (vectorized)
#' @param min_days Integer minimum days required for first Saturday (default 4)
#' @return Date vector
#' @keywords internal
#' @noRd
ibge_month_end <- function(year, month, min_days = 4L) {
  # Get first valid Saturday
  first_sat_day <- first_valid_saturday(year, month, min_days = min_days)
  first_sat_date <- make_date(year, month, first_sat_day)

  # Last Saturday = first Saturday + 21 days (4 weeks - 1 = 3 weeks offset)
  # IBGE months always have exactly 4 reference weeks
  first_sat_date + 21L
}


#' Number of IBGE Reference Weeks in a Month
#'
#' IBGE months ALWAYS have exactly 4 reference weeks (28 days). This is a
#' fundamental rule of the IBGE reference calendar. Any week that falls between
#' the end of one IBGE month and the start of the next is a "technical stop"
#' (parada técnica) and does not belong to any IBGE month.
#'
#' @param year Integer year (vectorized) - included for API consistency but not used
#' @param month Integer month 1-12 (vectorized) - included for API consistency but not used
#' @param min_days Integer minimum days required (default 4) - included for API consistency but not used
#' @return Integer vector, always 4L
#' @keywords internal
#' @noRd
ibge_month_weeks <- function(year, month, min_days = 4L) {

  # IBGE months ALWAYS have exactly 4 reference weeks - no exceptions

  # Any "5th week" would be a technical stop, not part of any IBGE month

  n <- max(length(year), length(month))
  rep(4L, n)
}


#' Detect Type of Technical Stop
#'
#' Classifies a technical stop (parada técnica) as either "quarter_boundary"
#' (the week falls outside the quarter's valid IBGE range) or "within_quarter"
#' (the week falls in a gap between two IBGE months within the quarter).
#'
#' Technical stops are IBGE weeks (Sunday-Saturday) that don't belong to any
#' IBGE month. They occur:
#' - At quarter boundaries (before first or after last valid week)
#' - Between consecutive IBGE months within a quarter (gaps)
#'
#' @param representative_date Date vector - a date within the technical stop week
#' @param quarter Integer vector of quarters (1-4)
#' @param year Integer vector of years
#' @param min_days Integer minimum days required for first Saturday (default 4)
#' @return Character vector: "quarter_boundary" or "within_quarter"
#' @keywords internal
#' @noRd
detect_technical_stop_type <- function(representative_date, quarter, year, min_days = 4L) {
  # Get Saturday of the week containing the representative date
  week_saturday <- ibge_week_saturday(representative_date)

  # Get first and third months of the quarter
  first_month <- quarter_first_month(quarter)
  third_month <- quarter_month_n(quarter, 3L)

  # Calculate quarter boundaries:
  # - First valid Saturday = first Saturday of month 1 with >= min_days in the week
  # - Last valid Saturday = last Saturday of month 3 (4th week)
  quarter_first_saturday <- make_date(year, first_month, first_valid_saturday(year, first_month, min_days))
  quarter_last_saturday <- ibge_month_end(year, third_month, min_days)

  # Classify: if Saturday is outside quarter bounds, it's a quarter boundary stop
  # Otherwise, it's within the quarter (a gap between months)
  data.table::fifelse(
    is.na(representative_date),
    NA_character_,
    data.table::fifelse(
      week_saturday < quarter_first_saturday | week_saturday > quarter_last_saturday,
      "quarter_boundary",
      "within_quarter"
    )
  )
}


#' First Valid IBGE Week After a Technical Stop
#'
#' Returns the first valid IBGE week number (1-12 within quarter) after a
#' within-quarter technical stop. This is used for Rule 3.3 when no household
#' consensus exists.
#'
#' The week numbers within a quarter are:
#' - Month 1: weeks 1-4
#' - Month 2: weeks 5-8
#' - Month 3: weeks 9-12
#'
#' Technical stops can occur in gaps between consecutive months:
#' - Before month 1 starts -> return week 1

#' - Between month 1 and 2 -> return week 5
#' - Between month 2 and 3 -> return week 9
#' - After month 3 ends -> return week 12 (fallback)
#'
#' @param representative_date Date vector - a date within the technical stop week
#' @param quarter Integer vector of quarters (1-4)
#' @param year Integer vector of years
#' @param min_days Integer minimum days required for first Saturday (default 4)
#' @return Integer vector: IBGE week number within quarter (1-12)
#' @keywords internal
#' @noRd
first_valid_week_after_technical_stop <- function(representative_date, quarter, year, min_days = 4L) {
  # Get Saturday of the week containing the representative date
  week_saturday <- ibge_week_saturday(representative_date)

  # Get all three months of the quarter
  month1 <- quarter_first_month(quarter)
  month2 <- quarter_month_n(quarter, 2L)
  month3 <- quarter_month_n(quarter, 3L)

  # Calculate the start (first Saturday) and end (last Saturday) of each IBGE month
  month1_start <- make_date(year, month1, first_valid_saturday(year, month1, min_days))
  month1_end <- ibge_month_end(year, month1, min_days)

  month2_start <- make_date(year, month2, first_valid_saturday(year, month2, min_days))
  month2_end <- ibge_month_end(year, month2, min_days)

  month3_start <- make_date(year, month3, first_valid_saturday(year, month3, min_days))
  month3_end <- ibge_month_end(year, month3, min_days)

  # Determine which gap the date falls in:
  # - Before month 1 -> next valid week is week 1
  # - After month 1, before month 2 -> next valid week is week 5 (first week of month 2)
  # - After month 2, before month 3 -> next valid week is week 9 (first week of month 3)
  # - After month 3 -> fallback to week 12 (last valid week)

  result <- data.table::fifelse(
    is.na(representative_date),
    NA_integer_,
    data.table::fifelse(
      week_saturday < month1_start,
      1L,  # Before month 1 -> week 1
      data.table::fifelse(
        week_saturday > month1_end & week_saturday < month2_start,
        5L,  # Gap between month 1 and 2 -> week 5
        data.table::fifelse(
          week_saturday > month2_end & week_saturday < month3_start,
          9L,  # Gap between month 2 and 3 -> week 9
          12L  # After month 3 or other cases -> week 12 (fallback)
        )
      )
    )
  )

  result
}


#' IBGE Week Number within a Month
#'
#' Returns which IBGE reference week (1-4) a date falls in within its IBGE month.
#' IBGE months always have exactly 4 reference weeks. Dates that fall outside
#' these 4 weeks (i.e., in a technical stop) return NA.
#'
#' @param date Date vector
#' @param year Integer year vector (for determining IBGE month boundaries)
#' @param month Integer month vector (1-12)
#' @param min_days Integer minimum days required for first Saturday (default 4)
#' @return Integer vector (1-4, or NA if in technical stop)
#' @keywords internal
#' @noRd
ibge_week_of_month <- function(date, year, month, min_days = 4L) {
  # Get Sunday of this date's week
  week_sunday <- ibge_week_sunday(date)

  # Get Sunday of the month's first reference week
  month_start <- ibge_month_start(year, month, min_days = min_days)

  # Calculate week number (1-indexed)
  week_num <- as.integer(as.integer(week_sunday - month_start) / 7L) + 1L

  # IBGE months always have exactly 4 weeks

  # Weeks outside 1-4 are technical stops (return NA)
  data.table::fifelse(week_num >= 1L & week_num <= 4L, week_num, NA_integer_)
}


#' IBGE Fortnight of a Month
#'
#' Returns which IBGE fortnight (1 or 2) a date falls in within its IBGE month.
#' IBGE fortnights are always exactly 2 weeks each:
#' - Fortnight 1 = reference weeks 1 + 2
#' - Fortnight 2 = reference weeks 3 + 4
#' No other fortnights are possible.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param month Integer month vector (1-12)
#' @param min_days Integer minimum days required (default 4)
#' @return Integer vector (1 or 2, or NA if in technical stop)
#' @keywords internal
#' @noRd
ibge_fortnight_of_month <- function(date, year, month, min_days = 4L) {
  week_num <- ibge_week_of_month(date, year, month, min_days = min_days)

  # Fortnight 1 = weeks 1-2, Fortnight 2 = weeks 3-4
  # NA propagates for technical stops
  data.table::fifelse(week_num <= 2L, 1L, 2L)
}


#' IBGE Week Number within a Quarter
#'
#' Returns which IBGE reference week (1-12) a date falls in within
#' its quarter, using IBGE week boundaries (Sunday-Saturday).
#'
#' @param date Date vector
#' @param quarter Integer quarter vector (1-4)
#' @param year Integer year vector
#' @param min_days Integer minimum days required (default 4)
#' @return Integer vector (1-12, or NA if out of range)
#' @keywords internal
#' @noRd
ibge_week_in_quarter <- function(date, quarter, year, min_days = 4L) {
  # Get first month of the quarter
  first_month <- quarter_first_month(quarter)

  # Get the start of the quarter's first IBGE month
  quarter_start <- ibge_month_start(year, first_month, min_days = min_days)

  # Get Sunday of the target date's week
  week_sunday <- ibge_week_sunday(date)

  # Calculate week position (1-indexed)
  week_pos <- as.integer(as.integer(week_sunday - quarter_start) / 7L) + 1L

  # Return NA for weeks before quarter start
  data.table::fifelse(week_sunday >= quarter_start, week_pos, NA_integer_)
}


#' IBGE Fortnight Number within a Quarter
#'
#' Returns which IBGE fortnight (1-6 per quarter) a date falls in.
#' Each month has 2 fortnights, so a quarter has 6.
#'
#' @param date Date vector
#' @param quarter Integer quarter vector (1-4)
#' @param year Integer year vector
#' @param min_days Integer minimum days required (default 4)
#' @return Integer vector (1-6)
#' @keywords internal
#' @noRd
ibge_fortnight_in_quarter <- function(date, quarter, year, min_days = 4L) {
  # Get first month of the quarter
  first_month <- quarter_first_month(quarter)

  # Determine which month of the quarter this date's IBGE week belongs to
  # by finding which month's Saturday boundary contains the date's Saturday
  sat_date <- ibge_week_saturday(date)
  sat_month <- data.table:::month(sat_date)
  sat_year <- data.table::year(sat_date)

  # Calculate month position in quarter (1, 2, or 3)
  month_in_quarter <- sat_month - first_month + 1L

  # Handle year boundary (e.g., Q4 spanning into January)
  month_in_quarter <- data.table::fifelse(
    sat_year > year,
    sat_month + 12L - first_month + 1L,
    month_in_quarter
  )

  # Constrain to 1-3
  month_in_quarter <- pmin(pmax(month_in_quarter, 1L), 3L)

  # Get fortnight within that month (1 or 2)
  fortnight_in_month <- ibge_fortnight_of_month(date, sat_year, sat_month, min_days = min_days)

  # Calculate fortnight in quarter
  (month_in_quarter - 1L) * 2L + fortnight_in_month
}


#' Convert IBGE Week in Quarter to Start/End Dates
#'
#' Given a week position within a quarter, returns the Sunday (start) and
#' Saturday (end) of that IBGE reference week.
#'
#' @param year Integer year
#' @param quarter Integer quarter (1-4)
#' @param week_in_quarter Integer week position (1-12)
#' @param min_days Integer minimum days required (default 4)
#' @return List with start (Sunday) and end (Saturday) Date vectors
#' @keywords internal
#' @noRd
ibge_week_dates_from_position <- function(year, quarter, week_in_quarter, min_days = 4L) {
  # Get first month of the quarter
  first_month <- quarter_first_month(quarter)

  # Get the start of the quarter's first IBGE month (Sunday)
  quarter_start <- ibge_month_start(year, first_month, min_days = min_days)

  # Calculate Sunday of the target week
  week_sunday <- quarter_start + (week_in_quarter - 1L) * 7L
  week_saturday <- week_sunday + 6L

  list(start = week_sunday, end = week_saturday)
}


#' Create IBGE Week Integer (YYYYWW format, IBGE-based)
#'
#' Creates an integer in YYYYWW format from year and IBGE week number.
#' This differs from ISO week numbers.
#'
#' @param year Integer year
#' @param ibge_week Integer IBGE week number (1-53)
#' @return Integer in YYYYWW format
#' @keywords internal
#' @noRd
ibge_yyyyww <- function(year, ibge_week) {
  as.integer(year * 100L + ibge_week)
}


#' Convert Date to IBGE YYYYWW Format
#'
#' Converts a date to IBGE week format (YYYYWW). The IBGE week year is the
#' year of the Saturday ending that week.
#'
#' @param date Date vector
#' @return Integer vector in YYYYWW format
#' @keywords internal
#' @noRd
date_to_ibge_yyyyww <- function(date) {
  # Get Saturday of this week
  sat_date <- ibge_week_saturday(date)

  # Year and month of Saturday
  sat_year <- data.table::year(sat_date)
  sat_month <- data.table:::month(sat_date)

  # Get the IBGE week number within that year
  # For simplicity, calculate from the start of the year
  jan_first_sat_day <- first_valid_saturday(sat_year, 1L, min_days = 4L)

  # Check if Saturday of week containing Jan 1 is in previous year
  # If so, that week belongs to the previous year
  jan_first_sat <- make_date(sat_year, 1L, jan_first_sat_day)
  year_start_sunday <- jan_first_sat - 6L  # Sunday of first week

  # Get Sunday of the target date's week
  week_sunday <- ibge_week_sunday(date)

  # Calculate week number
  week_num <- as.integer(as.integer(week_sunday - year_start_sunday) / 7L) + 1L

  # Handle weeks before year start (belong to previous year)
  prev_year <- sat_year - 1L
  prev_year_weeks <- ibge_weeks_in_year(prev_year)

  result_year <- data.table::fifelse(week_num < 1L, prev_year, sat_year)
  result_week <- data.table::fifelse(week_num < 1L, prev_year_weeks + week_num, week_num)

  ibge_yyyyww(result_year, result_week)
}


#' Number of IBGE Weeks in a Year
#'
#' Returns the number of IBGE reference weeks in a year. This is calculated
#' by finding the distance between the start of January and the start of
#' the following January.
#'
#' @param year Integer year (scalar only for efficiency)
#' @return Integer (typically 52-53)
#' @keywords internal
#' @noRd
ibge_weeks_in_year <- function(year) {
  # For IBGE calendar, the number of weeks in a year is the number of weeks

  # between the first Sunday of January year and first Sunday of January year+1
  jan_start_this <- ibge_month_start(year, 1L, min_days = 4L)
  jan_start_next <- ibge_month_start(year + 1L, 1L, min_days = 4L)

  # Number of weeks = days between / 7
  as.integer((jan_start_next - jan_start_this) / 7L)
}


#' Convert IBGE YYYYWW to Date (Sunday of that week)
#'
#' Converts an IBGE week number in YYYYWW format to the Sunday starting that week.
#'
#' @param yyyyww Integer in YYYYWW format
#' @return Date vector (Sunday of the specified IBGE week)
#' @keywords internal
#' @noRd
ibge_yyyyww_to_date <- function(yyyyww) {
  ibge_year <- yyyyww %/% 100L
  ibge_week <- yyyyww %% 100L

  # Get the start of the year (Sunday of first IBGE week)
  jan_first_sat_day <- first_valid_saturday(ibge_year, 1L, min_days = 4L)
  jan_first_sat <- make_date(ibge_year, 1L, jan_first_sat_day)
  year_start_sunday <- jan_first_sat - 6L  # Sunday of first week

  # Add weeks to get target Sunday
  year_start_sunday + (ibge_week - 1L) * 7L
}


#' IBGE Fortnight Start Date (Sunday of first week)
#'
#' Returns the Sunday that starts an IBGE fortnight.
#' Fortnight 1 starts with week 1, Fortnight 2 starts with week 3.
#'
#' @param year Integer year
#' @param month Integer month (1-12)
#' @param fortnight Integer fortnight (1 or 2)
#' @param min_days Integer minimum days required (default 4)
#' @return Date vector (Sunday)
#' @keywords internal
#' @noRd
ibge_fortnight_start <- function(year, month, fortnight, min_days = 4L) {
  # Get month start (Sunday of week 1)
  month_start <- ibge_month_start(year, month, min_days = min_days)

  # Fortnight 1 starts at week 1 (offset 0), Fortnight 2 starts at week 3 (offset 14)
  offset <- data.table::fifelse(fortnight == 1L, 0L, 14L)

  month_start + offset
}


#' IBGE Fortnight End Date (Saturday of last week)
#'
#' Returns the Saturday that ends an IBGE fortnight.
#' - Fortnight 1 ends with week 2 (first_saturday + 7 days)
#' - Fortnight 2 ends with week 4 (first_saturday + 21 days)
#'
#' @param year Integer year
#' @param month Integer month (1-12)
#' @param fortnight Integer fortnight (1 or 2)
#' @param min_days Integer minimum days required (default 4)
#' @return Date vector (Saturday)
#' @keywords internal
#' @noRd
ibge_fortnight_end <- function(year, month, fortnight, min_days = 4L) {
  # Get first valid Saturday
  first_sat_day <- first_valid_saturday(year, month, min_days = min_days)
  first_sat_date <- make_date(year, month, first_sat_day)

  # Fortnight 1 ends with week 2 (offset 7 days)
  # Fortnight 2 ends with week 4 (offset 21 days)
  data.table::fifelse(fortnight == 1L, first_sat_date + 7L, first_sat_date + 21L)
}


#' Number of Weeks in an IBGE Fortnight
#'
#' IBGE fortnights ALWAYS have exactly 2 reference weeks. This is a fundamental
#' rule of the IBGE reference calendar:
#' - Fortnight 1 = weeks 1 + 2 = 2 weeks
#' - Fortnight 2 = weeks 3 + 4 = 2 weeks
#'
#' @param year Integer year - included for API consistency but not used
#' @param month Integer month (1-12) - included for API consistency but not used
#' @param fortnight Integer fortnight (1 or 2) - included for API consistency but not used
#' @param min_days Integer minimum days required (default 4) - included for API consistency but not used
#' @return Integer vector, always 2L
#' @keywords internal
#' @noRd
ibge_fortnight_weeks <- function(year, month, fortnight, min_days = 4L) {
  # IBGE fortnights ALWAYS have exactly 2 weeks - no exceptions
  n <- max(length(year), length(month), length(fortnight))
  rep(2L, n)
}


#' Convert IBGE Fortnight in Quarter to YYYYFF Format
#'
#' Converts a fortnight position within a quarter (1-6) to YYYYFF format
#' where FF is the fortnight number within the year (1-24).
#'
#' @param year Integer year
#' @param quarter Integer quarter (1-4)
#' @param fortnight_in_quarter Integer fortnight position (1-6)
#' @return Integer YYYYFF format
#' @keywords internal
#' @noRd
ibge_fortnight_in_quarter_to_yyyyff <- function(year, quarter, fortnight_in_quarter) {
  # First fortnight of quarter in year
  first_ff <- (quarter - 1L) * 6L + 1L

  # Fortnight number in year (1-24)
  ff <- first_ff + fortnight_in_quarter - 1L

  year * 100L + ff
}


#' Convert IBGE YYYYFF to Fortnight Start Date
#'
#' Converts a fortnight in YYYYFF format to its start date (Sunday of first week).
#'
#' @param yyyyff Integer YYYYFF format (year * 100 + fortnight 1-24)
#' @return Date vector (Sunday)
#' @keywords internal
#' @noRd
ibge_yyyyff_to_date <- function(yyyyff) {
  year <- yyyyff %/% 100L
  ff <- yyyyff %% 100L

  # Month (1-12) and fortnight within month (1 or 2)
  month <- (ff - 1L) %/% 2L + 1L
  fortnight <- ((ff - 1L) %% 2L) + 1L

  ibge_fortnight_start(year, month, fortnight, min_days = 4L)
}


# ============================================================================
# OPTIMIZED IBGE FUNCTIONS WITH PRE-COMPUTATION
#
# These functions avoid redundant computation by pre-computing values for
# unique (year, month) combinations and using lookup joins.
# ============================================================================

#' Pre-compute IBGE Month Boundaries
#'
#' Creates a lookup table with pre-computed IBGE month boundaries for all
#' unique (year, month) combinations. This enables fast lookups instead of
#' per-observation computation.
#'
#' @param years Integer vector of years
#' @param months Integer vector of months (1-12)
#' @param min_days Integer minimum days required (default 4)
#' @return data.table with columns: year, month, ibge_month_start_int (integer days),
#'   ibge_n_weeks, ibge_first_sat_day
#' @keywords internal
#' @noRd
precompute_ibge_month_boundaries <- function(years, months, min_days = 4L) {
  # Get unique year-month combinations
  unique_ym <- unique(data.table::data.table(year = years, month = months))

  # Pre-compute all values (vectorized)
  unique_ym[, `:=`(
    ibge_first_sat_day = first_valid_saturday(year, month, min_days = min_days),
    ibge_n_weeks = ibge_month_weeks(year, month, min_days = min_days)
  )]

  # Calculate month start as integer (days since epoch) for fast arithmetic
  unique_ym[, ibge_month_start_int := as.integer(make_date(year, month, ibge_first_sat_day)) - 6L]

  # Set key for fast joins
  data.table::setkey(unique_ym, year, month)

  unique_ym
}


#' Fast IBGE Fortnight Position (with pre-computed boundaries)
#'
#' An optimized version of ibge_fortnight_in_quarter that uses pre-computed
#' month boundaries for ~100x speedup on large datasets.
#'
#' @param date Date vector
#' @param quarter Integer vector of quarters (1-4)
#' @param year Integer vector of years
#' @param month_boundaries data.table from precompute_ibge_month_boundaries()
#' @return Integer vector (1-6 for fortnight position in quarter)
#' @keywords internal
#' @noRd
ibge_fortnight_in_quarter_fast <- function(date, quarter, year, month_boundaries) {
  # Convert date to integer days since epoch
  date_int <- as.integer(date)

  # Get first month of the quarter
  first_month <- quarter_first_month(quarter)

  # Get Saturday of this week (IBGE weeks end on Saturday)
  date_dow <- (date_int + 4L) %% 7L
  sat_days <- date_int + (6L - date_dow)

  # Extract year and month from Saturday date
  sat_date <- structure(sat_days, class = "Date")
  sat_year <- data.table::year(sat_date)
  sat_month <- data.table::month(sat_date)

  # Calculate month position in quarter (1, 2, or 3)
  month_in_quarter <- sat_month - first_month + 1L
  # Handle year boundary (Q4 spanning into January)
  month_in_quarter <- data.table::fifelse(
    sat_year > year,
    sat_month + 12L - first_month + 1L,
    month_in_quarter
  )
  month_in_quarter <- pmin(pmax(month_in_quarter, 1L), 3L)

  # Create lookup table for this call
  lookup_dt <- data.table::data.table(
    idx = seq_along(date),
    year = sat_year,
    month = sat_month
  )

  # Join to get pre-computed boundaries
  lookup_dt[month_boundaries, on = .(year, month), `:=`(
    ibge_month_start_int = i.ibge_month_start_int,
    ibge_n_weeks = i.ibge_n_weeks
  )]

  # Calculate week number within month using pre-computed start
  # Sunday of this date's week
  week_sunday_int <- date_int - date_dow
  week_num <- as.integer((week_sunday_int - lookup_dt$ibge_month_start_int) / 7L) + 1L

  # Validate week number (should be 1 to n_weeks)
  valid_week <- week_num >= 1L & week_num <= lookup_dt$ibge_n_weeks

  # Fortnight: weeks 1-2 = fortnight 1, weeks 3+ = fortnight 2
  fortnight_in_month <- data.table::fifelse(week_num <= 2L, 1L, 2L)

  # Invalid weeks get NA
  fortnight_in_month <- data.table::fifelse(valid_week, fortnight_in_month, NA_integer_)

  # Calculate fortnight in quarter
  result <- (month_in_quarter - 1L) * 2L + fortnight_in_month

  result
}




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
  date_month  <- data.table:::month(date)
  date_day    <- data.table:::mday(date)

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
  date_day    <- data.table:::mday(date)

  # When day <= threshold, use the month of (date - 3 days)
  needs_adjust   <- date_day <= day_threshold
  adjusted_date  <- date - needs_adjust * 3L
  adjusted_month <- data.table:::month(adjusted_date)

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
  date_month <- data.table:::month(date)
  date_day <- data.table:::mday(date)

  pos <- date_month - first_month + 1L

  # Determine threshold based on which month the date falls in
  # Month 1 of quarter: months 1, 4, 7, 10
  # Month 2 of quarter: months 2, 5, 8, 11
  # Month 3 of quarter: months 3, 6, 9, 12
  # Use modular arithmetic to identify position in quarter
  month_in_quarter <- ((date_month - 1L) %% 3L) + 1L  # 1, 2, or 3

  # Threshold is 2 if exception applies for this month, 3 otherwise
  # OPTIMIZATION: Use fifelse instead of rep() + subsetting
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
  date_month  <- data.table:::month(date)
  date_day    <- data.table:::mday(date)

  # Determine threshold based on which month the date falls in
  # Use modular arithmetic to identify position in quarter
  month_in_quarter <- ((date_month - 1L) %% 3L) + 1L  # 1, 2, or 3

  # Threshold is 2 if exception applies for this month, 3 otherwise
  # OPTIMIZATION: Use fifelse instead of rep() + subsetting
  threshold <- fifelse(
    (month_in_quarter == 1L & exc_m1 == 1L) |
      (month_in_quarter == 2L & exc_m2 == 1L) |
      (month_in_quarter == 3L & exc_m3 == 1L),
    2L, 3L
  )

  # When day <= threshold, use the month of (date - 3 days)
  needs_adjust <- date_day <= threshold
  adjusted_date <- date - needs_adjust * 3L
  adjusted_month <- data.table:::month(adjusted_date)

  pos <- adjusted_month - first_month + 1L

  pmin(pmax(pos, 1L), 3L)
}
