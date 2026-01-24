# Helper functions for creating realistic PNADC test data
# These functions ensure consistency between birthday and age variables

#' Create Realistic PNADC Test Data
#'
#' Generates synthetic PNADC data with internally consistent fields.
#' Age is correctly derived from birth date and survey date.
#'
#' @param n_quarters Number of quarters to generate (default 4)
#' @param n_upas Number of UPAs per quarter (default 10)
#' @param persons_per_household Average persons per household (default 3)
#' @param start_year Starting year (default 2023)
#' @param seed Random seed for reproducibility
#' @return data.table with realistic PNADC structure
create_realistic_pnadc <- function(n_quarters = 4,
                                    n_upas = 10,
                                    persons_per_household = 3,
                                    start_year = 2023L,
                                    seed = 42L) {
  set.seed(seed)

  # Generate quarter combinations
  quarters <- data.table::data.table(
    q = seq_len(n_quarters)
  )
  quarters[, `:=`(
    Ano = start_year + (q - 1L) %/% 4L,
    Trimestre = ((q - 1L) %% 4L) + 1L
  )]

  # Generate UPAs (each UPA has a panel group V1014)
  upas <- data.table::data.table(
    UPA = seq_len(n_upas),
    V1014 = sample(1:8, n_upas, replace = TRUE)  # Panel groups 1-8
  )

  # Generate households per UPA
  households_per_upa <- sample(2:5, n_upas, replace = TRUE)
  households <- data.table::rbindlist(lapply(seq_len(n_upas), function(u) {
    data.table::data.table(
      UPA = u,
      V1008 = seq_len(households_per_upa[u])
    )
  }))
  households <- merge(households, upas, by = "UPA")

  # Generate persons per household
  persons <- data.table::rbindlist(lapply(seq_len(nrow(households)), function(h) {
    n_persons <- max(1L, rpois(1, persons_per_household))
    hh <- households[h]
    data.table::data.table(
      UPA = hh$UPA,
      V1008 = hh$V1008,
      V1014 = hh$V1014,
      V2003 = seq_len(n_persons)
    )
  }))

  # Generate birth dates with realistic age distribution
  # PNADC covers all ages, with typical Brazilian population distribution
  n_persons <- nrow(persons)

  # Generate birth years (wider range for realistic age distribution)
  # Ages typically 0-100, median around 35
  birth_years <- start_year - sample(0:85, n_persons, replace = TRUE,
                                      prob = dnorm(0:85, mean = 35, sd = 20) + 0.01)
  birth_months <- sample(1:12, n_persons, replace = TRUE)
  birth_days <- sapply(birth_months, function(m) {
    max_day <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[m]
    sample(1:max_day, 1)
  })

  persons[, `:=`(
    V20082 = as.integer(birth_years),
    V20081 = as.integer(birth_months),
    V2008 = as.integer(birth_days)
  )]

  # Properly create the cross-product of persons and quarters
  dt <- data.table::rbindlist(lapply(seq_len(nrow(quarters)), function(q) {
    qdata <- quarters[q]
    persons_copy <- data.table::copy(persons)
    persons_copy[, `:=`(
      Ano = qdata$Ano,
      Trimestre = qdata$Trimestre
    )]
    persons_copy
  }))

  # Calculate age based on survey date
  # Survey is conducted in months corresponding to quarter
  # Q1 = Jan-Mar, Q2 = Apr-Jun, Q3 = Jul-Sep, Q4 = Oct-Dec
  # Assume interview at middle of quarter (2nd month)
  dt[, survey_month := (Trimestre - 1L) * 3L + 2L]

  # Calculate age correctly
  dt[, V2009 := {
    age <- Ano - V20082
    # Adjust if birthday hasn't occurred yet this year
    had_birthday <- (survey_month > V20081) |
                    (survey_month == V20081 & 15L >= V2008)  # Assume interview on 15th
    age <- age - as.integer(!had_birthday)
    pmax(0L, age)  # Ensure non-negative
  }]

  # Add some realistic NA values for unknown birthdays (~2%)
  unknown_idx <- sample(seq_len(nrow(dt)), size = ceiling(nrow(dt) * 0.02))
  dt[unknown_idx, `:=`(
    V2008 = 99L,    # PNADC code for unknown
    V20081 = 99L,
    V20082 = 9999L
  )]

  # Clean up
  dt[, survey_month := NULL]

  # Reorder columns to match PNADC structure
  data.table::setcolorder(dt, c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
                                 "V2008", "V20081", "V20082", "V2009"))

  dt
}


#' Create Minimal Valid PNADC Data
#'
#' Creates a minimal dataset that passes validation for testing.
#'
#' @param n Number of observations
#' @param year Survey year
#' @param quarter Quarter (1-4)
#' @return data.table with minimal required columns
create_minimal_pnadc <- function(n = 10L, year = 2023L, quarter = 1L) {
  data.table::data.table(
    Ano = rep(year, n),
    Trimestre = rep(quarter, n),
    UPA = seq_len(n),
    V1008 = rep(1L, n),
    V1014 = sample(1:8, n, replace = TRUE),
    V2003 = rep(1L, n),
    V2008 = sample(1:28, n, replace = TRUE),
    V20081 = sample(1:12, n, replace = TRUE),
    V20082 = sample(1970:2005, n, replace = TRUE),
    V2009 = sample(18:65, n, replace = TRUE)
  )
}


#' Create PNADC Data with Calibration Columns
#'
#' Creates test data including columns needed for weight calibration.
#'
#' @param n Number of observations
#' @param year Survey year
#' @param quarter Quarter (1-4)
#' @return data.table with calibration-ready structure
create_pnadc_for_calibration <- function(n = 100L, year = 2023L, quarter = 1L) {
  dt <- create_minimal_pnadc(n, year, quarter)

  # Add calibration columns
  dt[, `:=`(
    UF = sample(11:53, n, replace = TRUE),  # Brazilian state codes
    V1028 = runif(n, 500, 2000),            # Quarterly weights
    V1032 = runif(n, 500, 2000),            # Annual weights
    posest = sample(1:500, n, replace = TRUE),
    posest_sxi = sample(100:999, n, replace = TRUE)
  )]

  dt
}


#' Create Stacked Multi-Quarter PNADC Data
#'
#' Creates data spanning multiple quarters for testing cross-quarter aggregation.
#'
#' @param n_quarters Number of quarters
#' @param start_year Starting year
#' @param n_upas Number of unique UPAs
#' @return data.table with stacked quarterly data
create_stacked_pnadc <- function(n_quarters = 8L, start_year = 2022L, n_upas = 20L) {
  create_realistic_pnadc(
    n_quarters = n_quarters,
    n_upas = n_upas,
    start_year = start_year
  )
}


#' Create Monthly Population Targets for Testing
#'
#' Creates mock monthly population targets matching SIDRA format.
#'
#' @param start_yyyymm Starting month (YYYYMM format)
#' @param n_months Number of months
#' @param base_pop Base population (in thousands)
#' @return data.table with monthly population targets
create_monthly_targets <- function(start_yyyymm = 202301L,
                                    n_months = 12L,
                                    base_pop = 215000) {
  start_year <- start_yyyymm %/% 100L
  start_month <- start_yyyymm %% 100L

  months <- data.table::data.table(
    month_num = seq_len(n_months)
  )
  months[, `:=`(
    year = start_year + (start_month + month_num - 2L) %/% 12L,
    month = ((start_month + month_num - 2L) %% 12L) + 1L
  )]
  months[, ref_month_yyyymm := year * 100L + month]

  # Add slight growth trend
  months[, m_populacao := base_pop * (1 + 0.001 * (month_num - 1))]

  months[, .(ref_month_yyyymm, m_populacao)]
}
