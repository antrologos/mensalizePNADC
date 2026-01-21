# Tests for ISO week utility functions

test_that("iso_week returns correct week numbers", {
  # Week 1 of 2024 starts on Monday Jan 1
  expect_equal(iso_week(as.Date("2024-01-01")), 1)
  expect_equal(iso_week(as.Date("2024-01-07")), 1)  # Sunday of week 1
  expect_equal(iso_week(as.Date("2024-01-08")), 2)  # Monday of week 2

  # Mid-year test

  expect_equal(iso_week(as.Date("2024-06-15")), 24)

  # Last week of year
  expect_equal(iso_week(as.Date("2024-12-30")), 1)  # Week 1 of 2025
})

test_that("iso_year handles year boundaries correctly", {
  # Dec 31, 2024 is in ISO week 1 of 2025 (2024 ends on Tuesday)
  expect_equal(iso_year(as.Date("2024-12-31")), 2025)
  expect_equal(iso_year(as.Date("2024-12-30")), 2025)

  # Jan 1, 2023 is in ISO week 52 of 2022 (2023 starts on Sunday)
  expect_equal(iso_year(as.Date("2023-01-01")), 2022)

  # Jan 4 is always in week 1 of its calendar year
  expect_equal(iso_year(as.Date("2023-01-04")), 2023)
  expect_equal(iso_year(as.Date("2024-01-04")), 2024)
  expect_equal(iso_year(as.Date("2025-01-04")), 2025)

  # Jan 1, 2024 is in week 1 of 2024 (2024 starts on Monday)
  expect_equal(iso_year(as.Date("2024-01-01")), 2024)
})

test_that("yyyyww creates correct format", {
  expect_equal(yyyyww(2024, 1), 202401L)
  expect_equal(yyyyww(2024, 52), 202452L)
  expect_equal(yyyyww(2020, 53), 202053L)  # 2020 had 53 weeks
  expect_equal(yyyyww(2023, 10), 202310L)
})

test_that("date_to_yyyyww works correctly", {
  expect_equal(date_to_yyyyww(as.Date("2024-01-01")), 202401L)
  # Dec 31, 2024 is week 1 of 2025
  expect_equal(date_to_yyyyww(as.Date("2024-12-31")), 202501L)
  # Jan 1, 2023 is week 52 of 2022
  expect_equal(date_to_yyyyww(as.Date("2023-01-01")), 202252L)
})

test_that("iso_week_monday returns correct Monday", {
  # Monday should return itself
  expect_equal(iso_week_monday(as.Date("2024-01-08")), as.Date("2024-01-08"))

  # Sunday should return previous Monday
  expect_equal(iso_week_monday(as.Date("2024-01-14")), as.Date("2024-01-08"))

  # Wednesday should return Monday of same week
  expect_equal(iso_week_monday(as.Date("2024-01-10")), as.Date("2024-01-08"))

  # Saturday should return Monday of same week
  expect_equal(iso_week_monday(as.Date("2024-01-13")), as.Date("2024-01-08"))

  # Year boundary: Dec 31, 2024 (Tuesday) - Monday is Dec 30
  expect_equal(iso_week_monday(as.Date("2024-12-31")), as.Date("2024-12-30"))
})

test_that("iso_weeks_in_year returns 52 or 53", {
  # 2020 had 53 weeks (Jan 1 was Wednesday + leap year)
  expect_equal(iso_weeks_in_year(2020), 53L)

  # 2015 had 53 weeks (Jan 1 was Thursday)
  expect_equal(iso_weeks_in_year(2015), 53L)

  # 2023 has 52 weeks (Jan 1 was Sunday)
  expect_equal(iso_weeks_in_year(2023), 52L)

  # 2024 has 52 weeks (Jan 1 was Monday)
  expect_equal(iso_weeks_in_year(2024), 52L)

  # 2026 will have 53 weeks (Jan 1 is Thursday)
  expect_equal(iso_weeks_in_year(2026), 53L)
})

test_that("week_in_quarter calculates position correctly", {
  # Q1 2024 starts Jan 1 (Monday)
  # Week 1 of Q1 is Jan 1-7
  expect_equal(week_in_quarter(as.Date("2024-01-01"), 1, 2024), 1)
  expect_equal(week_in_quarter(as.Date("2024-01-07"), 1, 2024), 1)
  expect_equal(week_in_quarter(as.Date("2024-01-08"), 1, 2024), 2)
  expect_equal(week_in_quarter(as.Date("2024-01-15"), 1, 2024), 3)

  # Last full week of Q1 2024 (March 25-31)
  expect_equal(week_in_quarter(as.Date("2024-03-25"), 1, 2024), 13)

  # Q2 2024 starts April 1 (Monday)
  expect_equal(week_in_quarter(as.Date("2024-04-01"), 2, 2024), 1)
  expect_equal(week_in_quarter(as.Date("2024-04-08"), 2, 2024), 2)

  # Q3 2024 starts July 1 (Monday)
  expect_equal(week_in_quarter(as.Date("2024-07-01"), 3, 2024), 1)

  # Q4 2024 starts October 1 (Tuesday) - first Monday is Sept 30
  # So week 1 starts Oct 7 (next Monday after Oct 1)
  # Actually: Oct 1 is Tuesday, so Monday of that week is Sept 30
  # Sept 30 < Oct 1 (quarter start), so first Monday is Oct 7
  expect_equal(week_in_quarter(as.Date("2024-10-07"), 4, 2024), 1)
})

test_that("yyyyww_to_date converts correctly", {
  # Week 1 of 2024 starts Monday Jan 1
  expect_equal(yyyyww_to_date(202401), as.Date("2024-01-01"))

  # Week 2 of 2024 starts Monday Jan 8
  expect_equal(yyyyww_to_date(202402), as.Date("2024-01-08"))

  # Week 52 of 2023 (which contains Jan 1, 2023) starts Mon Dec 26, 2022
  expect_equal(yyyyww_to_date(202252), as.Date("2022-12-26"))

  # Week 1 of 2025 starts Monday Dec 30, 2024
  expect_equal(yyyyww_to_date(202501), as.Date("2024-12-30"))
})

test_that("vectorized ISO week functions work", {
  dates <- as.Date(c("2024-01-01", "2024-06-15", "2024-12-31"))

  # iso_week
  expect_equal(iso_week(dates), c(1L, 24L, 1L))

  # iso_year
  expect_equal(iso_year(dates), c(2024L, 2024L, 2025L))

  # date_to_yyyyww
  expect_equal(date_to_yyyyww(dates), c(202401L, 202424L, 202501L))

  # iso_week_monday
  expect_equal(iso_week_monday(dates),
               as.Date(c("2024-01-01", "2024-06-10", "2024-12-30")))
})

test_that("ISO week functions handle NA values",
{
  dates <- as.Date(c("2024-01-15", NA, "2024-06-15"))

  # Functions should propagate NA
  result_week <- iso_week(dates)
  expect_true(is.na(result_week[2]))
  expect_equal(result_week[1], 3L)

  result_year <- iso_year(dates)
  expect_true(is.na(result_year[2]))
  expect_equal(result_year[1], 2024L)
})
