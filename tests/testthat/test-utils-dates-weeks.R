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

test_that("vectorized ISO week functions work", {
  dates <- as.Date(c("2024-01-01", "2024-06-15", "2024-12-31"))

  # iso_week
  expect_equal(iso_week(dates), c(1L, 24L, 1L))

  # iso_year
  expect_equal(iso_year(dates), c(2024L, 2024L, 2025L))

  # date_to_yyyyww
  expect_equal(date_to_yyyyww(dates), c(202401L, 202424L, 202501L))
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
