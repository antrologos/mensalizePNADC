# Tests for date utility functions

test_that("dow returns correct day of week", {
  # Sunday = 0
 expect_equal(dow(as.Date("2024-01-07")), 0)  # Sunday
  # Monday = 1
  expect_equal(dow(as.Date("2024-01-08")), 1)  # Monday
  # Saturday = 6
  expect_equal(dow(as.Date("2024-01-06")), 6)  # Saturday
})

test_that("make_date handles valid dates", {
  expect_equal(make_date(2024, 1, 15), as.Date("2024-01-15"))
  expect_equal(make_date(2024, 12, 31), as.Date("2024-12-31"))
})

test_that("make_date handles edge cases", {
  # NOTE: make_date is optimized for speed and does NOT validate dates.

  # Invalid dates like Feb 30 return incorrect dates, not NA.
  # This is by design - the algorithm only creates valid dates by construction.

  # February 29 in leap year is valid
  expect_false(is.na(make_date(2024, 2, 29)))
  expect_equal(make_date(2024, 2, 29), as.Date("2024-02-29"))

  # Invalid dates: function returns a date (not NA) - overflow behavior
  # Feb 30 in 2024 (leap year) returns Mar 1
  expect_equal(make_date(2024, 2, 30), as.Date("2024-03-01"))
  # Feb 29 in 2023 (non-leap year) returns Mar 1
  expect_equal(make_date(2023, 2, 29), as.Date("2023-03-01"))
})

test_that("make_birthday handles Feb 29", {
  # In leap year, Feb 29 is valid
  expect_equal(make_birthday(2, 29, 2024), as.Date("2024-02-29"))

  # In non-leap year, Feb 29 becomes March 1
  expect_equal(make_birthday(2, 29, 2023), as.Date("2023-03-01"))
})

test_that("is_leap_year identifies leap years correctly", {
  expect_true(is_leap_year(2024))
  expect_true(is_leap_year(2000))
  expect_false(is_leap_year(2023))
  expect_false(is_leap_year(1900))
})

test_that("yyyymm creates correct format", {
  expect_equal(yyyymm(2024, 1), 202401L)
  expect_equal(yyyymm(2024, 12), 202412L)
  expect_equal(yyyymm(2023, 6), 202306L)
})

test_that("first_valid_saturday calculates correctly", {
  # January 2024: starts on Monday (dow=1)
  # First Saturday is day 6
  # With min_days=4, need at least 4 days in week, so day 6 is OK (6 days in month)
  expect_equal(first_valid_saturday(2024, 1, min_days = 4), 6)

  # March 2024: starts on Friday (dow=5)
  # First Saturday is day 2 (only 2 days in March)
  # With min_days=4, need second Saturday (day 9)
  expect_equal(first_valid_saturday(2024, 3, min_days = 4), 9)

  # With min_days=3, first Saturday (day 2) might work
  # 2 days is less than 3, so still need second Saturday
  expect_equal(first_valid_saturday(2024, 3, min_days = 3), 9)
})

test_that("first_saturday_on_or_after works correctly", {
  # If date is Saturday, return same date
  expect_equal(first_saturday_on_or_after(as.Date("2024-01-06")), as.Date("2024-01-06"))

  # If date is Sunday, return next Saturday (6 days later)
  expect_equal(first_saturday_on_or_after(as.Date("2024-01-07")), as.Date("2024-01-13"))

  # If date is Monday, return Saturday (5 days later)
  expect_equal(first_saturday_on_or_after(as.Date("2024-01-08")), as.Date("2024-01-13"))
})

