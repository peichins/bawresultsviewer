# In a file named test-functions.R (or similar) within a 'tests/testthat' directory
library(testthat)
library(dplyr)
library(lubridate)

test_data <- dplyr::tribble(
  ~label, ~score, ~timestamp, ~site,
  "species1", 0.8, ymd_hms("2024-01-10 12:30:00"), "siteA",
  "species2", 0.9, ymd_hms("2024-01-15 08:45:00"), "siteB",
  "species1", 0.7, ymd_hms("2024-02-20 15:10:00"), "siteA",
  "species3", 0.6, ymd_hms("2024-03-05 11:22:00"), "siteC",
  "species3", 0.6, ymd_hms("2024-03-05 11:23:26"), "siteC"
)


# Tests for selectDataTibble

test_that("selectDataTibble filters correctly", {
  test_input <- list(
    speciesInput = c("species1", "species2"),
    scoreInput = c(0.7, 0.9),
    dateInput = c(ymd("2024-01-01"), ymd("2024-01-31")),
    siteInput = c("siteA")  # Filter by site
  )

  filtered_data <- selectDataTibble(test_data, test_input)

  expected_data <- test_data[c(3),]

  expect_equal(nrow(filtered_data), 1)
  expect_equal(filtered_data$label, "species1")
})


# Tests for selectTopScoringTibble
test_that("selectTopScoringTibble returns correct top-scoring observations", {
  test_input <- list(intervalInput = "day")

  top_scoring_data <- selectTopScoringTibble(test_data, test_input)

  expected_data <- test_data[c(1,2,3,4),]

  # Check that the top scoring data is equal to the expected data
  expect_equal(top_scoring_data, expected_data)
})
