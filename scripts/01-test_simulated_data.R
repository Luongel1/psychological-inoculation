#### Preamble ####
# Purpose: Tests the structure and validity of the simulated inoculation dataset
# Author: Elizabeth Luong
# Date: 3 December 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
#   - The `tidyverse`, `testthat` packages must be installed and loaded
#   - Simulated data must have been created or loaded


#### Workspace setup ####
library(tidyverse)
library(testthat)

# Load the simulated data
simulated_data <- read_csv("data/00-simulated_data/simulated_data.csv")

#### Test data ####

# Test if the data was successfully loaded
test_that("dataset was successfully loaded", {
  expect_true(exists("simulated_data"))
})

# Check the number of rows and columns
test_that("dataset has 1000 rows", {
  expect_equal(nrow(simulated_data), 1000)
})

test_that("dataset has 12 columns", {
  expect_equal(ncol(simulated_data), 12)
})

# Test if Group contains only "Treatment" or "Control"
test_that("'Group' contains only valid values", {
  expect_true(all(simulated_data$Group %in% c("Treatment", "Control")))
})

# Check for missing values
test_that("dataset contains no missing values", {
  expect_true(all(!is.na(simulated_data)))
})

# Test if Gender contains valid categories
test_that("'Gender' contains valid values", {
  expect_true(all(simulated_data$Gender %in% c("Male", "Female", "Other")))
})

# Test if Education contains valid categories
test_that("'Education' contains valid values", {
  expect_true(all(simulated_data$Education %in% c("High School", "Bachelor's", "Master's", "Doctorate")))
})

# Test if Political_Affiliation contains valid categories
test_that("'Political_Affiliation' contains valid values", {
  expect_true(all(simulated_data$Political_Affiliation %in% c("Democrat", "Republican", "Independent", "Other")))
})

# Check if Minority_Status contains only "Yes" or "No"
test_that("'Minority_Status' contains valid values", {
  expect_true(all(simulated_data$Minority_Status %in% c("Yes", "No")))
})

# Check if Neutral_Score and Manipulative_Score are numeric
test_that("'Neutral_Score' and 'Manipulative_Score' are numeric", {
  expect_true(is.numeric(simulated_data$Neutral_Score))
  expect_true(is.numeric(simulated_data$Manipulative_Score))
})

# Check if Discernment_Score is correctly calculated
test_that("'Discernment_Score' is correctly calculated", {
  # Recalculate Discernment_Score using the full formula
  recalculated_score <- simulated_data$Neutral_Score - simulated_data$Manipulative_Score +
    0.5 * simulated_data$Analytical_Thinking * ifelse(simulated_data$Group == "Treatment", 1.5, 1)
  
  # Check if the simulated Discernment_Score matches the recalculated value
  expect_true(all.equal(
    simulated_data$Discernment_Score,
    recalculated_score,
    tolerance = 1e-6
  ))
})

# Check if Analytical_Thinking, Bullshit_Receptivity, and Conspiracy_Belief are numeric
test_that("psychological scales are numeric", {
  expect_true(is.numeric(simulated_data$Analytical_Thinking))
  expect_true(is.numeric(simulated_data$Bullshit_Receptivity))
  expect_true(is.numeric(simulated_data$Conspiracy_Belief))
})

# Test the ranges of psychological scores
test_that("psychological scores are within plausible ranges", {
  expect_true(all(simulated_data$Analytical_Thinking >= 0 & simulated_data$Analytical_Thinking <= 10))
  expect_true(all(simulated_data$Bullshit_Receptivity >= 0 & simulated_data$Bullshit_Receptivity <= 10))
  expect_true(all(simulated_data$Conspiracy_Belief >= 0 & simulated_data$Conspiracy_Belief <= 10))
})


# Test if Age falls within a reasonable range
test_that("'Age' is within a valid range (18 to 70)", {
  expect_true(all(simulated_data$Age >= 18 & simulated_data$Age <= 70))
})

# Test that participants in the treatment group have lower scores on the manipulative post scores compared to those in the controlled group
test_that("treatment group has lower manipulative scores", {
  treatment_scores <- simulated_data %>%
    filter(Group == "Treatment") %>%
    pull(Manipulative_Score)
  
  control_scores <- simulated_data %>%
    filter(Group == "Control") %>%
    pull(Manipulative_Score)
  
  # Perform t-test to check for significant difference
  t_test <- t.test(treatment_scores, control_scores, alternative = "less")
  
  expect_true(t_test$p.value < 0.05) # Assert significant difference
})

# Test that the discernment score is positively correlated with analytical thinking
test_that("discernment score correlates positively with analytical thinking", {
  correlation <- cor(
    simulated_data$Discernment_Score,
    simulated_data$Analytical_Thinking,
    use = "complete.obs"
  )
  expect_true(correlation > 0.3)
})

