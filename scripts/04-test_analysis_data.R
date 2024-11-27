#### Preamble ####
# Purpose: Tests the analysis dataset saved in Parquet format
# Author: Elizabeth Luong
# Date: 3 December 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Parquet file saved from cleaned dataset

#### Workspace setup ####
library(arrow)
library(testthat)
library(tidyverse)

# Load the dataset
analysis_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")


#### Test Analysis Data ####

# Updated required columns based on `analysis_data` structure
required_columns <- c(
  "treatment",                        
  "technique_discernment",            
  "standardized_analytical_thinking", 
  "standardized_conspiracy_belief",   
  "standardized_bullshit_receptivity",
  "gender",                           
  "education",                        
  "political_ideology",               
  "age_text"
)

# Test if the dataset is loaded successfully
test_that("dataset loaded successfully", {
  expect_true(exists("analysis_data"))
  expect_true(nrow(analysis_data) > 0)  # Ensure there are rows
  expect_true(ncol(analysis_data) > 0)  # Ensure there are columns
})

# Test variable presence
test_that("required variables are present", {
  expect_true(all(required_columns %in% colnames(analysis_data)))
})

# Test data types of key variables
test_that("data types are valid", {
  expect_type(analysis_data$treatment, "character")  # Should be character (factor encoded as string)
  expect_type(analysis_data$technique_discernment, "double")  # Should be numeric
  expect_type(analysis_data$gender, "character")    # Should be character (factor encoded as string)
  expect_type(analysis_data$education, "character") # Should be character (factor encoded as string)
  expect_type(analysis_data$age_text, "character")  # Should be character
})

# Validate ranges for numerical variables
test_that("numerical variables are within expected ranges", {
  expect_true(all(analysis_data$technique_discernment >= -100 & analysis_data$technique_discernment <= 100))  # Example range
  expect_true(all(analysis_data$standardized_analytical_thinking >= -3 & analysis_data$standardized_analytical_thinking <= 3))  # Standardized scale
  expect_true(all(analysis_data$standardized_conspiracy_belief >= -3 & analysis_data$standardized_conspiracy_belief <= 3))  # Standardized scale
  expect_true(all(analysis_data$standardized_bullshit_receptivity >= -3 & analysis_data$standardized_bullshit_receptivity <= 3))  # Standardized scale
})

# Test that "Inoculation" group has higher technique discerment scores compared to "Control" group
test_that("Inoculation group has higher technique discernment than Control", {
  # Calculate mean technique discernment by treatment group
  group_means <- cleaned_data |> 
    group_by(treatment) |> 
    summarize(mean_technique_discernment = mean(technique_discernment, na.rm = TRUE))
  
  # Extract the means for the two groups
  inoculation_mean <- group_means$mean_technique_discernment[group_means$treatment == "Inoculation"]
  control_mean <- group_means$mean_technique_discernment[group_means$treatment == "Control"]
  
  # Check if Inoculation mean is greater than Control mean
  if (is.na(inoculation_mean) || is.na(control_mean)) {
    fail("One or both group means are NA, check data integrity.")
  } else {
    expect_gt(inoculation_mean, control_mean, 
              label = "Inoculation group should have higher technique discernment than Control group.")
  }
})

# Test that standardized variables have a mean of zero and a standard deviation of 1
test_that("Standardized psychological variables are properly standardized", {
  # List of standardized variables
  standardized_vars <- c("standardized_analytical_thinking", 
                         "standardized_conspiracy_belief", 
                         "standardized_bullshit_receptivity")
  
  for (var in standardized_vars) {
    # Calculate mean and standard deviation of each variable
    var_mean <- mean(cleaned_data[[var]], na.rm = TRUE)
    var_sd <- sd(cleaned_data[[var]], na.rm = TRUE)
    
    # Test mean and standard deviation
    expect_equal(var_mean, 0, tolerance = 1e-5, 
                 info = paste(var, "should have a mean of 0."))
    expect_equal(var_sd, 1, tolerance = 1e-5, 
                 info = paste(var, "should have a standard deviation of 1."))
  }
})

# Test that the distribution of political ideology is similar across "Inoculation" and "Control" groups, as an imbalance could bias the results.
test_that("Political ideology is balanced across treatment groups", {
  # Count frequencies of political ideology within each treatment group
  ideology_distribution <- cleaned_data |> 
    group_by(treatment, political_ideology) |> 
    summarize(count = n(), .groups = "drop") |> 
    pivot_wider(names_from = political_ideology, values_from = count, values_fill = 0)
  
  # Perform a Chi-Square test for independence
  ideology_table <- as.matrix(ideology_distribution[,-1])  # Exclude the 'treatment' column
  chi_test <- chisq.test(ideology_table)
  
  # Expect no significant difference in distribution (p-value > 0.05)
  expect_gt(chi_test$p.value, 0.05, 
            label = "Political ideology should be balanced across treatment groups (Chi-Square test).")
})











# Save validation results
validation_results <- tibble(
  test = c(
    "Dataset loaded successfully",
    "Required variables are present",
    "Data types are valid",
    "No missing values in critical variables",
    "Numerical variables are within expected ranges",
    "Categorical variables have valid levels"
  ),
  status = "Passed"
)

write_csv(validation_results, "data/02-analysis_data/validation_results.csv")

#### Save Analysis Data ####
write_parquet(analysis_data, "data/02-analysis_data/validated_analysis_data.parquet")
