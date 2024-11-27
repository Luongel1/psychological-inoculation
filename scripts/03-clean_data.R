#### Preamble ####
# Purpose: Cleans the raw dataset to include only relevant variables
# Author: Elizabeth Luong
# Date: 3 December 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Datasets provided in .parquet format

#### Workspace setup ####
library(tidyverse)
library(readxl)
library(arrow)

# Read the "Combined" tab from the uploaded Excel file
raw_data <- read_excel(file_path, sheet = "Combined")

# Select and clean relevant variables
cleaned_data <- 
  raw_data |>
  filter(
    !is.na(Condition),              # Ensure no missing values in treatment condition
    !is.na(`Diff-Technique`),       # Ensure technique discernment is available
    !is.na(`Age-text`),             # Use Age-text for age representation
    !is.na(`Gender-text`),                 # Ensure demographic variables are not missing
    !is.na(Education),              
    !is.na(`Political-Ideology`),   
    !is.na(`Bullshit-Receptivity`), 
    !is.na(`Conspiracy-Belief`),    
    !is.na(`Analytical-Thinking`)   
  ) |>
  mutate(
    # Calculate technique discernment from `Diff-Technique`
    technique_discernment = `Diff-Technique`,
    
    # Map Condition to treatment
    treatment = as.character(if_else(Condition == "Inoculation", "Inoculation", "Control")),
    
    # Standardize Gender-text to valid categories
    gender = as.character(case_when(
      `Gender-text` %in% c("male", "Male") ~ "Male",
      `Gender-text` %in% c("emale", "Female") ~ "Female",
      `Gender-text` %in% c("non-binary", "Non-binary", "nonbinary", "Nonbinary") ~ "Non-binary",
      `Gender-text` %in% c("prefer not to say", "Prefer not to say") ~ "Prefer not to say",
      `Gender-text` %in% c("other", "Other") ~ "Other",
      TRUE ~ NA_character_  # Handle unexpected or missing values
    )),
    
    # Map Education levels to valid categories and replace invalid values with NA
    education = as.character(case_when(
      Education == "1" ~ "High School",
      Education == "2" ~ "Bachelor's",
      Education == "3" ~ "Master's",
      Education == "4" ~ "Doctorate",
      TRUE ~ NA_character_      # Handle missing or unexpected values
    )),
    
    # Convert Age-text to character
    age_text = as.character(`Age-text`),
    
    # Standardize psychological variables
    standardized_analytical_thinking = scale(`Analytical-Thinking`),
    standardized_conspiracy_belief = scale(`Conspiracy-Belief`),
    standardized_bullshit_receptivity = scale(`Bullshit-Receptivity`),
    
    # Map numeric levels of Political Ideology to meaningful categories
    political_ideology = as.factor(case_when(
      `Political-Ideology` == "1" ~ "Liberal",
      `Political-Ideology` == "2" ~ "Liberal",
      `Political-Ideology` == "3" ~ "Conservative",
      `Political-Ideology` == "4" ~ "Moderate",
      `Political-Ideology` == "5" ~ "Conservative",
      `Political-Ideology` == "6" ~ "Conservative",
      `Political-Ideology` == "7" ~ "Conservative",
      TRUE ~ NA_character_        # Handle missing or unexpected values
    ))
  ) |>
  select(
    # Retain only relevant variables
    treatment,
    technique_discernment,
    standardized_analytical_thinking,
    standardized_conspiracy_belief,
    standardized_bullshit_receptivity,
    gender,
    education,
    political_ideology,
    age_text  
  )


#### Save Cleaned Data ####
# Save the cleaned dataset in Parquet format
write_parquet(cleaned_data, "data/02-analysis_data/analysis_data.parquet")

#### Check Data ####
# Display summary of the cleaned dataset
summary(cleaned_data)
