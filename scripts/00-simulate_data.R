#### Preamble ####
# Purpose: Simulates data for psychological inoculation study
# Author: Elizabeth Luong
# Date: 3 December 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: --

#### Workspace setup ####
library(tidyverse)

#### Simulate data ####
# Set seed for reproducibility
set.seed(853)

# Define the number of participants
num_participants <- 1000

# Simulate data
simulate_data <- 
  tibble(
    # Group assignment (Treatment = 1, Control = 0)
    Group = sample(0:1, size = num_participants, replace = TRUE),
    
    # Simulate Analytical Thinking
    Analytical_Thinking = pmin(pmax(rnorm(num_participants, mean = 5, sd = 1.5), 0), 10),
    
    # Simulate outcome scores for neutral and manipulative posts
    Neutral_Score = rnorm(num_participants, mean = 5, sd = 2),
    Manipulative_Score = rnorm(num_participants, mean = if_else(Group == 1, 3 - 0.3 * Analytical_Thinking, 4 - 0.2 * Analytical_Thinking), sd = 2),
    
    # Simulate demographic covariates
    Age = sample(18:70, size = num_participants, replace = TRUE),
    Gender = sample(c("Male", "Female", "Other"), size = num_participants, replace = TRUE, prob = c(0.48, 0.48, 0.04)),
    Education = sample(c("High School", "Bachelor's", "Master's", "Doctorate"), size = num_participants, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
    Political_Affiliation = sample(c("Democrat", "Republican", "Independent", "Other"), size = num_participants, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
    Minority_Status = sample(c("Yes", "No"), size = num_participants, replace = TRUE, prob = c(0.3, 0.7)),
    Bullshit_Receptivity = pmin(pmax(rnorm(num_participants, mean = 4, sd = 2), 0), 10),
    Conspiracy_Belief = pmin(pmax(rnorm(num_participants, mean = 4, sd = 2), 0), 10)
  ) |> 
  mutate(
    # Add a categorical representation for the group
    Group = if_else(Group == 1, "Treatment", "Control"),
    
    # Explicitly introduce a linear relationship with strengthened group interaction
    Discernment_Score = Neutral_Score - Manipulative_Score +
      0.5 * Analytical_Thinking * ifelse(Group == "Treatment", 1.5, 1)
  )

#### Test Simulated Data ####
# Summary of key variables
summary(simulate_data)

# Test distribution of Group
table(simulate_data$Group)

# Test demographic variable distributions
table(simulate_data$Gender)
table(simulate_data$Education)
table(simulate_data$Political_Affiliation)
table(simulate_data$Minority_Status)

# Test outcome variables
summary(simulate_data$Neutral_Score)
summary(simulate_data$Manipulative_Score)
summary(simulate_data$Discernment_Score)

#### Save data ####
# Save the simulated data to a CSV file
write_csv(simulate_data, "data/00-simulated_data/simulated_data.csv")
