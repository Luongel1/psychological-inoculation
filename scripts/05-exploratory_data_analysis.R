#### Preamble ####
# Purpose: Exploratory data analysis on cleaned data.
# Author: Elizabeth Luong
# Date: 3 December 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
#   - The `tidyverse`, `ggplot2`, and `lubridate` packages must be installed and loaded.
#   - The cleaned dataset `analysis_data.parquet` should exist in the `data/02-analysis_data/` directory.

#### Workspace setup ####
library(tidyverse)
library(ggplot2)
library(lubridate)
library(arrow)

#### Read data ####
analysis_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")

# Overview of the dataset
glimpse(analysis_data)
summary(analysis_data)

#### Summary Statistics ####
# Numeric column summaries
analysis_data %>% 
  select_if(is.numeric) %>% 
  summary()

# Count unique values for categorical columns
analysis_data %>% 
  select_if(is.character) %>% 
  summarise(across(everything(), n_distinct))

# Missing values per column
analysis_data %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_Count")

#### Univariate Analysis ####
# Distribution of technique discernment
ggplot(analysis_data, aes(x = technique_discernment)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Technique Discernment", x = "Technique Discernment", y = "Count")

# Distribution of standardized analytical thinking
ggplot(analysis_data, aes(x = standardized_analytical_thinking)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Analytical Thinking", x = "Analytical Thinking (Standardized)", y = "Count")

# Gender distribution
analysis_data %>% 
  count(gender) %>% 
  ggplot(aes(x = reorder(gender, n), y = n, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Bivariate Analysis ####
# Technique discernment by treatment group
ggplot(analysis_data, aes(x = treatment, y = technique_discernment, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Technique Discernment by Treatment Group", x = "Treatment Group", y = "Technique Discernment")

# Analytical thinking vs. technique discernment
ggplot(analysis_data, aes(x = standardized_analytical_thinking, y = technique_discernment)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Analytical Thinking vs. Technique Discernment", x = "Analytical Thinking (Standardized)", y = "Technique Discernment")

# Treatment vs. political ideology
analysis_data %>% 
  count(treatment, political_ideology) %>% 
  ggplot(aes(x = political_ideology, y = n, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Political Ideology by Treatment Group", x = "Political Ideology", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Multivariate Analysis ####
# Interaction of treatment and gender on technique discernment
ggplot(analysis_data, aes(x = gender, y = technique_discernment, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Interaction of Gender and Treatment on Technique Discernment", x = "Gender", y = "Technique Discernment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatterplot: Analytical thinking vs. conspiracy belief, colored by treatment
ggplot(analysis_data, aes(x = standardized_analytical_thinking, y = standardized_conspiracy_belief, color = treatment)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Analytical Thinking vs. Conspiracy Belief by Treatment", 
       x = "Analytical Thinking (Standardized)", y = "Conspiracy Belief (Standardized)")

# Identify variables with high missingness for imputation or exclusion
missing_summary <- analysis_data %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_Count")
print(missing_summary)
