#### Preamble ####
# Purpose: Downloads and saves the data from OSF
# Author: Elizabeth Luong
# Date: 3 December 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Install and load `readxl` and `tidyverse` packages

#### Workspace setup ####
library(tidyverse)
library(readxl)  # For reading Excel files
library(httr)    # For downloading files

#### Download data ####
# Define the URL and file path
data_url <- "https://osf.io/d2eqv/download"
file_path <- "data/raw_data/datasets.xlsx"

# Create the directory if it doesn't exist
if (!dir.exists("data/raw_data")) {
  dir.create("data/raw_data", recursive = TRUE)
}

# Download the file
GET(data_url, write_disk(file_path, overwrite = TRUE))

#### Load the "Combined" tab from the Excel file ####
raw_data <- read_excel(file_path, sheet = "Combined")

#### Save data in Parquet Format ####
write_parquet(raw_data, "data/raw_data/datasets_combined.parquet")

#### Check the data ####
# Display a summary of the data to confirm successful download and processing
summary(raw_data)
