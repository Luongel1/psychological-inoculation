#### Preamble ####
# Purpose: Model the relationship between psychological inoculation treatment and technique discernment.
# Author: Elizabeth Luong
# Date: 3 December 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
#   - The `tidyverse`, `rstanarm`, `arrow` packages must be installed and loaded
#   - 03-clean_data.R must have been run

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(arrow)
library(car)

#### Read data ####
analysis_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")

#### Model Data ####

# Fit a Bayesian regression model
model <- stan_glm(
  formula = technique_discernment ~ treatment + standardized_analytical_thinking + 
    standardized_conspiracy_belief + standardized_bullshit_receptivity + 
    gender + education + political_ideology + age_text,
  data = analysis_data,
  family = gaussian(),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_aux = exponential(rate = 1, autoscale = TRUE),
  seed = 853
)

#### Model Summary ####
# Summarize model results
summary(model)

# Save summary as a text file
capture.output(summary(model), file = "models/model_summary.txt")

#### Diagnostics ####

# Posterior predictive checks
pp_check(model)

# Save diagnostics plot
ggsave("models/pp_check_plot.png")

# Assess multicollinearity
vif_values <- car::vif(model)
print(vif_values)

# Save VIF values as a CSV
write_csv(as.data.frame(vif_values), "models/vif_values.csv")

#### Save Model ####
# Save the fitted model as an RDS file
saveRDS(model, file = "models/psych_inoculation_model.rds")

#### Extract Model Coefficients ####
#### Extract Model Coefficients ####
# Use as.matrix to extract the posterior draws for all model parameters
posterior_draws <- as.matrix(model)

# Summarize the posterior distribution for each coefficient
c# Summarize the posterior distribution for each coefficient
coefficients_summary <- data.frame(
  parameter = colnames(posterior_draws),  # Extract parameter names
  mean = apply(posterior_draws, 2, mean),  # Compute the mean
  sd = apply(posterior_draws, 2, sd),      # Compute the standard deviation
  `2.5%` = apply(posterior_draws, 2, quantile, probs = 0.025),  # Lower 95% credible interval
  `97.5%` = apply(posterior_draws, 2, quantile, probs = 0.975)  # Upper 95% credible interval
)

# Print the summary of coefficients
print(coefficients_summary)

# Save the coefficients summary for further analysis
write_csv(coefficients_summary, "models/model_coefficients_summary.csv")




