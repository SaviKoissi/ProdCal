# Load required libraries
if (!requireNamespace("rstan", quietly = TRUE)) install.packages("rstan")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(rstan)
library(ggplot2)
library(dplyr)

# Set initial parameters for the production process
x <- 100  # initial vegetal material
n_cycles <- 10  # number of cycles to simulate
months_per_cycle <- 2  # length of each cycle in months
gamma <- 6  # initial production period in months
alpha <- 2  # acclimation period in months

# Define prior distributions for uncertain parameters
regeneration_rate_prior <- function() {
  # Uniform distribution for regeneration rate (50% to 80%)
  runif(1, 0.5, 0.8)
}

survival_explant_prior <- function() {
  # Uniform distribution for explant survival rate (70% to 100%)
  runif(1, 0.7, 1)
}

survival_acclimatization_prior <- function() {
  # Uniform distribution for acclimatization survival rate (90% to 100%)
  runif(1, 0.9, 1)
}

technician_productivity_prior <- function() {
  # Uniform distribution for technician productivity (bottles per day)
  runif(1, 30, 100)
}

# Likelihood function for updating priors (dummy data for illustration)
update_likelihood <- function(survival_data, productivity_data) {
  # Assume survival data and productivity data are given as observations
  survival_updated <- rbeta(1, 2 + sum(survival_data), 2 + length(survival_data) - sum(survival_data))
  productivity_updated <- runif(1, min(productivity_data), max(productivity_data))
  return(list(survival_updated = survival_updated, productivity_updated = productivity_updated))
}

# Simulate the production process with Bayesian updates
monte_carlo_simulation <- function(x, n_cycles, months_per_cycle, gamma, alpha) {
  results <- list()
  
  for (cycle in 1:n_cycles) {
    # Bayesian updating for survival and productivity at each cycle
    regeneration_rate <- regeneration_rate_prior()
    survival_explant_rate <- survival_explant_prior()
    survival_acclimatization_rate <- survival_acclimatization_prior()
    productivity <- technician_productivity_prior()
    
    # Explant production after each cycle (with varying survival rates)
    explants_after_n <- x * regeneration_rate * (4 * survival_explant_rate)^(cycle)
    
    # Bottles required at each cycle
    bottles_required <- explants_after_n / 10
    
    # Number of technicians needed based on productivity
    tech_workload <- productivity * 5 * 20  # bottles per month
    technicians_needed <- ceiling(bottles_required / tech_workload)
    
    # Store results for each cycle
    results[[cycle]] <- data.frame(
      cycle = cycle,
      explants_after_n = explants_after_n,
      bottles_required = bottles_required,
      technicians_needed = technicians_needed,
      regeneration_rate = regeneration_rate,
      survival_explant_rate = survival_explant_rate,
      survival_acclimatization_rate = survival_acclimatization_rate
    )
  }
  
  # Combine results into a single data frame
  results_df <- do.call(rbind, results)
  return(results_df)
}

# Run the simulation for the production process
results_df <- monte_carlo_simulation(x, n_cycles, months_per_cycle, gamma, alpha)

# Print and plot the results
print(results_df)

# Plot the number of bottles required and technicians needed over time
ggplot(results_df, aes(x = cycle)) +
  geom_line(aes(y = bottles_required), color = "blue", size = 1.5) +
  geom_line(aes(y = technicians_needed), color = "red", size = 1.5) +
  labs(title = "Bottles Required and Technicians Needed Over Time",
       x = "Cycle",
       y = "Count") +
  theme_minimal()
