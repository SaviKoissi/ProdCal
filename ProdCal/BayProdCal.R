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
n_simulations <- 1000  # number of Monte Carlo simulations

# Define prior distributions for uncertain parameters
regeneration_rate_prior <- function() {
  runif(1, 0.5, 0.8)  # Uniform distribution for regeneration rate
}

survival_explant_prior <- function() {
  runif(1, 0.7, 1)  # Uniform distribution for explant survival rate
}

survival_acclimatization_prior <- function() {
  runif(1, 0.9, 1)  # Uniform distribution for acclimatization survival rate
}

technician_productivity_prior <- function() {
  runif(1, 30, 100)  # Uniform distribution for technician productivity
}

# Simulate the production process with Bayesian updates
monte_carlo_simulation <- function(x, n_cycles, n_simulations) {
  results <- list()
  
  for (sim in 1:n_simulations) {
    sim_results <- data.frame(cycle = integer(n_cycles),
                              explants_after_n = numeric(n_cycles),
                              bottles_required = numeric(n_cycles),
                              technicians_needed = numeric(n_cycles),
                              regeneration_rate = numeric(n_cycles),
                              survival_explant_rate = numeric(n_cycles),
                              survival_acclimatization_rate = numeric(n_cycles))
    
    for (cycle in 1:n_cycles) {
      # Bayesian updating for survival and productivity at each cycle
      regeneration_rate <- regeneration_rate_prior()
      survival_explant_rate <- survival_explant_prior()
      survival_acclimatization_rate <- survival_acclimatization_prior()
      productivity <- technician_productivity_prior()
      
      # Explant production after each cycle
      explants_after_n <- x * regeneration_rate * (4 * survival_explant_rate)^(cycle)
      
      # Bottles required at each cycle
      bottles_required <- explants_after_n / 10
      
      # Number of technicians needed based on productivity
      tech_workload <- productivity * 5 * 20  # bottles per month
      technicians_needed <- ceiling(bottles_required / tech_workload)
      
      # Store results for each cycle
      sim_results[cycle, ] <- c(cycle, explants_after_n, bottles_required, technicians_needed,
                                regeneration_rate, survival_explant_rate, survival_acclimatization_rate)
    }
    
    results[[sim]] <- sim_results
  }
  
  # Combine results into a single data frame for all simulations
  results_df <- do.call(rbind, results)
  return(results_df)
}

# Run the simulation for the production process
results_df <- monte_carlo_simulation(x, n_cycles, n_simulations)

# Summarize results
summary_results <- results_df %>%
  group_by(cycle) %>%
  summarise(
    mean_explants = mean(explants_after_n),
    sd_explants = sd(explants_after_n),
    mean_bottles = mean(bottles_required),
    sd_bottles = sd(bottles_required),
    mean_technicians = mean(technicians_needed),
    sd_technicians = sd(technicians_needed)
  )

# Print summary results
print(summary_results)

# Plot the mean and standard deviation of bottles required and technicians needed over time
ggplot(summary_results, aes(x = cycle)) +
  geom_line(aes(y = mean_bottles), color = "blue", size = 1.5) +
  geom_ribbon(aes(ymin = mean_bottles - sd_bottles, ymax = mean_bottles + sd_bottles), alpha = 0.2, fill = "blue") +
  geom_line(aes(y = mean_technicians), color = "red", size = 1.5) +
  geom_ribbon(aes(ymin = mean_technicians - sd_technicians, ymax = mean_technicians + sd_technicians), alpha = 0.2, fill = "red") +
  labs(title = "Mean Bottles Required and Technicians Needed Over Time with Uncertainty",
       x = "Cycle",
       y = "Count") +
  theme_minimal()
