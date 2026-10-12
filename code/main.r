# main.R

# 1. Load Libraries
library(readxl)
library(tidyverse)
library(zoo)

# 2. Source Modules
source("code/data_processing.R")
source("code/particle_filter.R")
source("code/plotting.R")

# 3. Model Configuration
set.seed(52948616)
N_particles <- 1000

# Tuned parameters for NZD/USD
params <- list(
  mu = -1.0,
  phi = 0.93,
  sigma_eta = 0.2,
  beta = 1.0
)

# 4. Execute Pipeline
exchange_usd_data <- load_and_prep_exchange_data("data/hb1-daily.xlsx")

results <- run_particle_filter(
  y = exchange_usd_data$y_t, 
  mu = params$mu, 
  phi = params$phi, 
  sigma_eta = params$sigma_eta, 
  beta = params$beta, 
  N_particles = N_particles
)

# 5. Append Results & Calculate Diagnostics
exchange_usd_data <- exchange_usd_data %>%
  mutate(
    filtered_volatility = results$filtered_volatility,
    n_eff = results$n_eff_history,
    rolling_var_30 = rollapply(y_t, width = 30, FUN = var, fill = NA, align = "right"),
    pf_implied_var = exp(filtered_volatility),
    u_t = y_t / exp(filtered_volatility / 2),
    u_t_squared = u_t^2
  )

# 6. Final Output
cat("\nRunning Ljung-Box Test for Residual Independence:\n")
print(Box.test(exchange_usd_data$u_t_squared, type = "Ljung-Box", lag = 10))

cat("\nGenerating Diagnostic Plots...\n")
generate_diagnostic_plots(exchange_usd_data, N_particles)