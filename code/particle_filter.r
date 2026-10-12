# R/particle_filter.R
library(readxl)
library(tidyverse)
library(zoo)

run_particle_filter <- function(y, mu, phi, sigma_eta, beta, N_particles = 1000) {
  T <- length(y)
  
  # Set up empty matrices
  h_particles <- matrix(0, nrow = N_particles, ncol = T)
  weights <- matrix(0, nrow = N_particles, ncol = T)
  h_filtered_mean <- numeric(T)
  n_eff_history <- numeric(T)
  
  # Initial Conditions
  uncond_variance <- sigma_eta^2 / (1 - phi^2)
  h_particles[, 1] <- rnorm(N_particles, mean = mu, sd = sqrt(uncond_variance))
  weights[, 1] <- 1 / N_particles
  h_filtered_mean[1] <- sum(h_particles[, 1] * weights[, 1])
  n_eff_history[1] <- N_particles
  
  # Algorithm Loop
  for (t in 2:T) {
    # Predict
    h_prev <- h_particles[, t-1]
    h_prop <- rnorm(N_particles, mean = mu + phi * (h_prev - mu), sd = sigma_eta)
    
    # Update (Observation)
    obs_sd <- beta * exp(h_prop / 2)
    likelihood <- dnorm(y[t], mean = 0, sd = obs_sd)
    
    # Calculate weights
    w_unnorm <- weights[, t-1] * likelihood
    w_norm <- w_unnorm / sum(w_unnorm)
    
    # Check Degeneracy
    n_eff <- 1 / sum(w_norm^2)
    n_eff_history[t] <- n_eff
    
    # Resampling
    if (n_eff < N_particles / 2) {
      resample_indices <- sample(1:N_particles, size = N_particles, replace = TRUE, prob = w_norm)
      h_particles[, t] <- h_prop[resample_indices]
      weights[, t] <- 1 / N_particles
    } else {
      h_particles[, t] <- h_prop
      weights[, t] <- w_norm
    }
    
    h_filtered_mean[t] <- sum(h_particles[, t] * weights[, t])
  }
  
  # Return results as a list
  return(list(
    filtered_volatility = h_filtered_mean,
    n_eff_history = n_eff_history
  ))
}