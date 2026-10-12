# R/plotting.R
library(readxl)
library(tidyverse)
library(zoo)

generate_diagnostic_plots <- function(df, N_particles) {
  
  # Figure 1: Returns
  p_returns <- ggplot(df, aes(x = Date, y = y_t)) +
    geom_line(color = "steelblue", alpha = 0.8, linewidth = 0.4) +
    labs(title = "Observed Daily Log-Returns (NZD/USD)", x = "Year", y = "Log-Return (%)") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))
  
  # Figure 2: Volatility
  p_volatility <- ggplot(df, aes(x = Date, y = filtered_volatility)) +
    geom_line(color = "darkred", linewidth = 0.6) +
    labs(title = "Particle Filter Estimate of Latent Log-Volatility", 
         x = "Year", y = expression("Estimated Log-Volatility (" * h[t] * ")")) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))
  
  # Figure 3: Effective Sample Size
  p_neff <- ggplot(df, aes(x = Date, y = n_eff)) +
    geom_line(color = "darkgreen", linewidth = 0.4) +
    geom_hline(yintercept = N_particles / 2, color = "red", linetype = "dashed") +
    labs(title = "Particle Degeneracy: Effective Sample Size (N_eff)", 
         x = "Year", y = expression("N"[eff])) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))
  
  # Figure 4: Variance Comparison
  p_variance <- ggplot(df, aes(x = Date)) + # <-- Added na.rm = TRUE
    geom_line(aes(y = pf_implied_var, color = "Particle Filter Variance"), 
              linewidth = 0.8) +
    geom_line(aes(y = rolling_var_30, color = "30-Day Rolling Variance"), 
              linewidth = 0.8, alpha = 0.7, na.rm = TRUE) +
    labs(title = "Variance Tracking: Particle Filter vs. 30-Day Rolling Window",
         x = "Year", y = expression("Estimated Variance (" * sigma[t]^2 * ")"), color = "Model") +
    scale_color_manual(values = c("30-Day Rolling Variance" = "grey50", "Particle Filter Variance" = "darkred")) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  
  # Figure 5: ACF of Squared Residuals
  clean_residuals <- na.omit(df$u_t_squared)
  n_obs <- length(clean_residuals)
  acf_math <- acf(clean_residuals, lag.max = 20, plot = FALSE)
  
  acf_df <- data.frame(
    lag = as.vector(acf_math$lag)[-1],
    acf = as.vector(acf_math$acf)[-1]
  )
  
  ci_bound <- 1.96 / sqrt(n_obs)
  
  p_acf <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_segment(aes(xend = lag, yend = 0), color = "black", linewidth = 1.2) +
    geom_point(color = "black", size = 2.5) + 
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_hline(yintercept = c(ci_bound, -ci_bound), color = "blue", linetype = "dashed", linewidth = 0.8) +
    labs(title = "ACF of Squared Standardized Residuals",
         subtitle = expression("Checking for remaining volatility clustering (" * u[t]^2 * ")"),
         x = "Lag", y = "Autocorrelation") +
    scale_x_continuous(breaks = seq(1, 20, by = 1)) +
    coord_cartesian(ylim = c(-0.10, 0.10)) + 
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  
  ggsave("figures/1_log_returns.pdf", plot = p_returns, width = 8, height = 4.5)
  ggsave("figures/2_filtered_volatility.pdf", plot = p_volatility, width = 8, height = 4.5)
  ggsave("figures/3_effective_sample_size.pdf", plot = p_neff, width = 8, height = 4.5)
  ggsave("figures/4_variance_comparison.pdf", plot = p_variance, width = 8, height = 4.5)
  ggsave("figures/5_acf_residuals.pdf", plot = p_acf, width = 8, height = 4.5)
}