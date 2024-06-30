library(survival)  
library(dplyr)

# Define parameters based on Lachin's example and additional context
lambda_e <- 0.20  # Hazard rate for the experimental group
lambda_c <- 0.30  # Hazard rate for the control group
N <- 378          # Initial sample size (will be recalculated)
n_e <- N / 2      # Size of experimental group, assuming equal split
n_c <- N / 2      # Size of control group, assuming equal split
T <- 5            # Total time of observation
R <- 3            # Accrual period duration

# Function to calculate phi(lambda) for the exponential distribution
calculate_phi <- function(lambda, T, R) {
  term <- (exp(-lambda * (T - R)) - exp(-lambda * T)) / (lambda * R)
  return(lambda^2 * (1 - term)^-1)
}

# Estimating variances for each group using phi(lambda)
phi_lambda_e <- calculate_phi(lambda_e, T, R)
phi_lambda_c <- calculate_phi(lambda_c, T, R)

# Expected number of deaths calculation
expected_deaths_H0 <- N * lambda_c^2 / phi_lambda_c
expected_deaths_H1_e <- n_e * lambda_e^2 / phi_lambda_e
expected_deaths_H1_c <- n_c * lambda_c^2 / phi_lambda_c
expected_deaths_H1 <- expected_deaths_H1_e + expected_deaths_H1_c

# Wald statistic for hypothesis testing
wald_statistic <- (lambda_e - lambda_c) / sqrt(phi_lambda_e / n_e + phi_lambda_c / n_c)
p_value <- 1 - pnorm(wald_statistic)  # One-sided test

# Critical z-values for alpha = 0.05 (one-sided) and beta = 0.10
Z_alpha <- qnorm(0.95)  # 95% confidence one-sided 
Z_beta <- qnorm(0.90)    # 90% power

# Combined Z value (simplification using Z_alpha + Z_beta)
Z_sum <- Z_alpha + Z_beta

# Calculate the required sample size N using the simplified model
combined_variance_term <- sqrt(phi_lambda_e / 0.5 + phi_lambda_c / 0.5)  # Q_e = Q_c = 0.5
required_difference <- abs(lambda_e - lambda_c)
N_required <- (Z_sum * combined_variance_term / required_difference)^2

# Print outputs
cat("Required sample size N using simplified model:", N_required, "\n")
cat("Wald Statistic:", wald_statistic, "\n")
cat("P-Value:", p_value, "\n")
cat("Expected number of deaths under H0:", expected_deaths_H0, "\n")
cat("Expected number of deaths under H1 (total):", expected_deaths_H1, "\n")
