library(stats)

# Define parameters
lambda_e <- 0.10  # Hazard rate for the exercise group
lambda_c <- 0.15  # Hazard rate for the sedentary group
N <- 100         # Total sample size

Q_e <- 0.5  # Proportion of total sample in the exercise group
Q_c <- 0.5  # Proportion of total sample in the control group

phi_lambda <- 0.012  # Variance function value for pooled estimate
phi_lambda_e <- 0.015  # Variance function value for exercise group
phi_lambda_c <- 0.015  # Variance function value for control group

# Standard normal deviates for alpha and beta
Z_alpha <- qnorm(0.975)  # For a two-sided test at alpha = 0.05
Z_beta <- qnorm(0.80)    # For power = 80%

# Calculation using the formula
left_side <- sqrt(N) * abs(lambda_e - lambda_c)

right_side <- Z_alpha * sqrt(phi_lambda * (1/Q_e + 1/Q_c)) +
              Z_beta * sqrt(phi_lambda_e/Q_e + phi_lambda_c/Q_c)

# Print results
print(paste("Left side of the equation (scaled difference):", left_side))
print(paste("Right side of the equation (threshold for significance and power):", right_side))

# Checking if the study is adequately powered
if (left_side > right_side) {
  message("The study is adequately powered to detect the difference.")
} else {
  message("The study may not be adequately powered to detect the difference. Consider increasing the sample size or revising other parameters.")
}

# Function to calculate variance given lambda and sample size
calculate_variance <- function(lambda, N, T, R) {
  term <- (exp(-lambda * (T - R)) - exp(-lambda * T)) / (lambda * R)
  return(lambda^2 * (1 - term)^-1 / N)
}

# Estimated variances
sigma_squared_lambda_e <- calculate_variance(lambda_e, n_e, T, R)
sigma_squared_lambda_c <- calculate_variance(lambda_c, n_c, T, R)

# Wald statistic
wald_statistic <- (lambda_e - lambda_c) / sqrt(sigma_squared_lambda_e + sigma_squared_lambda_c)

