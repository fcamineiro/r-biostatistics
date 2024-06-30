library(ggplot2)
library(dplyr)

lambda_rate <- 1/70  # Set the events rate parameter lambda

# Generate Poisson process: inter-event times
set.seed(42)
num_events <- 10
inter_event_times <- rexp(num_events, rate = lambda_rate)
event_times <- cumsum(inter_event_times)
total_time <- max(event_times)

# Prepare data frame for Poisson process plot
events_df <- data.frame(Event = 1:num_events, Time = event_times)

# Plotting the events in the Poisson process
p1 <- ggplot(events_df, aes(x = Time, y = 1, label = Event)) +
  geom_point() +
  geom_text(vjust = -1) +
  labs(title = "Poisson Process (Events Over Time)",
       x = "Time",
       y = "Events") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

# Generate data for the Exponential Distribution plot
time_values <- seq(0, total_time, length.out = 400)
pdf_values <- dexp(time_values, rate = lambda_rate)

# Prepare data frame for exponential distribution plot
exp_df <- data.frame(Time = time_values, PDF = pdf_values)

# Plotting the exponential distribution of waiting times
p2 <- ggplot(exp_df, aes(x = Time, y = PDF)) +
  geom_line(color = "red") +
  geom_area(fill = "red", alpha = 0.3) +
  labs(title = "Exponential Distribution of Waiting Times",
       x = "Time",
       y = "Probability Density") +
  theme_minimal()

# Display the plots
print(p1)
print(p2)
