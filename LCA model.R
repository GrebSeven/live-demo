# Load in Packages ----
library(tidyverse)

# Set Parameters ----

v1 <- 3 # drift value 1
v2 <- 2 # drift value 2
s <- 1 # sigma
d_bound <- 1 # decision boundary
dt <- 0.001 # time increment
endTime <- 0.5 # total simulated time
beta <- 0.5 # Parameter that limits the amount of Lateral Inhibition
lambda <- 0.5 # Limits the amount of leakage
acc_lim <- 0 #x limit truncation at 0

# Create Placeholder Variables ----

winner <- NA # placeholders are best practice
decision_time <- NA

# Create Vectors ----

## Time Vector
time <- seq(0, endTime, by = dt) # vector of all timepoints, increments of dt

## Steps Vector
nSteps <- length(time) # number of timepoints

## Accumulator Vectors

acc_1 <- numeric(nSteps) # acc_1 is vector with nSteps == 0

acc_2 <- numeric(nSteps)
# Generate Noise ----

noise_1 <- rnorm(nSteps - 1, mean = 0, sd = s) * sqrt(dt)
noise_2 <- rnorm(nSteps - 1, mean = 0, sd = s) * sqrt(dt)

# Model Generation ----

for (i in 2:nSteps) {
  # Current    #Previous     #Drift  #Leakage            #Lateral Inhibition
  acc_1[i] <- acc_1[i - 1] + (v1 - lambda * acc_1[i - 1] - beta * acc_2[i - 1]) * dt + (noise_1[i - 1])
  acc_2[i] <- acc_2[i - 1] + (v2 - lambda * acc_2[i - 1] - beta * acc_1[i - 1]) * dt + (noise_2[i - 1])
  
  
  ## max() function compares two values and returns the larger one.
  ##returns acc_1[i] as either positive value or zero
  acc_1[i] <- max(acc_1[i], acc_lim)
  acc_2[i] <- max(acc_2[i], acc_lim)
  
  if (acc_1[i] >= d_bound) {
    decision_time <- time[i]
    winner <- "Alternative 1 Chosen"
    break
  } else if (acc_2[i] >= d_bound) {
    decision_time <- time[i]
    winner <- "Alternative 2 Chosen"
    break
  }
}

# Create Data frame ----
acc_data <- data.frame(
  time = rep(time, 2),
  accumulator = rep(c("acc_1", "acc_2"), each = nSteps),
  evidence = c(acc_1, acc_2)
)

# Plot Data ----
ggplot(acc_data, aes(x = time, y = evidence, colour = accumulator)) +
  geom_line(linewidth = 0.8) +
  coord_cartesian(xlim = c(0, decision_time)) + # preserves data
  theme_minimal(base_size = 14) +
  labs(
    x = "Time (s)",
    y = "Accumulated evidence",
    title = "Evidence accumulation over time"
  ) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA)
  )
# Print Results----
print(decision_time)
print(winner)
