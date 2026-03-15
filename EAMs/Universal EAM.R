library(tidyverse)

# Drift Parameters -----------------------------------------------------------
v1 <- 3 # alternative 1 drift rate
v2 <- 2 # alternative 2 drift rate
s <- 1  # standard deviation/sigma

# Time Parameters ------------------------------------------------------------
dt <- 0.001     # time increments
endTime <- 0.2  # total time

# Inhibition & Leakage Parameters --------------------------------------------
beta <- 0.5
lambda <- 0.5
acc_lim <- 0

# Create Time and Accumulator Vectors ---------------------------------------
time <- seq(0, endTime, by = dt)
nSteps <- length(time)
acc_1 <- numeric(nSteps)
acc_2 <- numeric(nSteps)

# Boundary Parameters and Vectors ------------------------------------------
d_bound <- 1
bound_min <- 0
collapse_rate <- 4
boundary <- numeric(nSteps)
boundary[1] <- d_bound


# Generate Noise --------------------------------------------------------------
noise_1 <- rnorm(nSteps - 1, mean = 0, sd = s) * sqrt(dt)
noise_2 <- rnorm(nSteps - 1, mean = 0, sd = s) * sqrt(dt)

# Choose Collapse and Inhibition Types ----------------------------------------
collapse <- c("none", "linear", "hyperbolic")[1]
inhibition <- c("none", "ff", "lat")[3]

# Generate Boundary-------------------------------------------------------------
for (i in 2:nSteps){
  if (collapse == "none") {
    boundary[i] <- d_bound
  } else if (collapse == "linear") {
    boundary[i] <- d_bound - (d_bound - bound_min) * (time[i] / endTime)
  } else if (collapse == "hyperbolic") {
    boundary[i] <- d_bound / (1 + collapse_rate * time[i])
  }
}

# Model Generation ------------------------------------------------------------
for (i in 2:nSteps) {
  if (inhibition == "none") {
    v1_mod <- 0
    v2_mod <- 0
    noise_1_mod <- 0
    noise_2_mod <- 0
  } else if (inhibition == "ff") {
    v1_mod <- v2
    v2_mod <- v1
    noise_1_mod <- beta*noise_2[i - 1]
    noise_2_mod <- beta*noise_1[i - 1]
  } else if (inhibition == "lat") {
    v1_mod <- acc_2[i - 1]
    v2_mod <- acc_1[i - 1]
    noise_1_mod <- 0
    noise_2_mod <- 0
  }
  
  acc_1[i] <- acc_1[i - 1] + (v1 - lambda * acc_1[i - 1] - beta * v1_mod) * dt + (noise_1[i - 1] - noise_1_mod)
  acc_2[i] <- acc_2[i - 1] + (v2 - lambda * acc_2[i - 1] - beta * v2_mod) * dt + (noise_2[i - 1] - noise_2_mod)
  
  acc_1[i] <- max(acc_1[i], acc_lim)
  acc_2[i] <- max(acc_2[i], acc_lim)
  
  if (acc_1[i] >= boundary[i]) {
    decision_time <- time[i]
    winner <- "Alternative 1 Chosen"
    break
  } else if (acc_2[i] >= boundary[i]) {
    decision_time <- time[i]
    winner <- "Alternative 2 Chosen"
    break
  }
}

# Create Data Frames -----------------------------------------------------------
acc_data <- data.frame(
  time = rep(time, 2),
  accumulator = rep(c("acc_1", "acc_2"), each = nSteps),
  evidence = c(acc_1, acc_2)
)

boundary_data <- data.frame(
  time = time,
  bound = boundary
)

# Plot Data --------------------------------------------------------------------
ggplot(acc_data, aes(x = time, y = evidence, colour = accumulator)) +
  geom_line(linewidth = 0.8) +
  geom_line(data = boundary_data, aes(x = time, y = bound), 
            colour = "black", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
  coord_cartesian(xlim = c(0, ifelse(is.na(decision_time), endTime, decision_time))) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Time (s)",
    y = "Accumulated evidence",
    title = "Evidence accumulation over time"
  ) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA)
  )

# Print Results ---------------------------------------------------------------
print(decision_time)
print(winner)