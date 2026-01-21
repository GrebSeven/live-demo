v1 <- 3 # drift value 1
v2 <- 2 # drift value 2
s <- 1 # sigma
d_bound <- 1 #decision boundary
winner <- NA #placeholders are best practice
decision_time <- NA

dt <- 0.001 #time increment
endTime <- 0.5 # total simulated time
time <- seq(0, endTime, by = dt) # vector of all timepoints, increments of dt
nSteps <- length(time) # number of timepoints

# Pre-generate noise
## generate 500 random noise points for acc_1. nSteps-1 because we start at 0
## sqrt(dt) because noise variance scales with time step size.
noise_1 <- rnorm(nSteps - 1, mean = 0, sd = s) * sqrt(dt)
##Same for acc_2
noise_2 <- rnorm(nSteps - 1, mean = 0, sd = s) * sqrt(dt)

# Pre-allocate accumulators
acc_1 <- numeric(nSteps) #acc_1 is vector with nSteps == 0
acc_2 <- numeric(nSteps)

inhibition=c("none","ff","lat") [3] #could we perhaps map the types on inhibition?


for (i in 2:nSteps) { #Start at 2 because i[1] == 0
  #acc_1[i] is the previous value + drift term + noise term.
  if (inhibition=="none") {
    v1_mod=0
    v2_mod=0
    noise_1_mod=0
    noise_2_mod=0
  } else if (inhibition=="ff") {
    v1_mod=v2
    v2_mod=v1
    noise_1_mod=noise_2[i - 1]
    noise_2_mod=noise_1[i - 1]
  } else if (inhibition=="lat") {
    v1_mod=acc_2[i-1]
    v2_mod=acc_1[i-1]
    noise_1_mod=0
    noise_2_mod=0
  }
  acc_1[i] <- acc_1[i - 1] + (v1 - v1_mod) * dt + (noise_1[i - 1] - noise_1_mod)
  acc_2[i] <- acc_2[i - 1] + (v2 - v2_mod) * dt + (noise_2[i - 1] - noise_2_mod)
  if (acc_1[i] >= d_bound) {
    print("acc_1")
    decision_time <- time[i]
    break
  }
  if (acc_2[i] >= d_bound) {
    print("acc_2")
    decision_time <- time[i]
    break
  }
}

# Combine data for plotting
acc_data <- data.frame(
  time = rep(time, 2),
  accumulator = rep(c("acc_1", "acc_2"), each = nSteps),
  evidence = c(acc_1, acc_2)
)

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

