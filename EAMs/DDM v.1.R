library(tidyverse)

v1 <- 3 # drift value 1
v2 <- 2 # drift value 2
s <- 1 # sigma
v1_bound <- 1 #alternative 1 decision boundary
v2_bound <- -1 #alternative 2 decision boundary

dt <- 0.001 #time increment
endTime <- 2 # total simulated time
time <- seq(0, endTime, by = dt) # vector of all time points, increments of dt
nSteps <- length(time) # number of time points

noise <- rnorm(nSteps - 1, mean = 0, sd = s) * sqrt(dt)

drift <- v1 - v2

acc <- numeric(nSteps)

for (i in 2:nSteps) {
  acc[i] <- acc[i - 1] + drift * dt + noise[i - 1]
  if (acc[i] >= v1_bound) {
    print("alternative 1 chosen")
    decision_time <- time[i]
    print(decision_time)
    break
  }
  if (acc[i] <= v2_bound) {
    print("alternative 2 chosen")
    decision_time <- time[i]
    print(decision_time)
    break
  }
}

ddm_data <- data.frame(
  time = rep(time, 2),
  accumulator = rep(("acc"), each = nSteps),
  evidence = acc
)

ggplot(ddm_data, aes(x = time, y = evidence)) +
  geom_line(linewidth = 0.8) +
  coord_cartesian(xlim = c(0, decision_time), ylim = c(-1, 1)) +
  geom_hline(yintercept = c(1, -1), linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "black", alpha = 0.5) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Time (s)",
    y = "Accumulated evidence",
    title = "Evidence accumulation over time"
  )

