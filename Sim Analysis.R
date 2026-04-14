library(tidyverse)
library(ggpubr)

# Wrangle Data ----------------------------------------------------------------
# Create a list of all the files with the specified path.
sim_files <- list.files(
  path = "Simulations/Datasets_CollapsingTruncatedLCA_FreeResp", # path to Sim folder
  full.names = TRUE # Keep full path names
)

sim_data_list <- vector("list")

for (file in sim_files) {
  loaded_names <- load(file) # Load RData file
  data <- as.data.frame(get(loaded_names[1])) # retrieve first object as a data frame
  gen_param_df <- as.data.frame(t(genParams)) # Append genparams as new columns
  data <- cbind(data, gen_param_df[rep(1, nrow(data)), , drop = FALSE]) # bind param data, repeating through all data points.
  colnames(data) <- c("Time", "Resp", "OV", "DIFF", names(genParams)) # Assign column names
  sim_data_list[[file]] <- data # Store in list using file name as key
}

sim_data <- bind_rows(sim_data_list)


# Data Visualisation -----------------------------------------------------------
# Stimuli Data -----------------------------------------------------------------
stimuli_data <- sim_data |>
  mutate(
    Resp = case_when( # Changing them for Clarity
      Resp == 1 ~ "correct",
      Resp == 2 ~ "incorrect"
    ),
    across(c(Resp, DIFF), as.factor) # Helps with visualisations.
  ) |>
  group_by(OV, DIFF, Resp) |> # Will result in averages over all data sharing these variables
  summarise(
    count = n(), # needed to calculate averages
    mean_rt = mean(Time)
  ) |>
  pivot_wider(
    names_from = Resp,
    values_from = c(count, mean_rt) # Will create columns for both these variables
  ) |>
  mutate(
    total_count = count_correct + count_incorrect,
    accuracy = as.numeric(count_correct / (count_correct + count_incorrect)),
    mean_rt = (mean_rt_correct * (count_correct / total_count)) +
      (mean_rt_incorrect * (count_incorrect / total_count)) # Weighted mean
  )
## Accuracy Plot ------------------------------------------------------------------
PLOT_acc <-
  ggplot(data = stimuli_data, aes(x = OV, y = accuracy, colour = DIFF)) +
  geom_smooth(method = "loess") +
  geom_point() +
  scale_y_continuous(
    breaks = seq(.5, 1, by = .05),
    labels = scales::percent
  ) +
  scale_x_continuous(breaks = seq(3, 9, by = 1)) +
  theme_minimal() +
  guides(colour = (guide_legend(reverse = TRUE))) +
  labs(
    title = "Simulation Decision Accuracy by Magnitude",
    subtitle = "Coloured by Alternative Value Difference",
    x = "Magnitude",
    y = "Accuracy",
    colour = "Value Difference"
  )

PLOT_acc
## Response Time Plots ---------------------------------------------------------------
### Overall Response Time Plot----
PLOT_rt <-
  ggplot(data = stimuli_data, aes(x = OV, y = mean_rt, colour = DIFF)) +
  geom_smooth(method = "loess") +
  geom_point() +
  scale_x_continuous(breaks = seq(3, 9, by = 1)) +
  theme_minimal() +
  labs(
    title = "Simulation Mean Decision Response Time by Magnitude",
    subtitle = "Coloured by Alternative Value Difference",
    x = "Magnitude",
    y = "Response Time (seconds)",
    colour = "Value Difference"
  )

PLOT_rt
### Mean Correct Response Time Plot----
PLOT_mcrt <-
  ggplot(data = stimuli_data, aes(x = OV, y = mean_rt_correct, colour = DIFF)) +
  geom_smooth(method = "loess") +
  geom_point() +
  scale_x_continuous(breaks = seq(3, 9, by = 1)) +
  theme_minimal() +
  labs(
    title = "Simulation Mean Correct Response Time by Magnitude",
    subtitle = "Coloured by Alternative Value Difference",
    x = "Magnitude",
    y = "Response Time (seconds)",
    colour = "Value Difference"
  )

PLOT_mcrt
### Mean Error Response Time ----
PLOT_mert <-
  ggplot(data = stimuli_data, aes(x = OV, y = mean_rt_incorrect, colour = DIFF)) +
  geom_smooth(method = "loess") +
  geom_point() +
  scale_x_continuous(breaks = seq(3, 9, by = 1)) +
  theme_minimal() +
  labs(
    title = "Simulation Mean Incorrect Response Time by Magnitude",
    subtitle = "Coloured by Alternative Value Difference",
    x = "Magnitude",
    y = "Response Time (seconds)",
    colour = "Value Difference"
  )

PLOT_mert
# Parameter Data ---------------------------------------------------------------
level_labels <- c("low", "med", "high") # Can be used for most parameters

diff_labels <- c("difficult", "medium", "easy") # How easy is it to make the choice, higher difference is easier

OV_breaks <- c(seq(min(sim_data$OV), max(sim_data$OV), length.out = 4)) # Makes a sequence of 4 between the min and max values

diff_breaks <- c(seq(min(sim_data$DIFF), max(sim_data$DIFF), length.out = 4))

threshold_breaks <- c(seq(min(sim_data$a.intercept), max(sim_data$a.intercept), length.out = 4))

slope_breaks <- c(seq(min(sim_data$a.slope), max(sim_data$a.slope), length.out = 4))

beta_breaks <- c(seq(min(sim_data$beta), max(sim_data$beta), length.out = 4))

lambda_breaks <- c(seq(min(sim_data$lambda), max(sim_data$lambda), length.out = 4))

parameter_data <- sim_data |>
  mutate(
    Resp = case_when( # Changing them for Clarity
      Resp == 1 ~ "correct",
      Resp == 2 ~ "incorrect"
    ),
    #                                                             Keeps the order       Includes Lowest value   Closed on the right side of the sequence (cant go higher)
    OV_level = cut(OV, breaks = OV_breaks, labels = level_labels, ordered_result = TRUE, include.lowest = TRUE, right = TRUE),
    diff_level = case_when(
      DIFF == 0 ~ "equal", # Special case that needs excluding.
      TRUE ~ as.character(cut(DIFF, breaks = diff_breaks, labels = diff_labels, ordered_result = TRUE, include.lowest = TRUE, right = TRUE))
    ) |>
      factor(levels = c("equal", diff_labels), ordered = TRUE),
    threshold_level = cut(a.intercept, breaks = threshold_breaks, labels = level_labels, ordered_result = TRUE, include.lowest = TRUE, right = TRUE),
    collapse_rate = cut(a.slope, breaks = slope_breaks, labels = level_labels, ordered_result = TRUE, include.lowest = TRUE, right = TRUE),
    beta_level = cut(beta, breaks = beta_breaks, labels = level_labels, ordered_result = TRUE, include.lowest = TRUE, right = TRUE),
    lambda_level = cut(lambda, breaks = lambda_breaks, labels = level_labels, ordered_result = TRUE, include.lowest = TRUE, right = TRUE),
    across(c(Resp, DIFF), as.factor)
  ) |>
  group_by(beta_level, lambda_level, OV, OV_level, diff_level, threshold_level, collapse_rate, Resp) |>
  summarise(
    count = n(),
    mean_rt = mean(Time)
  ) |>
  pivot_wider(
    names_from = Resp,
    values_from = c(count, mean_rt)
  ) |>
  mutate(
    total_count = count_correct + count_incorrect,
    mean_rt = (mean_rt_correct * (count_correct / total_count)) +
      (mean_rt_incorrect * (count_incorrect / total_count)),
    accuracy = as.numeric(count_correct / (count_correct + count_incorrect))
  )

## Lambda:Beta Plots ---------------------------------------------------------------
### Lambda:Beta Accuracy -----
PLOT_acc_beta_lambda <-
  ggplot(parameter_data, aes(x = OV, y = accuracy, colour = beta_level:lambda_level)) +
  geom_smooth(method = "loess", # Use polynomial regression
              se = FALSE) + # Remove Standard Error Lines
  facet_wrap(~diff_level,
    axes = "all", # Give each plot its own axis labels
    labeller = as_labeller( # change facets labels 
      c(
        equal = "Equal Values",
        difficult = "Difficult (Close Values)",
        medium = "Medium",
        easy = "Easy (Far Values)"
      )
    )
  ) +
  scale_y_continuous(
    breaks = seq(.4, 1, by = .1),
    labels = scales::percent
  ) +
  scale_x_continuous(breaks = seq(3, 9, by = 1)) +
  theme_minimal() +
  labs(
    title = "Simulation Accuracy by Decision Magnitude",
    subtitle = "Coloured by Beta:Lambda Levels, Faceted by Decision Difficulty",
    x = "Magnitude",
    y = "Accuracy",
    colour = "Beta Level:Lambda Level"
  )

PLOT_acc_beta_lambda

### Lambda:Beta Response Time ----
PLOT_rt_beta_lambda <-
  ggplot(parameter_data, aes(x = OV, y = mean_rt, colour = beta_level:lambda_level)) +
  geom_smooth(method = "loess",
              se = FALSE) +
  facet_wrap(~diff_level,
    axes = "all",
    labeller = as_labeller(
      c(
        equal = "Equal Values",
        difficult = "Difficult (Close Values)",
        medium = "Medium",
        easy = "Easy (Far Values)"
      )
    )
  ) +
  scale_y_continuous(
    breaks = seq(.2, 3, by = 0.2)
  ) +
  scale_x_continuous(breaks = seq(3, 9, by = 1)) +
  theme_minimal() +
  labs(
    title = "Mean Simulation Response Time by Decision Magnitude",
    subtitle = "Coloured by Beta:Lambda Levels, Faceted by Decision Difficulty",
    x = "Magnitude",
    y = "Mean Response Time (Seconds)",
    colour = "Beta Level:Lambda Level"
  )

PLOT_rt_beta_lambda

## Threshold Plots----

### Threshold Accuracy ----
PLOT_acc_thresholds <-
  ggplot(data = parameter_data, aes(x = OV, y = accuracy, colour = threshold_level:collapse_rate)) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  facet_grid(
    rows = vars(beta_level), # Rows represent inhibition levels
    cols = vars(lambda_level),# Cols Represent Leak levels
    scales = "fixed",
    axes = "all",
    labeller = labeller(.rows = as_labeller(
      c(
        low = "Low Inhibition",
        med = "Medium Inhibition",
        high = "High Inhibition"
      )
    ), .cols = as_labeller(
      c(
        low = "Low Leak",
        med = "Medium Leak",
        high = "High Leak"
      )
    ))
  ) +
  scale_x_continuous(breaks = seq(3, 9, by = 1)) +
  scale_y_continuous(
    breaks = seq(.4, 1, by = .05),
    labels = scales::percent
  ) +
  theme_minimal() +
  labs(
    title = "Simulation Accuracy by Decision Magnitude",
    subtitle = "Faceted by Inhibition and Leak Levels, coloured by Threshold and Collapse Rate",
    x = "Magnitude",
    y = "Accuracy",
    colour = "Threshold:Collapse Rate"
  )

PLOT_acc_thresholds

### Threshold Response Time ----
PLOT_rt_thresholds <-
  ggplot(data = parameter_data, aes(x = OV, y = mean_rt, colour = threshold_level:collapse_rate)) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  facet_grid(
    rows = vars(beta_level),
    cols = vars(lambda_level),
    scales = "fixed",
    axes = "all",
    labeller = labeller(.rows = as_labeller(
      c(
        low = "Low Inhibition",
        med = "Medium Inhibition",
        high = "High Inhibition"
      )
    ), .cols = as_labeller(
      c(
        low = "Low Leak",
        med = "Medium Leak",
        high = "High Leak"
      )
    ))
  ) +
  scale_x_continuous(breaks = seq(3, 9, by = 1)) +
  theme_minimal() +
  labs(
    title = "Simulation Response Time by Decision Magnitude",
    subtitle = "Faceted by Inhibition and Leak Levels, coloured by Threshold and Collapse Rate",
    x = "Magnitude",
    y = "Response Time (seconds)",
    colour = "Threshold:Collapse Rate"
  )

PLOT_rt_thresholds
