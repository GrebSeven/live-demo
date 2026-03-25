library(tidyverse)

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
  data <- cbind(data, gen_param_df[rep(1, nrow(data)), , drop = FALSE])
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
    across(c(Resp, DIFF), as.factor)
  ) |>
  group_by(OV, DIFF, Resp) |>
  summarise(
    Count = n(),
    mean_rt = mean(Time)
  ) |>
  pivot_wider(
    names_from = Resp,
    values_from = c(Count, mean_rt)
  ) |>
  mutate(accuracy = as.numeric(Count_correct / (Count_correct + Count_incorrect)))
## Accuracy Plots ------------------------------------------------------------------
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
    title = "Decision Accuracy of Simulations by Magnitude, Differentiated by Alternative Value Difference",
    x = "Magnitude",
    y = "Accuracy",
    colour = "Value Difference"
  )

ggsave("accuracy_plot.pdf",
  width = 21, height = 15,
  units = "cm", path = "~/UoL/Live-DeMo"
)

## Response Time ---------------------------------------------------------------
### Overall Response Time PLot----
ggplot(data = sim_data, aes(x = OV, y = Time, colour = DIFF)) +
  geom_smooth(method = "loess") +
  geom_point() +
  facet_wrap(~Resp) +
  theme_minimal()

### Mean Correct Response Time Plot----
ggplot(data = stimuli_data, aes(x = OV, y = mean_rt_correct, colour = DIFF)) +
  geom_smooth(method = "loess") +
  geom_point() +
  theme_minimal()

### Mean Error Response Time

ggplot(data = stimuli_data, aes(x = OV, y = mean_rt_incorrect, colour = DIFF)) +
  geom_smooth(method = "loess") +
  geom_point() +
  theme_minimal()

# Parameter Data ---------------------------------------------------------------
level_labels <- c("low", "med", "high")

OV_breaks <- c(seq(min(sim_data$OV), max(sim_data$OV), length.out = 3), Inf)

slope_breaks <- c(seq(min(sim_data$a.slope), max(sim_data$a.slope), length.out = 3), Inf)

beta_breaks <- c(seq(min(sim_data$beta), max(sim_data$beta), length.out = 3), Inf)

lambda_breaks <- c(seq(min(sim_data$lambda), max(sim_data$lambda), length.out = 3), Inf)

parameter_data <- sim_data |>
  filter(DIFF != 0) |>
  mutate(
    across(c(Resp, DIFF), as.factor),
    Resp = case_when( # Changing them for Clarity
      Resp == 1 ~ "correct",
      Resp == 2 ~ "incorrect"
    ),
    OV_level = cut(OV, breaks = OV_breaks, labels = level_labels, ordered_result = TRUE, right = FALSE),
    slope_degrees = cut(a.slope, breaks = slope_breaks, labels = level_labels, right = FALSE),
    beta_level = cut(beta, breaks = beta_breaks, labels = level_labels, ordered_result = TRUE, right = FALSE),
    lambda_level = cut(lambda, breaks = lambda_breaks, labels = level_labels, ordered_result = TRUE, right = FALSE)
  )

beta_lambda_data <- parameter_data |>
  group_by(beta_level, lambda_level, OV, OV_level, Resp) |>
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
    accuracy = as.numeric(count_correct / (count_correct + count_incorrect)),
    ratio = as.numeric(mean_rt / accuracy)
  )

## Test Plots ---------------------------------------------------------------

ggplot(parameter_data, aes(y = Time, x = beta_level, colour = OV_level)) +
  geom_boxplot(outliers = FALSE) +
  facet_wrap(~lambda_level)

ggplot(beta_lambda_data, aes(x = OV, y = accuracy, colour = beta_level:lambda_level)) +
  geom_smooth()

ggplot(beta_lambda_data, aes(x = OV, y = ratio, colour = beta_level:lambda_level)) +
  geom_smooth()
