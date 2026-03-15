library(tidyverse)

# Wrangle Data ----------------------------------------------------------------
# Create a list of all the files with the specified path.
sim_files <- list.files(
  path = "Simulations/Datasets_CollapsingTruncatedLCA_FreeResp",
  full.names = TRUE # Keep full path names
)

sim_data_list <- list()

for (file in sim_files) { # For every file in the sim_files list
  loaded_names <- load(file) # Load the RData file
  data <- get(loaded_names[1])
  sim_data_list[[file]] <- data # Add the Data to the data list under its file name
}

sim_data <- bind_rows(sim_data_list) |>
  mutate(
    across(c(Resp, DIFF), as.factor)
  )


# Data Visualisation ----------------------------------------------------------
## Accuracy ------------------------------------------------------------------
accuracy_data <- sim_data |>
  mutate(Resp = case_when(
    Resp == 1 ~ "correct",
    Resp == 2 ~ "incorrect"
  )) |>
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

ggplot(data = accuracy_data, aes(x = OV, y = accuracy, colour = DIFF)) +
  geom_smooth() +
  geom_point() +
  ylim(0.4, 1) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

## Response Time --------------------------------------------------------------
### Overall Response Time
ggplot(data = sim_data, aes(x = OV, y = Time, colour = DIFF)) +
  geom_smooth(method = lm) +
  geom_point() +
  facet_wrap(~Resp) +
  theme_minimal()

### Mean Correct Response Time
ggplot(data = accuracy_data, aes(x = OV, y = mean_rt_correct, colour = DIFF)) +
         geom_smooth(method = lm) +
         geom_point() +
         theme_minimal()

### Mean Error Response Time

ggplot(data = accuracy_data, aes(x = OV, y = mean_rt_correct, colour = DIFF)) +
  geom_smooth(method = lm) +
  geom_point() +
  theme_minimal()

## Ratio Plots ------------------------------------------------------------
ratio_data <- sim_data |>
  mutate(ratio = )