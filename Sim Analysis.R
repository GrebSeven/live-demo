library(tidyverse)

# Wrangle Data ----------------------------------------------------------------
# Create a list of all the files with the specified path.
sim_files <- list.files(
  path = "Simulations/Datasets_CollapsingTruncatedLCA_FreeResp", # path to Sim folder
  full.names = TRUE # Keep full path names
)

sim_data_list <- list()

for (file in sim_files) { # For every file in the sim_files list
  loaded_names <- load(file) # Load the RData file
  data <- get(loaded_names[1]) # Get data for specified file name
  sim_data_list[[file]] <- data # Add the Data to the data list under its file name
}

sim_data <- bind_rows(sim_data_list) |> # Bind all data in the list together
  mutate(
    across(c(Resp, DIFF), as.factor) # Turn Resp and Diff into factors. Useful for Visualisations.
  )


# Data Visualisation ----------------------------------------------------------
## Accuracy ------------------------------------------------------------------
accuracy_data <- sim_data |>
  mutate(Resp = case_when( # Changing them for Clarity 
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
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    title = "Decision Accuracy of Simulations by Magnitude, Differentiated by Alternative Value Difference",
    x = "Magnitude",
    y = "Accuracy",
    colour = "Value Difference"
  )

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