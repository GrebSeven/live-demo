# Load in Packages (Should be loaded if read has just taken place) -----------------------------------------
library(tidyverse)
library(parallel)

## Defining functions for later

make_breaks <- function(parameter){
  c(seq(min(parameter), max(parameter), length.out = 3), Inf)
}

compute_summary_stats <- function(df) {
  df |>
    mutate(
      count_correct = replace_na(count_correct, 0),
      count_incorrect = replace_na(count_incorrect, 0),
      count_none = replace_na(count_none, 0),
      mean_rt_correct = replace_na(mean_rt_correct, 0),
      mean_rt_incorrect = replace_na(mean_rt_incorrect, 0),
      total_count = count_correct + count_incorrect + count_none,
      accuracy = as.numeric(count_correct / (total_count)),
      mean_rt = (mean_rt_correct * (count_correct / total_count)) +
        (mean_rt_incorrect * (count_incorrect / total_count))
    )
}

# Define Summary function ---------------------------------------------------
## Summary for different Stimuli matchups
create_summaries <- function(rds_path) {
  ### Read in data
  sim_data <- readRDS(rds_path)
  
  ### Create names for summaries files
  stim_summary_filename <- gsub("_sim_data.rds", "_stim_summary.rds", basename(rds_path))
  param_summary_filename <- gsub("_sim_data.rds", "_param_summary.rds", basename((rds_path)))
  
  ### Stimuli Summary ======
  stimuli_summary <- sim_data |>
    mutate(
      Resp = case_when( # Changing them for Clarity
        Resp == 1 ~ "correct",
        Resp == 2 ~ "incorrect",
        Resp == -1 ~ "none"
      ),
      across(c(Resp, DIFF), as.factor)
    ) |>
    group_by(OV, DIFF, Resp) |>
    summarise(
      count = n(),
      mean_rt = mean(Time)
    ) |>
    pivot_wider(
      names_from = Resp,
      values_from = c(count, mean_rt)
    ) |>
    compute_summary_stats()
  ### Save stimuli summary as an RDS file, at this location, under this name,
  saveRDS(stimuli_summary, file = file.path("Analyses/Summaries", stim_summary_filename))
  
  ### Parameter Summary =======
  
  #### Create breaks for Levels
  level_labels <- c("low", "med", "high")
  
  diff_labels <- c("difficult", "medium", "easy")
  
  OV_breaks <- make_breaks(sim_data$OV)
  
  diff_breaks <- make_breaks(sim_data$DIFF[sim_data$DIFF != 0]) # Want it to ignore 0 as this is a separate case
  
  threshold_breaks <- make_breaks(sim_data$a.intercept)
  
  slope_breaks <- make_breaks(sim_data$a.slope)
  
  beta_breaks <- make_breaks(sim_data$beta)
  
  lambda_breaks <- make_breaks(sim_data$lambda)
  
  #### Create Parameter Level Dataframe
  parameter_summary <- sim_data |>
    mutate(
      Resp = case_when( # Changing them for Clarity
        Resp == 1 ~ "correct",
        Resp == 2 ~ "incorrect",
        Resp == -1 ~ "none"
      ),
      OV_level = cut(OV, breaks = OV_breaks, labels = level_labels, ordered_result = TRUE, right = FALSE),
      diff_level = case_when(
        DIFF == 0 ~ "equal",
        TRUE ~ as.character(cut(DIFF, breaks = diff_breaks, labels = diff_labels, ordered_result = TRUE, right = FALSE))
      ) |>
        factor(levels = c("equal", diff_labels), ordered = TRUE),
      threshold_level = cut(a.intercept, breaks = threshold_breaks, labels = level_labels, ordered_result = TRUE, right = FALSE),
      collapse_rate = cut(a.slope, breaks = slope_breaks, labels = level_labels, ordered_result = TRUE, right = FALSE),
      beta_level = cut(beta, breaks = beta_breaks, labels = level_labels, ordered_result = TRUE, right = FALSE),
      lambda_level = cut(lambda, breaks = lambda_breaks, labels = level_labels, ordered_result = TRUE, right = FALSE),
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
      count_correct = replace_na(count_correct, 0),
      count_incorrect = replace_na(count_incorrect, 0),
      count_none = replace_na(count_none, 0),
      mean_rt_correct = replace_na(mean_rt_correct, 0),
      mean_rt_incorrect = replace_na(mean_rt_incorrect, 0),
      total_count = count_correct + count_incorrect + count_none,
      mean_rt = (mean_rt_correct * (count_correct / total_count)) +
        (mean_rt_incorrect * (count_incorrect / total_count)),
      accuracy = as.numeric(count_correct / total_count)
    )
  #### Save Parameter Summary
  saveRDS(parameter_summary, file = file.path("Analyses/Summaries", param_summary_filename))
}

# Creating Summaries -------------------------------------------------------------------------------------
## Making lost of all simulation dataframes within specified path
rds_paths <- list.files(path = "Analyses/Data", pattern = "_sim_data.rds", full.names = TRUE)

for (path in rds_paths) {
  stimuli_summary(path)
}