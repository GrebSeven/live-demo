# How Many Cores ?!?!?!?!?!!
how_many_cores <- 110
# Load in Packages (Should be loaded if read has just taken place) -----------------------------------------
library(tidyverse)
library(parallel)

## Defining functions for later

make_breaks <- function(parameter) {
  c(seq(min(parameter), max(parameter), length.out = 3), Inf)
  return(parameter)
}

compute_summary_stats <- function(df) {
  df %>%
    mutate(
      count_none = if ("count_none" %in% names(.)) count_none else NA, # Need this because in some simulations it always reaches an answer. Stops errors being thrown.
      count_correct = replace_na(count_correct, 0),
      count_incorrect = replace_na(count_incorrect, 0),
      count_none = replace_na(count_none, 0),
      mean_rt_correct = replace_na(mean_rt_correct, 0),
      mean_rt_incorrect = replace_na(mean_rt_incorrect, 0),
      total_count = sum(count_correct, count_incorrect, count_none, na.rm = TRUE),
      accuracy = as.numeric(count_correct / (total_count)),
      mean_rt = (mean_rt_correct * (count_correct / total_count)) +
        (mean_rt_incorrect * (count_incorrect / total_count))
    )
  return()
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
    ) %>%
    mutate(
      count_none = if ("count_none" %in% names(.)) count_none else NA, # Need this because in some simulations it always reaches an answer. Stops errors being thrown.
      count_correct = replace_na(count_correct, 0),
      count_incorrect = replace_na(count_incorrect, 0),
      count_none = replace_na(count_none, 0),
      mean_rt_correct = replace_na(mean_rt_correct, 0),
      mean_rt_incorrect = replace_na(mean_rt_incorrect, 0),
      total_count = sum(count_correct, count_incorrect, count_none, na.rm = TRUE),
      accuracy = as.numeric(count_correct / (total_count)),
      mean_rt = (mean_rt_correct * (count_correct / total_count)) +
        (mean_rt_incorrect * (count_incorrect / total_count))
    )
  ### Save stimuli summary as an RDS file, at this location, under this name,
  saveRDS(stimuli_summary, file = file.path("Analyses/Summaries", stim_summary_filename))

  ### Parameter Summary =======

  #### Create breaks for Levels
  level_labels <- c("low", "med", "high")

  diff_labels <- c("difficult", "medium", "easy")

  OV_breaks <- c(seq(min(sim_data$OV), max(sim_data$OV), length.out = 4))
  
  diff_breaks <- c(seq(min(sim_data$DIFF), max(sim_data$DIFF), length.out = 4))

  threshold_breaks <- c(seq(min(sim_data$a.intercept), max(sim_data$a.intercept), length.out = 4))
  
  slope_breaks <- c(seq(min(sim_data$a.slope), max(sim_data$a.slope), length.out = 4))
  
  beta_breaks <- c(seq(min(sim_data$beta), max(sim_data$beta), length.out = 4))
  
  lambda_breaks <- c(seq(min(sim_data$lambda), max(sim_data$lambda), length.out = 4))

  #### Create Parameter Level Data frame
  parameter_summary <- sim_data |>
    mutate(
      Resp = case_when( # Changing them for Clarity
        Resp == 1 ~ "correct",
        Resp == 2 ~ "incorrect",
        Resp == -1 ~ "none"
      ),
      OV_level = cut(OV, breaks = OV_breaks, labels = level_labels, ordered_result = TRUE, include.lowest = TRUE, right = TRUE),
      diff_level = case_when(
        DIFF == 0 ~ "equal",
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
    ) %>%
    mutate(
      count_none = if ("count_none" %in% names(.)) count_none else NA, # Need this because in some simulations it always reaches an answer. Stops errors being thrown.
      count_correct = replace_na(count_correct, 0),
      count_incorrect = replace_na(count_incorrect, 0),
      count_none = replace_na(count_none, 0),
      mean_rt_correct = replace_na(mean_rt_correct, 0),
      mean_rt_incorrect = replace_na(mean_rt_incorrect, 0),
      total_count = sum(count_correct, count_incorrect, count_none, na.rm = TRUE),
      accuracy = as.numeric(count_correct / (total_count)),
      mean_rt = (mean_rt_correct * (count_correct / total_count)) +
        (mean_rt_incorrect * (count_incorrect / total_count))
    )
  #### Save Parameter Summary
  saveRDS(parameter_summary, file = file.path("Analyses/Summaries", param_summary_filename))
}

# Creating Summaries -------------------------------------------------------------------------------------
## Making lost of all simulation dataframes within specified path
rds_paths <- list.files(path = "Analyses/Data", pattern = "_sim_data.rds", full.names = TRUE)

mclapply(rds_paths, create_summaries, mc.cores = how_many_cores)
