# Load in Packages (Should be loaded if read has just taken place) -----------------------------------------
library(tidyverse)
library(parallel)

## Defining labels for Later (keeps them out of loops)

level_labels <- c("low", "med", "high")

diff_labels <- c("difficult", "medium", "easy")

# Define Summary function ---------------------------------------------------
## Summary for different Stimuli matchups
create_summaries <- function(rds_path) {
  ### Read in data
  sim_data <- readRDS(rds_path)
  
  ### Create names for summaries files
  stim_summary_filename <- gsub("_sim_data.rds", "_stim_summary.rds", basename(rds_path))
  param_summary_filename <- gsub("_sim_data.rds", "_param_summary.rds", basename((rds_path)))
  
  ### Stimuli Summary ======
  stimuli_data <- sim_data |>
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
    mutate(
      count_correct = if("count_correct" %in% names(pick(everything()))) count_correct else 0,
      count_incorrect = if("count_incorrect" %in% names(pick(everything()))) count_incorrect else 0,
      count_none = if("count_none" %in% names(pick(everything()))) count_none else 0,
      mean_rt_correct = if("mean_rt_correct" %in% names(pick(everything()))) mean_rt_correct else 0,
      mean_rt_incorrect = if("mean_rt_incorrect" %in% names(pick(everything()))) mean_rt_incorrect else 0,
      total_count = count_correct + count_incorrect + count_none,
      accuracy = as.numeric(count_correct / (total_count)),
      mean_rt = (mean_rt_correct * (count_correct / total_count)) +
        (mean_rt_incorrect * (count_incorrect / total_count))
    )
  ### Save stimuli sumarry as an RDS file, at this location, under this name,
  saveRDS(stimuli_data, file = file.path("Analyses/Summaries", stim_summary_filename))
  
  ### Parameter Summary =======
  
  
}

## Making lost of all simulation dataframes within specified path
rds_paths <- list.files(path = "Analyses/Data", pattern = "_sim_data.rds", full.names = TRUE)

for (path in rds_paths) {
  stimuli_summary(path)
}