library(tidyverse)
# Read in Data ---------------------------------------------------------------
dir_paths <- list.dirs(path = "Simulations", full.names = TRUE, recursive = FALSE)

dir.create("Analyses/Data", recursive = TRUE, showWarnings = FALSE)
dir.create("Analyses/Summaries", recursive = TRUE, showWarnings = FALSE)
dir.create("Analyses/Plots", recursive = TRUE, showWarnings = FALSE)

for (i in dir_paths) {
  dir_name <- basename(i)
  
  sim_files <- list.files(
    path = i,
    full.names = TRUE
  )
  
  sim_data_list <- vector("list")
  
  for (file in sim_files) {
    loaded_names <- load(file) # Load RData file
    data <- as.data.frame(get(loaded_names[1])) # retrieve first object as a data frame
    gen_param_df <- as.data.frame(t(genParams)) # Append genparams as new columns
    data <- cbind(data, gen_param_df[rep(1, nrow(data)), , drop = FALSE]) #bind param data, repeating through all data points.
    colnames(data) <- c("Time", "Resp", "OV", "DIFF", names(genParams)) # Assign column names
    sim_data_list[[file]] <- data # Store in list using file name as key
  }
  
  sim_data <- bind_rows(sim_data_list)
  
  saveRDS(sim_data, file = file.path("Analyses/Data", paste0(dir_name, "_sim_data.rds"))) #Create a unique RDS file for each Sim Type
  
  rm(sim_data, sim_data_list)
  
  gc()
}

# Summarising Data -------------------------------------------------------------

stimuli_summary <- function(rds_path) {
  sim_data <- readRDS(rds_path)
  
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
  
  stim_summary_path <- gsub("_sim_data.rds", "_stim_summary.rds", basename(rds_path))
  
  saveRDS(stimuli_data, file = file.path("Analyses/Summaries", stim_summary_path))
  
  rm(sim_data)
  
  gc()
}

rds_paths <- list.files(path = "Analyses/Data", pattern = "_sim_data.rds", full.names = TRUE)

for (path in rds_paths) {
  stimuli_summary(path)
}
