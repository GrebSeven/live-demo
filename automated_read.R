# Set How Many Cores to USE!!!!!!!!!!!!!!!!!!!!!!! ---------------------------
how_many_cores = 3
# Load in Packages ---------------------------------------------------------

library(tidyverse)
library(parallel)

# Create Directory Structure ---------------------------------------------------------------

dir.create("Analyses/Data", recursive = TRUE, showWarnings = FALSE)
dir.create("Analyses/Summaries", recursive = TRUE, showWarnings = FALSE)
dir.create("Analyses/Plots", recursive = TRUE, showWarnings = FALSE)

# Read and Load in Data -------------------------------------------------------------------

## Function For Loading Sim data and turning into a dataframe

sim_load <- function(file){
  loaded_names <- load(file) # Load RData file
  data <- as.data.frame(get(loaded_names[1])) # retrieve first object as a data frame
  gen_param_df <- as.data.frame(t(genParams)) # Append genparams as new columns
  data <- cbind(data, gen_param_df[rep(1, nrow(data)), , drop = FALSE]) #bind param data, repeating through all data points.
  colnames(data) <- c("Time", "Resp", "OV", "DIFF", names(genParams)) # Assign column names
}

## List directories containing Simulations
dir_paths <- list.dirs(path = "~/Blake/Simulations", full.names = TRUE, recursive = FALSE)

##Create data frame for each simulation set in the directory paths lists
for (i in dir_paths) {
  dir_name <- basename(i)
  
  sim_files <- list.files(
    path = i,
    full.names = TRUE
  )
  
  sim_data_list <- mclapply(sim_files, sim_load, mc.cores = how_many_cores)
  
  sim_data <- bind_rows(sim_data_list)
  
  saveRDS(sim_data, file = file.path("Analyses/Data", paste0(dir_name, "_sim_data.rds"))) #Create a unique RDS file for each Sim Type
  
  rm(sim_data, sim_data_list)
}


