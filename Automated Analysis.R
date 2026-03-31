dir_paths <- list.dirs(path = "Simulations", full.names = FALSE, recursive = TRUE)

for (i in dir_paths) {
  dir_name <- basename(i)
  
  dir.create(file.path("Analyses", dir_name), recursive = FALSE, showWarnings = FALSE)
  
  sim_files <- vector("list")
  
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
  sim_data <- vector("list")
  
  sim_data <- bind_rows(sim_data_list)
}
