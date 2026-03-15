library(tidyverse)

# Wrangle Data ----------------------------------------------------------------
# Create a list of all the files with the specified path.
sim_files <- list.files(path = "Simulations/Datasets_CollapsingTruncatedLCA_FreeResp",
                        full.names = TRUE # Keep full path names
)

sim_data_list <- list()

for (file in sim_files) { #For every file in the sim_files list
  loaded_names <- load(file) # Load the RData file
  data <- get(loaded_names[1])
  sim_data_list[[file]] <- data #Add the Data to the data list under its file name
}

sim_data <- bind_rows(sim_data_list)|>
  mutate(
    across(c(Resp, OV, DIFF), as.factor)
  )

# Data Visualisation ----------------------------------------------------------
## Response Time --------------------------------------------------------------
ggplot(data = sim_data, aes(x = OV, y = Time, colour = DIFF)) +
  geom_smooth(method = lm) +
  facet_wrap(~Resp) +
  theme_minimal()
