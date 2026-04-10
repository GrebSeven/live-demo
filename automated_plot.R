# SET HOW MNAY CORES TO USE!!!!! ----------------------------------

how_many_cores <- 3

# Load in Packages--------------------------------------
library(tidyverse)
library(parallel)

# Read in Summaries ----------------------------

stim_sum_paths <- list.files(path = "Analyses/Summaries", pattern = "_stim_summary.rds", full.names = TRUE)

param_sum_paths <- list.files(path = "Analyses/Summaries", pattern = "_param_summary.rds", full.names = TRUE)

# Plot Functions -------------------------------------

plot_stimuli <- function(stim_sum_path) {
  
  stimuli_data <- readRDS(stim_sum_path)
  
  summary_name <- basename(stim_sum_path)
  
  ## Plot Accuracy ======
  
  PLOT_acc <-
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
      title = "Simulation Decision Accuracy by Magnitude",
      subtitle = "Coloured by Alternative Value Difference",
      x = "Magnitude",
      y = "Accuracy",
      colour = "Value Difference"
    )
  
  PLOT_acc_name <- gsub("_stim_summary\\.rds", "_PLOT_acc.png", summary_name)
  
  ggsave(PLOT_acc_name, plot = PLOT_acc, path = "Analyses/Plots")
  
  ## Plot Response Times ======
  
  PLOT_rt <-
    ggplot(data = stimuli_data, aes(x = OV, y = mean_rt, colour = DIFF)) +
    geom_smooth(method = "loess") +
    geom_point() +
    scale_x_continuous(breaks = seq(3, 9, by = 1)) +
    theme_minimal() +
    labs(
      title = "Simulation Mean Decision Response Time by Magnitude",
      subtitle = "Coloured by Alternative Value Difference",
      x = "Magnitude",
      y = "Response Time (seconds)",
      colour = "Value Difference"
    )
  PLOT_rt_name <- gsub("_stim_summary\\.rds", "_PLOT_rt.png", summary_name)
  
  ggsave(PLOT_rt_name, plot = PLOT_rt, path = "Analyses/Plots")
  
  ## Plot Mean Correct Response Times ====
  
  PLOT_mcrt <-
    ggplot(data = stimuli_data, aes(x = OV, y = mean_rt_correct, colour = DIFF)) +
    geom_smooth(method = "loess") +
    geom_point() +
    scale_x_continuous(breaks = seq(3, 9, by = 1)) +
    theme_minimal() +
    labs(
      title = "Simulation Mean Correct Response Time by Magnitude",
      subtitle = "Coloured by Alternative Value Difference",
      x = "Magnitude",
      y = "Response Time (seconds)",
      colour = "Value Difference"
    )
  
  PLOT_mcrt_name <- gsub("_stim_summary\\.rds", "_PLOT_mcrt.png", summary_name)
  
  ggsave(PLOT_mcrt_name, plot = PLOT_mcrt, path = "Analyses/Plots")
  
  ## Plot Mean Error Response Times ======
  
  PLOT_mert <-
    ggplot(data = stimuli_data, aes(x = OV, y = mean_rt_incorrect, colour = DIFF)) +
    geom_smooth(method = "loess") +
    geom_point() +
    scale_x_continuous(breaks = seq(3, 9, by = 1)) +
    theme_minimal() +
    labs(
      title = "Simulation Mean Incorrect Response Time by Magnitude",
      subtitle = "Coloured by Alternative Value Difference",
      x = "Magnitude",
      y = "Response Time (seconds)",
      colour = "Value Difference"
    )
  
  PLOT_mert_name <- gsub("_stim_summary\\.rds", "_PLOT_mert.png", summary_name)
  
  ggsave(PLOT_mert_name, plot = PLOT_mert, path = "Analyses/Plots")
}

plot_parameters <- function(param_sum_path) {
  
  parameter_data <- readRDS(param_sum_path)
  
  summary_name <- basename(param_sum_path)
  
  ## Plot Beta:Lambda Accuracy ======
  
  PLOT_acc_beta_lambda <-
    ggplot(parameter_data, aes(x = OV, y = accuracy, colour = beta_level:lambda_level)) +
    geom_smooth(method = "loess",
                se = FALSE) +
    facet_wrap(~diff_level,
               axes = "all",
               labeller = as_labeller(
                 c(
                   equal = "Equal Values",
                   difficult = "Difficult (Close Values)",
                   medium = "Medium",
                   easy = "Easy (Far Values)"
                 )
               )
    ) +
    scale_y_continuous(
      breaks = seq(.4, 1, by = .1),
      labels = scales::percent
    ) +
    scale_x_continuous(breaks = seq(3, 9, by = 1)) +
    theme_minimal() +
    labs(
      title = "Simulation Accuracy by Decision Magnitude",
      subtitle = "Coloured by Beta:Lambda Levels, Faceted by Decision Difficulty",
      x = "Magnitude",
      y = "Accuracy",
      colour = "Beta Level:Lambda Level"
    )
  
  PLOT_BL_acc_name <- gsub("_stim_summary\\.rds", "_PLOT_Beta:Lambda_acc.png", summary_name)
  
  ggsave(PLOT_BL_acc_name, plot = PLOT_acc_beta_lambda, path = "Analyses/Plots")
  
  ## Plot Beta:Lambda Response Times =====
  
  PLOT_rt_beta_lambda <-
    ggplot(parameter_data, aes(x = OV, y = mean_rt, colour = beta_level:lambda_level)) +
    geom_smooth(method = "loess",
                se = FALSE) +
    facet_wrap(~diff_level,
               axes = "all",
               labeller = as_labeller(
                 c(
                   equal = "Equal Values",
                   difficult = "Difficult (Close Values)",
                   medium = "Medium",
                   easy = "Easy (Far Values)"
                 )
               )
    ) +
    scale_y_continuous(
      breaks = seq(.2, 3, by = 0.2)
    ) +
    scale_x_continuous(breaks = seq(3, 9, by = 1)) +
    theme_minimal() +
    labs(
      title = "Mean Simulation Response Time by Decision Magnitude",
      subtitle = "Coloured by Beta:Lambda Levels, Faceted by Decision Difficulty",
      x = "Magnitude",
      y = "Mean Response Time (Seconds)",
      colour = "Beta Level:Lambda Level"
    )
  
  PLOT_BL_RT_name <- gsub("_stim_summary\\.rds", "_PLOT_Beta:Lambda_RT.png", summary_name)
  
  ggsave(PLOT_BL_RT_name, plot = PLOT_rt_beta_lambda, path = "Analyses/Plots")
  ## Plot Threshold Accuracy =====
  
  PLOT_threshold_acc <-
    ggplot(data = parameter_data, aes(x = OV, y = accuracy, colour = threshold_level:collapse_rate)) +
    geom_smooth(
      method = "loess",
      se = FALSE
    ) +
    facet_grid(
      rows = vars(beta_level),
      cols = vars(lambda_level),
      scales = "fixed",
      axes = "all",
      labeller = labeller(.rows = as_labeller(
        c(
          low = "Low Inhibition",
          med = "Medium Inhibition",
          high = "High Inhibition"
        )
      ), .cols = as_labeller(
        c(
          low = "Low Leak",
          med = "Medium Leak",
          high = "High Leak"
        )
      ))
    ) +
    scale_x_continuous(breaks = seq(3, 9, by = 1)) +
    scale_y_continuous(
      breaks = seq(.4, 1, by = .05),
      labels = scales::percent
    ) +
    theme_minimal() +
    labs(
      title = "Simulation Accuracy by Decision Magnitude",
      subtitle = "Faceted by Inhibition and Leak Levels, coloured by Threshold and Collapse Rate",
      x = "Magnitude",
      y = "Accuracy",
      colour = "Threshold:Collapse Rate"
    )
  
  PLOT_threshold_acc_name <- gsub("_stim_summary\\.rds", "_PLOT_Threshold_acc.png", summary_name)
  
  ggsave(PLOT_threshold_acc_name, plot = PLOT_threshold_acc, path = "Analyses/Plots")
  
  ## Plot Threshold Response Time
  
  PLOT_rt_thresholds <-
    ggplot(data = parameter_data, aes(x = OV, y = mean_rt, colour = threshold_level:collapse_rate)) +
    geom_smooth(
      method = "loess",
      se = FALSE
    ) +
    facet_grid(
      rows = vars(beta_level),
      cols = vars(lambda_level),
      scales = "fixed",
      axes = "all",
      labeller = labeller(.rows = as_labeller(
        c(
          low = "Low Inhibition",
          med = "Medium Inhibition",
          high = "High Inhibition"
        )
      ), .cols = as_labeller(
        c(
          low = "Low Leak",
          med = "Medium Leak",
          high = "High Leak"
        )
      ))
    ) +
    scale_x_continuous(breaks = seq(3, 9, by = 1)) +
    theme_minimal() +
    labs(
      title = "Simulation Response Time by Decision Magnitude",
      subtitle = "Faceted by Inhibition and Leak Levels, coloured by Threshold and Collapse Rate",
      x = "Magnitude",
      y = "Response Time (seconds)",
      colour = "Threshold:Collapse Rate"
    )
  
  PLOT_threshold_rt_name <- gsub("_stim_summary\\.rds", "_PLOT_Threshold_rt.png", summary_name)
  
  ggsave(PLOT_threshold_rt_name, plot = PLOT_threshold_art, path = "Analyses/Plots")
}

# Execute Functions -----------------------------------------

mclapply(stim_sum_paths, plot_stimuli, mc.cores = how_many_cores)

mclapply(param_sum_paths, plot_parameters, mc.cores = how_many_cores)