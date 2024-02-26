# Load Libraries
library(dplyr)
library(tidyr)
library(purrr)

# Updated Configuration and Constants
debug <- TRUE # Toggle based on requirement
environment_label <- ifelse(debug, "debug", "final")
measure_type <- "prevalence"  # Can be set to "incidence" or "prevalence"
# modes <- c("observed", "PyOnly", "PyPBO", "counterfactual")
modes <- c("observed", "debug", "counterfactual")

isos <- c("MLI")  # List of ISO codes to iterate over

# Adjusted Directory Paths Function
get_directory_paths <- function(environment_label, mode) {
  base_dir <- paste0(getwd(), "/outputs/raw/sim/", environment_label, "/", mode, "/")
  post_dir_base <- paste0(getwd(), "/outputs/post/", environment_label, "/", mode, "/")
  list(base_dir = base_dir, post_dir_base = post_dir_base)
}

  # Utility Functions
  preprocess_data <- function(file_path, source_label, measure_type) {
    data <- readRDS(file_path)
    data <- data %>%
      mutate(timestep = floor((timestep / 365.25) + 2000),  # Convert days to whole years
            net_types = source_label)

    if (measure_type == "incidence") {
      data <- data %>%
        select(timestep, net_types, n_inc_clinical_1825_5474) %>%
        mutate(value = n_inc_clinical_1825_5474)
    } else if (measure_type == "prevalence") {
      data <- data %>%
        select(timestep, net_types, n_detect_730_3649, n_730_3649) %>%
        mutate(value = n_detect_730_3649 / n_730_3649)
    }

    return(data)
  }

  aggregate_data <- function(data, measure_type) {
    if (measure_type == "incidence") {
      data %>%
        group_by(timestep, net_types) %>%
        summarise(value = sum(value, na.rm = TRUE)) %>%
        ungroup()
    } else if (measure_type == "prevalence") {
      data %>%
        group_by(timestep, net_types) %>%
        summarise(value = mean(value, na.rm = TRUE)) %>%
        ungroup()
    }
  }

# Main Execution Logic Adjusted for New Modes and ISOs
process_and_combine_data_for_mode_and_iso <- function(mode, iso, measure_type) {
  dir_paths <- get_directory_paths(environment_label, mode)
  files <- list.files(path = paste0(dir_paths$base_dir, iso, "/"), pattern = "*.RDS", full.names = TRUE)
  datasets <- lapply(files, preprocess_data, mode, measure_type) %>% bind_rows()
  aggregated_data <- aggregate_data(datasets, measure_type)
  
  # Construct the file path for the processed data
  post_dir <- paste0(dir_paths$post_dir_base, iso, "/")
  processed_filename <- paste0(post_dir, "post_model_output_", iso, "_", mode, "_", measure_type, ".RDS")
  
  # Ensure the directory for the processed data exists
  if (!dir.exists(post_dir)) {
    dir.create(post_dir, recursive = TRUE)
  }

  # Save the processed data
  saveRDS(aggregated_data, file = processed_filename)
}

# Execution for Each Mode and ISO
for (iso in isos) {
  for (mode in modes) {
    process_and_combine_data_for_mode_and_iso(mode, iso, measure_type)
  }
}