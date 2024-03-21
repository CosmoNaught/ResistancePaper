# Load Libraries
library(dplyr)
library(tidyr)
library(purrr)

source(paste0(getwd(), "/scripts/utils/utils.R"))

# Updated Configuration and Constants
debug <- FALSE # Toggle based on requirement
environment_label <- ifelse(debug, "debug", "final")
measure_type <- "prevalence"  # Can be set to "incidence" or "prevalence"
modes <- "observed" #c("observed", "PyOnly", "PyPBO", "IG2Only", "counterfactual")

iso_codes <- unique(read.csv("D:/Malaria/ResistancePaper/data/post/SSA_region_combined.csv")$ISO3C)
check_iso_codes(iso_codes)
iso_codes <- iso_codes[iso_codes != "CPV"]

iso_codes <- "MLI" #iso_codes

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
    mutate(timestep = floor((timestep / 365) + 2000),  # Convert days to whole years 
          net_types = source_label)
  browser()
  if (measure_type == "incidence") {
    data <- data %>%
      select(timestep, net_types, n_inc_clinical_1825_5474) %>%
      mutate(value = n_inc_clinical_1825_5474 / n_1825_5474) # number of new clinical cases for each timestep for the age range not actual incidence -- to get clinical incidence divide by number of individuals in that age range
  } else if (measure_type == "prevalence") {
    data <- data %>%
      select(timestep, net_types, n_detect_730_3649, n_730_3649) %>%
      mutate(value = n_detect_730_3649 / n_730_3649)
  } else if (measure_type == "cases") {
    data <- data %>%
      select(timestep, net_types, n_detect_730_3649, n_730_3649) %>%
      mutate(value = (n_infections)) ## number of cases in every timestep annually sum them (but convert from sim pop to actual pop so divide by sim pop * actual pop)
    }

  return(data)
}

aggregate_data <- function(data, measure_type) {
  if (measure_type == "incidence") {
    data %>%
      group_by(timestep, net_types) %>%
      summarise(value = mean(value, na.rm = TRUE)) %>%
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
for (iso in iso_codes) {
  for (mode in modes) {
    process_and_combine_data_for_mode_and_iso(mode, iso, measure_type)
  }
}


#######################

# per person for a given time-span

# clinical incidence between 2 time frames (start and end year) per person for each time point, then sum for the year 

# s_date start of sim at 2018
# s_end end of sim at 2022

# so select rows of n_inc_clinical_

# index start date of 2019 to end of 2022 and

calc_incidence <- function(output, s_date, e_date, min_age, max_age){
  s_time <- which(output$date == s_date) 
  e_time <- which(output$date == e_date)
  return(output[s_time:e_time,paste0("n_inc_clinical_",min_age,"_",max_age)]/
  output[s_time:e_time, paste0("n_", min_age,"_", max_age)])
}

# incidence
# multiply by however many cases 
calc_incidence <- function(output, s_index, e_index, min_age, max_age){
  sum(output[s_index:e_index,paste0("n_inc_clinical_",min_age,"_",max_age)]/ 
  output[s_index:e_index, paste0("n_", min_age,"_", max_age)])
}

data <- readRDS("D:/Malaria/ResistancePaper/outputs/raw/sim/final/counterfactual/AGO/pre_model_output_Bengo_rural.RDS")

# cases per person for the 3 year period 2019 start to end 2022
# first day of 2019, to last day of 2022, for the 0 - 5 age range
# number of cases per person over the 3 year for that given region
calc_incidence(data, 365*19, 365*23 - 1, 0, 1824) * 100000

data$n_0_1824

## take average for the 3 year range from data$n_0_1825