# Load Libraries
library(dplyr)
library(tidyr)
library(purrr)

source(paste0(getwd(), "/scripts/utils/utils.R"))

# Define the incidence calculation function
calc_incidence <- function(output, s_index, e_index, min_age, max_age) {
  sum(output[s_index:e_index, paste0("n_inc_clinical_", min_age, "_", max_age)] / 
      output[s_index:e_index, paste0("n_", min_age, "_", max_age)])
}

# Setup
interventions = c("observed" = "Observed", "PyOnly" = "PyOnly", "PyPBO" = "PyPBO", "IG2Only" = "IG2Only", "counterfactual" = "No Resistance")

iso_codes <- unique(read.csv("D:/Malaria/ResistancePaper/data/post/SSA_region_combined.csv")$ISO3C)
check_iso_codes(iso_codes)
iso_codes <- iso_codes[iso_codes != "CPV"]

age_groups <- list("0_5" = c(0, 5), "5_15" = c(5, 15), "15_100" = c(15, 100)) # Age groups in years

# Initialize data frames for storing results
aggregated_data <- data.frame()
unaggregated_data <- data.frame()

# Process each ISO code and intervention
for (iso in iso_codes) {
  for (mode in names(interventions)) {
    # Define directory path (adjust as necessary)
    dir_path <- sprintf("D:/Malaria/ResistancePaper/outputs/raw/sim/final/%s/%s", mode, iso)
    
    # Read all RDS files for the current ISO code and mode
    files <- list.files(path = dir_path, pattern = "pre_model_output_.*\\.RDS$", full.names = TRUE)
    
    for (file_path in files) {
      data <- readRDS(file_path)
      
      # Extract region name
      region_name <- sub(".*output_(.*)_rural.*", "\\1", basename(file_path))
      
      # Initialize a vector to store incidence rates for current file
      incidence_rates <- setNames(numeric(length(age_groups)), names(age_groups))
      
      # Calculate incidence for each age group
      for (age_group_name in names(age_groups)) {
        age_group <- age_groups[[age_group_name]]
        min_age <- age_group[1]
        max_age <- age_group[2]
        
        # Convert ages to days
        min_age_days <- min_age * 365
        max_age_days <- max_age * 365 - 1
        
        incidence_rates[age_group_name] <- round(calc_incidence(data, s_index = 365 * 19, e_index = 365 * 23 - 1, min_age_days, max_age_days) * 100000)
      }
      
      # Create a temporary data frame to store current file's results
      temp_df <- data.frame(iso = iso, region = region_name, intervention = interventions[mode], t(incidence_rates))
      colnames(temp_df)[4:6] <- paste("Cases", names(incidence_rates), sep = "_")
      
      # Append to unaggregated data frame
      unaggregated_data <- rbind(unaggregated_data, temp_df)
    }
  }
}

# Aggregate data by ISO, intervention, and age group
aggregated_data <- unaggregated_data %>%
  group_by(iso, intervention) %>%
  summarise(across(starts_with("Cases"), sum, na.rm = TRUE), .groups = 'drop')


# Assuming aggregated_data is your dataset
aggregated_cases_averted <- aggregated_data %>%
  filter(intervention %in% c("PyOnly", "IG2Only")) %>%
  group_by(iso) %>%
  summarise(Cases_Averted_0_5 = sum(Cases_0_5[intervention == "PyOnly"]) - sum(Cases_0_5[intervention == "IG2Only"]),
            Cases_Averted_5_15 = sum(Cases_5_15[intervention == "PyOnly"]) - sum(Cases_5_15[intervention == "IG2Only"]),
            Cases_Averted_15_100 = sum(Cases_15_100[intervention == "PyOnly"]) - sum(Cases_15_100[intervention == "IG2Only"]))

unaggregated_filtered <- unaggregated_data %>%
  filter(intervention %in% c("PyOnly", "IG2Only"))

# Calculate cases averted for each region by subtracting IG2Only from PyOnly
unaggregated_cases_averted <- unaggregated_filtered %>%
  group_by(iso, region) %>%
  summarise(Cases_Averted_0_5 = sum(Cases_0_5[intervention == "PyOnly"]) - sum(Cases_0_5[intervention == "IG2Only"]),
            Cases_Averted_5_15 = sum(Cases_5_15[intervention == "PyOnly"]) - sum(Cases_5_15[intervention == "IG2Only"]),
            Cases_Averted_15_100 = sum(Cases_15_100[intervention == "PyOnly"]) - sum(Cases_15_100[intervention == "IG2Only"]))


unaggregated_data$row.names <- NULL
unaggregated_cases_averted$row.names <- NULL

View(aggregated_data)
View(unaggregated_data)
View(aggregated_cases_averted)
View(unaggregated_cases_averted)

output_dir <- paste0(getwd(), "/outputs/tables")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

saveRDS(unaggregated_data, paste0(output_dir, "/unaggregated_data.RDS"))
write.csv(unaggregated_data, paste0(output_dir, "/unaggregated_data.csv"), row.names = FALSE)


saveRDS(aggregated_data, paste0(output_dir, "/aggregated_data.RDS"))
write.csv(aggregated_data, paste0(output_dir, "/aggregated_data.csv"), row.names = FALSE)

saveRDS(aggregated_cases_averted, paste0(output_dir, "/aggregated_cases_averted.RDS"))
write.csv(aggregated_cases_averted, paste0(output_dir, "/aggregated_cases_averted.csv"), row.names = FALSE)

saveRDS(unaggregated_cases_averted, paste0(output_dir, "/unaggregated_cases_averted.RDS"))
write.csv(unaggregated_cases_averted, paste0(output_dir, "/unaggregated_cases_averted.csv"), row.names = FALSE)