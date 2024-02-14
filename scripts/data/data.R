# Required library
library(dplyr)

# Function to read CSV data and add net type identifier
read_and_label <- function(file_path, label) {
  data <- read.csv(file_path)
  data$NetType <- label
  return(data)
}

# Function to combine data frames
combine_data_frames <- function(df_list) {
  combined_df <- bind_rows(df_list)
  return(combined_df)
}

# Define file paths and labels
files_and_labels <- list(
  "data/raw/pyrethroid_only_nets.csv" = "PyNets",
  "data/raw/pyrethroid_pbo_nets.csv" = "PyPBONets",
  "data/raw/pyrethroid_pyrrole_nets.csv" = "PyPyroNets"
)

# Read, label, and combine
df_list <- lapply(names(files_and_labels), function(file) {
  read_and_label(file, files_and_labels[[file]])
})

combined_df <- combine_data_frames(df_list)

# Write the combined data frame to a new CSV
write.csv(combined_df, "data/post/combined_nets2.csv", row.names = FALSE)

##############

library(openxlsx)
library(dplyr)
library(countrycode)

# Define the path to the original and the CSV output file
input_file_path <- paste0(getwd(), "/data/raw/SSA_region.xlsx")
output_csv_file_path <- paste0(getwd(), "/data/post/SSA_region.csv")

sheets <- getSheetNames(input_file_path)

# Initialize an empty list to store data frames
all_data_frames <- list()

# Pre-fetch ISO3C codes for all unique countries, excluding the specific ones
all_countries <- lapply(sheets, function(sheet_name) {
  df <- read.xlsx(input_file_path, sheet = sheet_name)
  # Filter out specific countries before fetching unique countries
  filtered_df <- df %>% filter(!Country %in% c("Zanzibar", "STP", "CAR"))
  unique(filtered_df$Country)
})

unique_countries <- unique(unlist(all_countries))
country_to_iso3c <- setNames(countrycode(unique_countries, "country.name", "iso3c", warn = TRUE), unique_countries)

# Process each sheet and store in the list
for(sheet_name in sheets) {
  df <- read.xlsx(input_file_path, sheet = sheet_name)
  
  # Immediately filter out rows where Country is "Zanzibar", "STP", or "CAF"
  df <- df %>% filter(!Country %in% c("Zanzibar", "STP", "CAR"))
  
  # Map ISO3C codes using pre-fetched values for remaining countries
  df$ISO3C <- ifelse(df$Country %in% names(country_to_iso3c), country_to_iso3c[df$Country], NA)
  
  # Insert ISO3C column after Country
  df <- df %>% select(Country, ISO3C, everything())
  
  # Divide specified columns by the 'Total' column, set to 0 if Total is 0
  df <- df %>%
    mutate(across(c(PyNets, PyPBONets, PyPyroNets), ~if_else(Total == 0, 0, ./Total)))
  
  # Add a 'year' column
  df$year <- sheet_name
  
  # Append the data frame to the list
  all_data_frames[[sheet_name]] <- df
}

# Combine all data frames into one
combined_df <- bind_rows(all_data_frames)

# Write the combined data frame to a CSV file
write.csv(combined_df, output_csv_file_path, row.names = FALSE)
