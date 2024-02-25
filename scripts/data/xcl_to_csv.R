library(openxlsx)
library(dplyr)
library(countrycode)

# Define the base paths and file names
input_base_path <- paste0(getwd(), "/data/raw/")
output_base_path <- paste0(getwd(), "/data/post/")
input_files <- c("SSA_region_combined.xlsx", "SSA_region_PyNets.xlsx", "SSA_region_Py_PBONets.xlsx")
output_files <- c("SSA_region_combined.csv", "SSA_region_PyNets.csv", "SSA_region_Py_PBONets.csv")

# Loop through each file
for(i in 1:length(input_files)) {
  input_file_path <- paste0(input_base_path, input_files[i])
  output_csv_file_path <- paste0(output_base_path, output_files[i])
  
  sheets <- getSheetNames(input_file_path)
  
  # Initialize an empty list to store data frames
  all_data_frames <- list()
  
  # Pre-fetch ISO3C codes for all unique countries, excluding the specific ones
  all_countries <- lapply(sheets, function(sheet_name) {
    df <- read.xlsx(input_file_path, sheet = sheet_name)
    # Filter out specific countries before fetching unique countries
    filtered_df <- df %>% filter(!Country %in% c("STP", "CAR"))
    unique(filtered_df$Country)
  })
  
  unique_countries <- unique(unlist(all_countries))
  country_to_iso3c <- setNames(countrycode(unique_countries, "country.name", "iso3c", warn = TRUE), unique_countries)
  
  # Process each sheet and store in the list
  for(sheet_name in sheets) {
    df <- read.xlsx(input_file_path, sheet = sheet_name)
    
    # Immediately filter out rows where Country is "STP"
    df <- df %>% filter(!Country %in% c("STP"))
    
    # Map ISO3C codes using pre-fetched values for remaining countries
    df$ISO3C <- ifelse(df$Country %in% names(country_to_iso3c), country_to_iso3c[df$Country], NA)
    
    # Manually assign "CAF" for "CAR" where necessary
    df$ISO3C <- ifelse(df$Country == "CAR", "CAF", df$ISO3C)
    
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
}
