# Load necessary libraries
library(dplyr)
library(countrycode)

# Read the CSV file
data <- read.csv("D:/Malaria/ResistancePaper/data/raw/master/BASF nets model input 2024.csv", stringsAsFactors = FALSE)

# Remove duplicate countries, keeping only the first occurrence
data_unique <- data %>% distinct(Country, .keep_all = TRUE)

# Sort the data frame by Country alphabetically
data_sorted <- data_unique %>% arrange(Country)
data_sorted$year <- as.character(data_sorted$year)

# Add ISO3C codes
data_sorted$ISO3C <- countrycode(data_sorted$Country, "country.name", "iso3c")

# Relocate the ISO3C column to be between Country and the next column (assumed to be 'year' here)
data_sorted <- data_sorted %>% relocate(ISO3C, .after = Country)

# Write the modified data frame to a new CSV file
write.csv(data_sorted, "D:/Malaria/ResistancePaper/data/raw/IG2_instructions.csv", row.names = FALSE, quote = TRUE)
####################

# Load necessary libraries
library(dplyr)
library(readr) # For reading CSV files

# Read the CSV files
ssa_data <- read_csv("D:/Malaria/ResistancePaper/data/post/SSA_region_combined.csv")
ig2_instructions <- read_csv("D:/Malaria/ResistancePaper/data/raw/IG2_instructions.csv")

# For each country and year in ig2_instructions, update ssa_data
for(i in 1:nrow(ig2_instructions)) {
  country <- ig2_instructions$Country[i]
  year <- as.integer(ig2_instructions$year[i])
  
  # Identify rows to update based on matching country and year or later
  update_rows <- ssa_data$Country == country & as.integer(ssa_data$year) >= year
  
  # Update specified columns
  ssa_data$PyNets[update_rows] <- 0
  ssa_data$PyPBONets[update_rows] <- 0
  ssa_data$PyPyroNets[update_rows] <- 1
}

ssa_data$year <- as.character(ssa_data$year)
# Write the updated data frame to a new CSV file
write.csv(ssa_data, "D:/Malaria/ResistancePaper/data/post/SSA_region_IG2.csv", row.names = FALSE, quote = TRUE)