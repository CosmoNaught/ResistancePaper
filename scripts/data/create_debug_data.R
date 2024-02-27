library(readr)
library(dplyr)

# Step 2: Read the CSV file
file_path <- "D:/Malaria/ResistancePaper/data/post/SSA_region_combined.csv"
df <- read_csv(file_path)

# Step 3: Zero in specific columns
columns_to_zero <- c("PyNets", "PyPBONets", "PyPyroNets", "Total")
df <- df %>%
  mutate(across(all_of(columns_to_zero), ~ 0))

# Step 4: Write the modified data frame to a new CSV file
output_path <- "D:/Malaria/ResistancePaper/data/post/SSA_region_debug.csv"
write_csv(df, output_path)