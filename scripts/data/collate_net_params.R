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
write.csv(combined_df, "data/post/combined_nets.csv", row.names = FALSE)