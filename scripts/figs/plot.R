# Load Libraries
library(ggplot2)

# Configuration and Constants
debug <- TRUE
iso <- "NER"
environment_label <- ifelse(debug, "debug", "final")
plot_base_dir <- "D:/Malaria/ResistancePaper/outputs/figs/"
plot_dir <- paste0(plot_base_dir, environment_label, "/")

# Hardcoded RDS file paths
counterfactual_file_path <- "D:/Malaria/ResistancePaper/outputs/post/final/counterfactual/NER/post_model_output_NER_counterfactual_prevalence.RDS"
current_file_path <- "D:/Malaria/ResistancePaper/outputs/post/final/current/NER/post_model_output_NER_current_prevalence.RDS"

# Load the RDS files
counterfactual_data <- readRDS(counterfactual_file_path)
current_data <- readRDS(current_file_path)

# Ensure the plot directory exists
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# Plotting Function, incorporating the white background and other specifics
create_comparison_plot <- function(current_data, counterfactual_data, iso, environment_label, data_type) {
  # Assuming both datasets have 'timestep' and 'value' columns for plotting
  # Combine the data
  combined_data <- rbind(current_data, counterfactual_data)
  
  # Modify 'net_types' column in combined_data for plotting purposes
  combined_data$net_types <- ifelse(row.names(combined_data) %in% row.names(current_data), 'Current', 'Counterfactual')
  
  # Create the plot
  plot <- ggplot(combined_data, aes(x = timestep, y = value, color = net_types)) +
    geom_line() +
    labs(x = "Time (years)", y = ifelse(data_type == "prevalence", "prevalence", "Prevalence"), 
         title = paste(iso, "Comparison:", data_type, "-", environment_label)) +
    theme_minimal(base_size = 15) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(),
          panel.background = element_rect(fill = "white", colour = "white"),
          plot.background = element_rect(fill = "white", colour = NA)) +
    scale_color_manual(values = c("Current" = "blue", "Counterfactual" = "red"))
  
  # Construct the filename
  plot_filename <- paste0(plot_dir, iso, "_", environment_label, "_comparison_plot_", data_type, ".png")
  
  # Save the plot with a white background
  ggsave(filename = plot_filename, plot = plot, background = "white", width = 10, height = 8)
}

# Data type for labeling and plotting
data_type <- "prevalence" # Adjust as necessary

# Call the plotting function with the loaded data
create_comparison_plot(current_data, counterfactual_data, iso, environment_label, data_type)