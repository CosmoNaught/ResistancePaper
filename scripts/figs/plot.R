# Load Libraries
library(ggplot2)
library(dplyr)
library(viridis) # For viridis color palette

# Configuration and Constants
debug <- TRUE
isos <- c("MLI") # List of ISO codes for which to generate plots
environment_label <- ifelse(debug, "debug", "final")
measure_type <- "prevalence"
plot_base_dir <- paste0(getwd(), "/outputs/figs/") # Adjust your base directory accordingly
modes <- c("observed", "PyOnly", "PyPBO", "counterfactual")


# Ensure the base plot directory exists
if (!dir.exists(plot_base_dir)) {
  dir.create(plot_base_dir, recursive = TRUE)
}

# Function to create and save comparison plots
create_and_save_plots <- function(iso, modes, environment_label, measure_type) {
  plot_dir <- paste0(plot_base_dir, environment_label, "/", iso, "/")
  
  # Ensure the plot directory for each ISO exists
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }
  
  all_data <- list()
  
  # Load the RDS files for each mode and store in a list
  for (mode in modes) {
    file_path <- paste0(getwd(), "/outputs/post/", environment_label, "/", mode, "/", iso, "/post_model_output_", iso, "_", mode, "_", measure_type, ".RDS")
    if (file.exists(file_path)) {
      data <- readRDS(file_path)
      data$mode <- mode # Add a column to distinguish modes
      all_data[[mode]] <- data
    }
  }
  
  combined_data <- bind_rows(all_data)
  
  # Create the plot
  plot <- ggplot(combined_data, aes(x = timestep, y = value, color = mode)) +
    geom_line() +
    labs(x = "Year", y = "Prevalence", title = paste("Prevalence Comparison -", iso)) +
    theme_minimal(base_size = 15) +
    theme(panel.background = element_rect(fill = "white", colour = "white"),
          plot.background = element_rect(fill = "white", colour = NA)) +
    scale_color_viridis(discrete = TRUE, option = "D") + # Using viridis for color scale
    guides(color = guide_legend(title = "Mode"))
  
  # Save the plot with a white background
  plot_filename <- paste0(plot_dir, iso, "_", environment_label, "_comparison_", measure_type, ".png")
  ggsave(plot_filename, plot, background = "white", width = 10, height = 8)
}

# Generate and save plots for each ISO code
for (iso in isos) {
  create_and_save_plots(iso, modes, environment_label, measure_type)
}
