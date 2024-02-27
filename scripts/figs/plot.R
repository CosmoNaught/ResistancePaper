# Load Libraries
library(ggplot2)
library(dplyr)
library(viridis) # For viridis color palette

# Configuration and Constants
debug <- FALSE
isos <- c("MWI") # List of ISO codes for which to generate plots
environment_label <- ifelse(debug, "debug", "final")
measure_type <- "prevalence"
plot_base_dir <- paste0(getwd(), "/outputs/figs/") # Adjust your base directory accordingly
modes <- c("observed", "PyOnly", "PyPBO", "IG2Only", "counterfactual")

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
# Create the plot with legend label customizations
plot <- ggplot(combined_data, aes(x = timestep, y = value, color = mode)) +
  geom_line(linewidth=0.5) + # Use linewidth for line thickness
  labs(x = "Year", y = "Prevalence (%)", title = paste("Malaria Prevalence Comparison in Malawi (PrPf 2-10)"),
       subtitle = "Comparison across different modelling scenarios") +
  theme_minimal(base_size = 20) + # Increase base font size for other elements
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = NA),
        legend.title = element_text(size=16), # Customize legend title
        legend.text = element_text(size=14), # Customize legend text
        axis.title = element_text(size=18), # Customize axis titles
        axis.text = element_text(size=14), # Customize axis text
        plot.title = element_text(size=20, face="bold"), # Customize plot title
        plot.subtitle = element_text(size=18), # Customize plot subtitle
        legend.position = c(0.8, 0.8), # Place legend inside plot
        axis.text.x = element_text(angle = 45, hjust = 1, size=10)) + # Adjust x-axis labels, with smaller size
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1,
                      breaks = c("counterfactual", "observed", "PyOnly", "PyPBO", "IG2Only"),
                      labels = c("No ITN Resistance", "Observed ITN Distribution", "Pyrethroid ITN Distribution", "Pyrethroid & PBO ITN Distribution", "IG2 ITN Distribution")) + # Custom legend labels
  guides(color = guide_legend(title = "Scenario")) + # Rename legend title to 'Scenario'
  scale_x_continuous(limits = c(2018, 2023), breaks = seq(2018, 2023, by = 1), expand = c(0, 0)) + # Set x-axis limits and ensure ticks for every year from 2000 to 2023, remove padding
  scale_y_continuous(limits = c(0, 0.2), expand = c(0, 0)) # Set y-axis limits from 0 to 1, remove padding

# Note: Custom legend labels are specified to match the provided descriptions


    
  # Save the plot with a white background
  plot_filename <- paste0(plot_dir, iso, "_", environment_label, "_comparison_", measure_type, ".png")
  ggsave(plot_filename, plot, background = "white", width = 10, height = 8)
}

# Generate and save plots for each ISO code
for (iso in isos) {
  create_and_save_plots(iso, modes, environment_label, measure_type)
}
