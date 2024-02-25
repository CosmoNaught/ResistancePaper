# Load Libraries
library(foresite)
library(site)
library(malariasimulation)
library(data.table)
library(dplyr)
library(purrr)
library(remotes)
library(drat)
library(furrr)
library(dplyr)
library(readr)
library(foresite) # Assuming foresite is a package you have access to
library(readr)

combined_nets <- read.csv("D:/Malaria/ResistancePaper/data/post/combined_nets.csv") %>%
  select(dn0_med, rn0_med, gamman_med, resistance, NetType)

ssa_region <- read.csv("D:/Malaria/ResistancePaper/data/post/SSA_region_combined.csv")

# Mode Settings Function
get_mode_settings <- function(mode) {
  switch(mode,
    "observed" = list(counterfactual = FALSE),
    "PyOnly" = list(counterfactual = FALSE),
    "PyPBO" = list(counterfactual = FALSE),
    "counterfactual" = list(counterfactual = TRUE),
    stop("Invalid mode specified")
  )
}

# Function to prepare input data for a single site
prep_single_site_data <- function(site_data, site_index) {
  site <- site::single_site(site_data, index = site_index)

  # Retrieve site information
  site_name <- site$sites$name_1
  ur <- site$sites$urban_rural
  iso <- site$sites$iso3c

  # Skip urban sites
  if (ur == "urban") {
    message(paste0("Skipping urban site: ", site_name))
    return(NULL)
  }

  # Check EIR value and skip site if EIR <= 0
  if (site$eir$eir[1] <= 0) {
    message(paste0(site_name, " has EIR <= 0. Skipping."))
    return(NULL)
  }

  message(paste0("Prepping inputs for site ", site_name, " ", ur))
  print(site$eir)

  # Retrieve parameters for the site
  params <- site::site_parameters(
    interventions = site$interventions,
    demography = site$demography,
    vectors = site$vectors,
    seasonality = site$seasonality,
    eir = site$eir$eir[1],
    overrides = list(human_population = 10000,
    prevalence_rendering_min_ages = c(0, 5,  0,  2) * 365, ## Prev in 6 months to 14 years measured
    prevalence_rendering_max_ages = c(5,15,100, 10) * 365, 
    clinical_incidence_rendering_min_ages = c(0, 5, 0,  2) * 365, ## All age clin_inc
    clinical_incidence_rendering_max_ages = c(5,15,100, 10) * 365,
    incidence_rendering_min_ages = c(0, 5,  0,  2) * 365, ## All age clin_inc
    incidence_rendering_max_ages = c(5,15,100, 10) * 365,
    severe_incidence_rendering_min_ages = c(0, 5,  0,  2) * 365,
    severe_incidence_rendering_max_ages = c(5,15,100, 10) * 365)
  )

  inputs <- list(
    "param_list" = params,
    "site_name" = site_name,
    "ur" = ur,
    "iso" = iso
  )

  return(inputs)
}

prep_all_site_data <- function(site_data) {
  jobs <- nrow(site_data$sites)
  # Filter for rural sites
  rural_sites <- site_data$sites[site_data$sites$urban_rural == "rural", ]
  message(paste0("Prepping ", nrow(rural_sites), " jobs for model launch"))

  # Prepare data for each site
  output <- lapply(seq_len(jobs), function(num) prep_single_site_data(site_data, num))
  
  # Remove NULL entries (skipped sites)
  output <- output[!sapply(output, is.null)]

  if (length(output) == 0) {
    message("All sites skipped. Exiting function.")
    return(NULL)
  } else {
    return(output)
  }
}

# Main function to prepare inputs
prep_inputs <- function(site_data) {
  prep_all_site_data(site_data)
}

# Function to run the malaria model simulation
run_simulation <- function(output) {
  malariasimulation::run_simulation(
    timesteps = output$param_list$timesteps,
    parameters = output$param_list
  )
}

# Function to augment model data with site information
augment_model_data <- function(model, output) {
  model <- data.table(model)
  model[, site_name := output$site_name]
  model[, urban_rural := output$ur]
  model[, iso := output$iso]
  return(model)
}

# Function to save the model output to a file
save_model_output <- function(model, output, folder) {
  file_path <- paste0(folder, "pre_model_output_", output$site_name, "_", output$ur, ".RDS")
  saveRDS(model, file = file_path)
  message("Model saved: ", file_path)
}

# Main function to run the malaria model and handle output
run_malaria_model <- function(output, folder) {
  start_time <- Sys.time()

  message("Running the model...")
  model <- run_simulation(output)
  model <- augment_model_data(model, output)
  save_model_output(model, output, folder)

  end_time <- Sys.time()
  time_taken <- end_time - start_time
  message("Time taken: ", time_taken)
}

SiteFilePrep <- function(site_data, combined_nets, ssa_region) {
    ssa_fact <- ssa_region %>%
      filter(ISO3C == site_data$country)
    
    if (!is.null(site_data$interventions)) {
        min_year <- min(ssa_fact$year, na.rm = TRUE)
        max_year <- max(site_data$interventions$year, na.rm = TRUE)
      
        # Use lapply instead of for loop for years
        lapply(min_year:max_year, function(year_edit) {
            ssa_fact_year <- filter(ssa_fact, year == year_edit)
            
            if(nrow(ssa_fact_year) > 0) {
                interventions_res <- site_data$interventions %>%
                    filter(year == year_edit)
                
                if(nrow(interventions_res) > 0) {
                    row_indices <- which(site_data$interventions$year == year_edit)
                    
                    lapply(row_indices, function(idx) {
                        # Capture original data types
                        original_dn0_type <- typeof(site_data$interventions$dn0[idx])
                        original_rn0_type <- typeof(site_data$interventions$rn0[idx])
                        original_gamman_type <- typeof(site_data$interventions$gamman[idx])
                        
                        lapply(c("PyNets", "PyPBONets", "PyPyroNets"), function(net_type) {
                            combined_factors <- combined_nets %>%
                                filter(resistance == site_data$interventions$pyrethroid_resistance[idx], NetType == net_type)
                            
                            if(nrow(combined_factors) > 0) {
                                ssa_net_factor <- ssa_fact_year[[net_type]]
                                
                                # Update values while ensuring data types remain unchanged
                                site_data$interventions$dn0[idx] <- as(
                                    site_data$interventions$dn0[idx] +
                                    combined_factors$dn0_med * ssa_net_factor, original_dn0_type)
                                
                                site_data$interventions$rn0[idx] <- as(
                                    site_data$interventions$rn0[idx] +
                                    combined_factors$rn0_med * ssa_net_factor, original_rn0_type)
                                
                                site_data$interventions$gamman[idx] <- as(
                                    site_data$interventions$gamman[idx] +
                                    combined_factors$gamman_med * ssa_net_factor, original_gamman_type)
                            }
                        })
                    })
                }
            }
        })
    }
    return(site_data)
}

counterfactual_replacement <- function(interventions, counterfactual) {
  if (!counterfactual) {
    return(interventions)
  }

  interventions %>%
    dplyr::mutate(
      dn0 = ifelse(counterfactual, 0.541979954, dn0),
      rn0 = ifelse(counterfactual, 0.456350279, rn0),
      gamman = ifelse(counterfactual, 2.64, gamman)
    )
}

# Main Function
update_interventions <- function(site_data, combined_nets, ssa_region, output_dir, counterfactual) {
  site_data <- SiteFilePrep(site_data, combined_nets, ssa_region)
  site_data$interventions <- counterfactual_replacement(site_data$interventions, mode_settings$counterfactual)

    # Save interventions data
  save_interventions_data(site_data, output_dir, mode_settings$mode)

  return(site_data)

}


save_interventions_data <- function(site_data, output_dir, mode) {
  # Extract ISO code from site_data
  iso_code <- site_data$sites$iso3c[1]

  # Construct the path for saving the RDS file in the sitefile directory
  raw_sitefile_folder_path <- paste0("outputs/raw/sitefile/", output_dir, mode,"/",iso_code, "/")
  create_directory(raw_sitefile_folder_path)

  # File name for the RDS file
  rds_file_name <- paste0(raw_sitefile_folder_path, "site_data_interventions_", iso_code, ".RDS")

  # Save the RDS file
  saveRDS(site_data$interventions, file = rds_file_name)
  message("Interventions data saved: ", rds_file_name)
}

# Initialize Environment
initialize_environment <- function() {
    if (parallel) {
        plan(multisession, workers = workers)
    }
}

# Utility Functions
create_directory <- function(path) {
    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
}

read_net_data <- function() {
    base_path <- paste0(getwd(), "/data/raw/")
    net_types <- setNames(lapply(paste0(base_path, net_files), read.csv), net_names)
    return(net_types)
}


# Model Execution Functions
run_model_for_country <- function(iso, folder_base, net_data, mode_settings) {
    site_data <- foresite:::get_site(iso)
    
    site_data <- update_interventions(site_data, combined_nets, ssa_region, output_dir, mode_settings$counterfactual)
    
    output <- prep_inputs(site_data)
    
    if (parallel) {
        future_map(output, ~run_malaria_model(.x, folder_base))
    } else {
        lapply(output, function(x) {
            run_malaria_model(x, folder_base)
        })
    }
}

# Main Execution Logic
execute_models <- function() {
    net_types <- read_net_data()

    # Adjust net types based on the mode
    net_names_to_use <- if (mode_settings$counterfactual) {
        # Use only PyNets for counterfactual and don't create a sub-folder for net types
        names(net_types)[names(net_types) == "PyNets"]
    } else {
        # Use all net types otherwise and create sub-folders for each
        names(net_types)
    }

    invisible(lapply(net_names_to_use, function(net_name) {
        net_data <- net_types[[net_name]]

        # Adjust folder structure based on counterfactual setting
        folder_net_type <- if (mode_settings$counterfactual) folder_base else paste0(folder_base, net_name, "/")
        create_directory(folder_net_type)

        invisible(lapply(iso_codes, function(iso) {
            folder_iso <- paste0(folder_net_type, iso, "/")
            create_directory(folder_iso)

            run_model_for_country(iso, folder_iso, net_data, mode_settings)
        }))
    }))
}

# Configuration and Constants
debug <- TRUE
parallel <- TRUE
mode <- "counterfactual" # Set mode to "observed", "delay", or "counterfactual"

# Apply mode-specific settings
mode_settings <- get_mode_settings(mode)

# Parallel and Debug Settings
workers <- if(parallel) 22 else 1
output_dir <- ifelse(debug, "debug", "final")
human_population <- if(debug) 1500 else 150000
iso_codes <- c("MLI") # Add additional ISO codes as required

# File and Folder Paths
net_files <- c("pyrethroid_only_nets.csv", "pyrethroid_pyrrole_nets.csv", "pyrethroid_pbo_nets.csv")
net_names <- c("PyNets")
folder_base <- paste0(getwd(), "/outputs/raw/sim/", output_dir, "/", mode, "/")

# Script Execution
initialize_environment()
execute_models()