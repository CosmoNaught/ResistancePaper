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
library(readr)

combined_nets <- read_csv(paste0(getwd(),"/data/post/combined_nets.csv"))
combined_nets <- select(combined_nets, "dn0_med", "rn0_med", "gamman_med", "resistance", "NetType")

# Adjusted Mode Settings Function
get_mode_settings <- function(mode) {
  settings <- list(
    "observed" = list(counterfactual = FALSE, ssa_region_file = "SSA_region_combined.csv"),
    "PyOnly" = list(counterfactual = FALSE, ssa_region_file = "SSA_region_PyNets.csv"),
    "PyPBO" = list(counterfactual = FALSE, ssa_region_file = "SSA_region_Py_PBONets.csv"),
    "IG2Only" = list(counterfactual = FALSE, ssa_region_file = "SSA_region_IG2.csv"),
    "counterfactual" = list(counterfactual = TRUE, ssa_region_file = "SSA_region_combined.csv")
  )
  if (!mode %in% names(settings)) stop("Invalid mode specified")
  return(settings[[mode]])
}

# Load SSA Region based on Mode
load_ssa_region <- function(ssa_region_file) {
  path <- paste0("D:/Malaria/ResistancePaper/data/post/", ssa_region_file)
  read.csv(path)
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
    overrides = list(human_population = human_population,
    prevalence_rendering_min_ages = c(0,  0,  2) * 365,
    prevalence_rendering_max_ages = c(5, 100, 10) * 365,
    clinical_incidence_rendering_min_ages = c(0, 0) * 365,
    clinical_incidence_rendering_max_ages = c(5, 100) * 365,
    incidence_rendering_min_ages = c(0,  0) * 365,
    incidence_rendering_max_ages = c(5, 100) * 365,
    severe_incidence_rendering_min_ages = c(0,  0) * 365,
    severe_incidence_rendering_max_ages = c(5, 100) * 365
    )
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
  # Min year in SSA
  min_year <- min(ssa_region$year, na.rm = TRUE)
  
  # Max year in site file
  max_year <- max(site_data$interventions$year, na.rm = TRUE)
  
  for (year_edit in min_year:max_year) {
    # Filter SSA data for the current year and country
    ssa_fact <- ssa_region %>%
      filter(ISO3C == site_data$country, year == year_edit)
    
    # Filter site data for the current year and rural areas
    row_edit <- site_data$interventions %>%
      filter(year == year_edit, urban_rural == "rural") %>%
      select(urban_rural, year, dn0, rn0, gamman, pyrethroid_resistance)
    
    if (nrow(row_edit) > 0) {
      row_edit <- row_edit %>%
        rowwise() %>%
        mutate(
          dn0 = sum(
            (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyNets"))$dn0_med * ssa_fact$PyNets,
            (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyPBONets"))$dn0_med * ssa_fact$PyPBONets,
            (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyPyroNets"))$dn0_med * ssa_fact$PyPyroNets
          ),
          rn0 = sum(
            (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyNets"))$rn0_med * ssa_fact$PyNets,
            (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyPBONets"))$rn0_med * ssa_fact$PyPBONets,
            (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyPyroNets"))$rn0_med * ssa_fact$PyPyroNets
          ),
          gamman = sum(
            (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyNets"))$gamman_med * ssa_fact$PyNets,
            (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyPBONets"))$gamman_med * ssa_fact$PyPBONets,
            (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyPyroNets"))$gamman_med * ssa_fact$PyPyroNets
          )
        ) %>%
        ungroup()
      
      # Update the original site_data with the modified values
      for (i in 1:nrow(row_edit)) {
        site_data$interventions <- site_data$interventions %>%
          mutate(
            dn0 = ifelse(year == year_edit & urban_rural == "rural", row_edit$dn0[i], dn0),
            rn0 = ifelse(year == year_edit & urban_rural == "rural", row_edit$rn0[i], rn0),
            gamman = ifelse(year == year_edit & urban_rural == "rural", row_edit$gamman[i], gamman)
          )
      }
    }
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
update_interventions <- function(site_data, combined_nets, ssa_region, output_dir, counterfactual, mode) {
  site_data <- SiteFilePrep(site_data, combined_nets, ssa_region)
  site_data$interventions <- counterfactual_replacement(site_data$interventions, mode_settings$counterfactual)

  ## Malawi has non-zero values in rtss_cov which leads to the model not running
  site_data$interventions$rtss_cov <- 0 # Zero out all values in rtss_cov

    # Save interventions data
  save_interventions_data(site_data, output_dir, mode)

  return(site_data)

}


save_interventions_data <- function(site_data, output_dir, mode) {
  # Extract ISO code from site_data
  iso_code <- site_data$sites$iso3c[1]

  # Construct the path for saving the RDS file in the sitefile directory
  raw_sitefile_folder_path <- paste0("outputs/raw/sitefile/", output_dir, "/", mode,"/",iso_code, "/")
  create_directory(raw_sitefile_folder_path)

  # File name for the RDS file
  rds_file_name <- paste0(raw_sitefile_folder_path, "site_data_interventions_", iso_code, ".RDS")

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

# Model Execution Functions
run_model_for_country <- function(iso, folder_base, net_data, mode_settings) {
    site_data <- foresite:::get_site(iso)
    
    site_data <- update_interventions(site_data, combined_nets, ssa_region, output_dir, mode_settings$counterfactual, mode)
    
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
execute_models <- function() {  # Add mode as a parameter
    mode_settings <- get_mode_settings(mode)
    ssa_region <- load_ssa_region(mode_settings$ssa_region_file)

    sim_base_path <- paste0(getwd(), "/outputs/raw/sim/", output_dir, "/", mode, "/")
    create_directory(sim_base_path)
    invisible(lapply(iso_codes, function(iso) {
        sim_iso_path <- paste0(sim_base_path, iso, "/")
        create_directory(sim_iso_path)
        run_model_for_country(iso, sim_iso_path, ssa_region, mode_settings)
    }))
}

# Configuration and Constants - Adjusted
debug <- FALSE
parallel <- TRUE

# Adjust workers, output directory, and human population based on mode or debug flag
workers <- if(parallel) 22 else 1
output_dir <- if(debug) "debug" else "final"
human_population <- if(debug) 10000 else 100000

iso_codes <- "MWI" #unique(read.csv("D:/Malaria/ResistancePaper/data/post/SSA_region_combined.csv")$ISO3C)
modes <- c("observed", "PyOnly", "PyPBO", "IG2Only", "counterfactual")

initialize_environment()

for(mode in modes) {
    cat("Executing mode:", mode, "\n")
    
    # Get mode settings and SSA region data
    mode_settings <- get_mode_settings(mode)
    ssa_region <- load_ssa_region(mode_settings$ssa_region_file)
    
    execute_models()  # Assumes this function now internally uses the 'mode' variable correctly
    rm(mode, mode_settings, ssa_region)

}