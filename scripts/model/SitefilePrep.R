library(dplyr)
library(readr)
library(foresite) # Assuming foresite is a package you have access to
library(readr)

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
# # Initialize an empty data frame to store updated site data

# updated_site_data <- data.frame()

# site_data <- foresite::get_site(site_data$country)
# # Determine the year range
# min_year <- min(ssa_region$year, na.rm = TRUE)
# max_year <- max(site_data$interventions$year, na.rm = TRUE)

# # Iterate through each year within the range

# for(year_edit in min_year:max_year) {
#   ssa_fact_year <- ssa_region %>%
#     filter(ISO3C == iso3c, year == year_edit)
    
#   # Perform calculations only if ssa_fact_year is not empty
#   if(nrow(ssa_fact_year) > 0) {
#     site_data_res <- site_data$interventions %>%
#       select(year, dn0, rn0, gamman, pyrethroid_resistance) %>%
#       filter(year == year_edit)
    
#     if(nrow(site_data_res) > 0) {
#       for(idx in 1:nrow(site_data_res)) {
#         # Calculate dn0, rn0, gamman for the current year and resistance type
#         for(net_type in c("PyNets", "PyPBONets", "PyPyroNets")) {
#           net_factors <- combined_nets %>%
#             filter(resistance == site_data_res$pyrethroid_resistance[idx], NetType == net_type) %>%
#             summarise(
#               dn0_med = sum(dn0_med * ssa_fact_year[[net_type]], na.rm = TRUE),
#               rn0_med = sum(rn0_med * ssa_fact_year[[net_type]], na.rm = TRUE),
#               gamman_med = sum(gamman_med * ssa_fact_year[[net_type]], na.rm = TRUE)
#             )
#           site_data_res$dn0[idx] <- site_data_res$dn0[idx] + net_factors$dn0_med
#           site_data_res$rn0[idx] <- site_data_res$rn0[idx] + net_factors$rn0_med
#           site_data_res$gamman[idx] <- site_data_res$gamman[idx] + net_factors$gamman_med
#         }
#       }
      
#       # Update the original site_data with the new values
#       for(idx in 1:nrow(site_data_res)) {
#         site_data$interventions <- site_data$interventions %>%
#           mutate(
#             dn0 = ifelse(year == year_edit & pyrethroid_resistance == site_data_res$pyrethroid_resistance[idx], site_data_res$dn0[idx], dn0),
#             rn0 = ifelse(year == year_edit & pyrethroid_resistance == site_data_res$pyrethroid_resistance[idx], site_data_res$rn0[idx], rn0),
#             gamman = ifelse(year == year_edit & pyrethroid_resistance == site_data_res$pyrethroid_resistance[idx], site_data_res$gamman[idx], gamman)
#           )
#       }
#     }
#   }
# }

# # Optionally, append the updated site_data for this ISO3C to a larger dataframe
# site_data$interventions <- rbind(updated_site_data, site_data$interventions)

# return(site_data)
# }