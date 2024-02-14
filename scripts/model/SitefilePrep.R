library(dplyr)
library(readr)
library(foresite) # Assuming foresite is a package you have access to
library(readr)

combined_nets <- read.csv("D:/Malaria/ResistancePaper/data/post/combined_nets.csv") %>%
  select(dn0_med, rn0_med, gamman_med, resistance, NetType)

ssa_region <- read.csv("D:/Malaria/ResistancePaper/data/post/SSA_region.csv")

SiteFilePrep <- function(site_data, combined_nets, ssa_region) {
# Initialize an empty data frame to store updated site data
browser()
updated_site_data <- data.frame()

# Main loop through each ISO3C code
for(iso3c in iso_codes) {
  site_data <- foresite::get_site(iso3c = iso3c)
  # Determine the year range
  min_year <- min(ssa_region$year, na.rm = TRUE)
  max_year <- max(site_data$interventions$year, na.rm = TRUE)
  
  # Iterate through each year within the range
  
  for(year_edit in min_year:max_year) {
    ssa_fact_year <- ssa_region %>%
      filter(ISO3C == iso3c, year == year_edit)
      
    # Perform calculations only if ssa_fact_year is not empty
    if(nrow(ssa_fact_year) > 0) {
      site_data_res <- site_data$interventions %>%
        select(year, dn0, rn0, gamman, pyrethroid_resistance) %>%
        filter(year == year_edit)
      
      if(nrow(site_data_res) > 0) {
        for(idx in 1:nrow(site_data_res)) {
          # Calculate dn0, rn0, gamman for the current year and resistance type
          for(net_type in c("PyNets", "PyPBONets", "PyPyroNets")) {
            net_factors <- combined_nets %>%
              filter(resistance == site_data_res$pyrethroid_resistance[idx], NetType == net_type) %>%
              summarise(
                dn0_med = sum(dn0_med * ssa_fact_year[[net_type]], na.rm = TRUE),
                rn0_med = sum(rn0_med * ssa_fact_year[[net_type]], na.rm = TRUE),
                gamman_med = sum(gamman_med * ssa_fact_year[[net_type]], na.rm = TRUE)
              )
            site_data_res$dn0[idx] <- site_data_res$dn0[idx] + net_factors$dn0_med
            site_data_res$rn0[idx] <- site_data_res$rn0[idx] + net_factors$rn0_med
            site_data_res$gamman[idx] <- site_data_res$gamman[idx] + net_factors$gamman_med
          }
        }
        
        # Update the original site_data with the new values
        for(idx in 1:nrow(site_data_res)) {
          site_data$interventions <- site_data$interventions %>%
            mutate(
              dn0 = ifelse(year == year_edit & pyrethroid_resistance == site_data_res$pyrethroid_resistance[idx], site_data_res$dn0[idx], dn0),
              rn0 = ifelse(year == year_edit & pyrethroid_resistance == site_data_res$pyrethroid_resistance[idx], site_data_res$rn0[idx], rn0),
              gamman = ifelse(year == year_edit & pyrethroid_resistance == site_data_res$pyrethroid_resistance[idx], site_data_res$gamman[idx], gamman)
            )
        }
      }
    }
  }
  
  # Optionally, append the updated site_data for this ISO3C to a larger dataframe
  site_data$interventions <- rbind(updated_site_data, site_data$interventions)
}
}