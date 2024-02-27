# tst1 <- readRDS("D:\\Malaria\\ResistancePaper\\outputs\\raw\\sitefile\\debug\\observed\\MLI\\site_data_interventions_MLI.RDS")
# tst2 <- readRDS("D:\\Malaria\\ResistancePaper\\outputs\\raw\\sitefile\\debug\\PyOnly\\MLI\\site_data_interventions_MLI.RDS")

# # # tst1$dn0 - tst2$dn0

# library(dplyr)
# library(readr)

# # Coutnrye
# iso3c <- "MLI"

# # Load combined_nets.csv
# combined_nets <- read_csv(paste0(getwd(),"/data/post/combined_nets.csv"))
# combined_nets <- select(combined_nets, "dn0_med", "rn0_med", "gamman_med", "resistance", "NetType")

# ssa_region <- read_csv(paste0(getwd(),"/data/post/SSA_region_combined.csv"))

# site_data <- foresite::get_site(iso3c = iso3c)

# SiteFilePrep <- function(site_data, combined_nets, ssa_region) {
#   # Min year in SSA
#   min_year <- min(ssa_region$year, na.rm = TRUE)
  
#   # Max year in site file
#   max_year <- max(site_data$interventions$year, na.rm = TRUE)
  
#   for (year_edit in min_year:max_year) {
#     # Filter SSA data for the current year and country
#     ssa_fact <- ssa_region %>%
#       filter(ISO3C == iso3c, year == year_edit)
    
#     # Filter site data for the current year and rural areas
#     row_edit <- site_data$interventions %>%
#       filter(year == year_edit, urban_rural == "rural") %>%
#       select(urban_rural, year, dn0, rn0, gamman, pyrethroid_resistance)
    
#     if (nrow(row_edit) > 0) {
#       row_edit <- row_edit %>%
#         rowwise() %>%
#         mutate(
#           dn0 = sum(
#             (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyNets"))$dn0_med * ssa_fact$PyNets,
#             (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyPBONets"))$dn0_med * ssa_fact$PyPBONets,
#             (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyPyroNets"))$dn0_med * ssa_fact$PyPyroNets
#           ),
#           rn0 = sum(
#             (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyNets"))$rn0_med * ssa_fact$PyNets,
#             (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyPBONets"))$rn0_med * ssa_fact$PyPBONets,
#             (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyPyroNets"))$rn0_med * ssa_fact$PyPyroNets
#           ),
#           gamman = sum(
#             (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyNets"))$gamman_med * ssa_fact$PyNets,
#             (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyPBONets"))$gamman_med * ssa_fact$PyPBONets,
#             (combined_nets %>% filter(resistance == pyrethroid_resistance, NetType == "PyPyroNets"))$gamman_med * ssa_fact$PyPyroNets
#           )
#         ) %>%
#         ungroup()
      
#       # Update the original site_data with the modified values
#       for (i in 1:nrow(row_edit)) {
#         site_data$interventions <- site_data$interventions %>%
#           mutate(
#             dn0 = ifelse(year == year_edit & urban_rural == "rural", row_edit$dn0[i], dn0),
#             rn0 = ifelse(year == year_edit & urban_rural == "rural", row_edit$rn0[i], rn0),
#             gamman = ifelse(year == year_edit & urban_rural == "rural", row_edit$gamman[i], gamman)
#           )
#       }
#     }
#   }
  
#   return(site_data)
# }

# # Assuming site_data, combined_nets, and ssa_region are already loaded
# updated_site_data <- SiteFilePrep(site_data, combined_nets, ssa_region)


tst1 <- readRDS("D:\\Malaria\\ResistancePaper\\outputs\\raw\\sitefile\\debug\\observed\\MLI\\site_data_interventions_MLI.RDS")
tst2 <- readRDS("D:\\Malaria\\ResistancePaper\\outputs\\raw\\sitefile\\debug\\PyOnly\\MLI\\site_data_interventions_MLI.RDS")

tst1$rn0 - tst2$rn0

# Function to check and print ISO codes with non-zero sum of rtss_cov
check_non_zero_rtss_cov <- function(iso_codes) {
  non_zero_iso_codes <- c() # Initialize an empty vector to store ISO codes with non-zero sums
  
  for (iso in iso_codes) {
    sum_rtss_cov <- sum(foresite::get_site(iso)$interventions$rtss_cov)
    
    if (sum_rtss_cov > 0) {
      non_zero_iso_codes <- c(non_zero_iso_codes, iso) # Add ISO code to the list
    }
  }
  
  # Print ISO codes with non-zero sum of rtss_cov
  if (length(non_zero_iso_codes) > 0) {
    cat("ISO codes with non-zero sum of rtss_cov:", paste(non_zero_iso_codes, collapse = ", "), "\n")
  } else {
    cat("No ISO codes with non-zero sum of rtss_cov found.\n")
  }
}

# Execute the function with the provided ISO codes
check_non_zero_rtss_cov(iso_codes)