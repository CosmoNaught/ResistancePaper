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
  browser()
  return(site_data)

}