#' @title JSON output
#'
#' @description It generates a JSON file of all CLEANED model computation.
#'
#' @param feed_basket_quality A dataframe computed using the `feed_quality` function
#'
#' @param soil_erosion A dataframe computed using the `soil_health` function
#'
#' @param land_required A dataframe computed using the `land_requirement` function
#'
#' @param energy_required A list computed using the `energy_required` function
#'
#' @param water_required A dataframe computed using the `water_required` function
#'
#' @param nitrogen_balance A dataframe computed using the `n_balance` function
#'
#' @param livestock_productivity A dataframe computed using the `land_productivity` function
#'
#' @param biomass A dataframe computed using the `biomass_calculation` function
#'
#' @param soil_carbon A dataframe computed using the `soil_organic_carbon` function
#'
#' @param ghg_emission A dataframe computed using the `n_balance` ghg_emission
#'
#' @return saved JSON file
#'
#' @importFrom jsonlite toJSON
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' data(ghg_para)
#' feed_basket_quality <- feed_quality(mufindi)
#' energy_required <- energy_requirement(mufindi,feed_basket_quality)
#' land_required <- land_requirement(feed_basket_quality, energy_required, mufindi)
#' soil_erosion <- soil_health(mufindi, land_required)
#' water_required <- water_requirement(mufindi,land_required)
#' nitrogen_balance <- n_balance(mufindi, land_required, soil_erosion)
#' livestock_productivity <- land_productivity(mufindi)
#' biomass <- biomass_calculation(mufindi, land_required)
#' soil_carbon <- soil_organic_carbon(para, land_required, biomass)
#' ghg_emission <- ghg_emission(mufindi,energy_required,ghg_para,land_required,nitrogen_balance)
#' combineOutputs(feed_basket_quality,energy_required,land_required,soil_erosion,water_required,
#' nitrogen_balance,livestock_productivity,biomass,soil_carbon,ghg_emission)
#' }
#'
#' @export
#'
combineOutputs <- function(feed_basket_quality, energy_required, land_required,
                                  soil_erosion, water_required, nitrogen_balance,
                                  livestock_productivity, biomass,soil_carbon, ghg_emission, filePath){

  land_required <- land_required[["land_requirements_all"]]


  if (exists("feed_basket_quality")) {
    feed_basket_quality = split(feed_basket_quality, f=feed_basket_quality$season_name)
  }else {feed_basket_quality = "ERROR: Feed quality was not computed"}

  if (exists("energy_required")) {
    energy_required = energy_required
  }else {energy_required = "ERROR: Energy requirement was not computed"}

  if (exists("land_required")) {
    land_required = land_required
  }else {land_required = "ERROR: Land requirement was not computed"}

  if (exists("soil_erosion")) {
    soil_erosion = soil_erosion
  }else {soil_erosion = "ERROR: Soil erosion was not computed"}

  if (exists("water_required")) {
    water_required = water_required
  }else {water_required = "ERROR: Water requirement was not computed"}

  if (exists("nitrogen_balance")) {
    nitrogen_balance = nitrogen_balance
  }else {nitrogen_balance = "ERROR: Nitrogen balance was not computed"}

  if (exists("livestock_productivity")) {
    livestock_productivity = livestock_productivity
  }else {livestock_productivity = "ERROR: Livestock productivity was not computed"}

  if (exists("biomass")) {
    biomass = biomass
  }else {biomass = "ERROR: Biomass was not computed"}

  if (exists("biomass")) {
    soil_carbon = soil_carbon
  }else {soil_carbon = "ERROR: Soil carbon was not computed"}

  if (exists("ghg_emission")) {
    ghg_emissions = ghg_emissions
  }else {ghg_emissions = "ERROR: Greenhouse gas emissions were not computed"}

  if (exists("filePath")) {
    filePath = filePath
    # Extract the directory path
    directoryPath <- dirname(filePath)
  }else {filePath = "ERROR: File path is not provided"}


  feed_basket_quality <- lapply(feed_basket_quality, function(x) {x <- x[,-1]})
  ###############################################################################################
  ## Land required
  ###############################################################################################

  # Plotting land requirement
  land_required %>%
    group_by(feed, season_name) %>%
    summarise(area_feed_total = sum(area_feed, na.rm = T)) %>%
    ggplot2::ggplot(aes(x=feed, y=area_feed_total, fill=season_name))+
    geom_bar(stat = "identity", width = 0.6)+
    labs(x = "Feed Item", y = "Area (Ha)", fill = "Seasons", title = "Land Requirement and Feed Basket") +
    geom_text(aes(label = round(area_feed_total, 2)), vjust = -0.5, size = 3, angle = 45) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(paste0(directoryPath, "/land_required.png"), width = 150, height = 100, units = "mm")


  # Expanded table land requirement
  ex_land_required <- land_required %>%
    group_by(feed) %>%
    summarise(total_area = sum(area_feed, na.rm = T),
              farm = sum(farm, na.rm = T),
              rough_of = sum(rough_of, na.rm = T),
              conc_of = sum(conc_of, na.rm = T),
              conc_ip = sum(conc_ip, na.rm = T),
              grasses = sum(grasses, na.rm = T),
              tree_legume = sum(tree_legume, na.rm = T))

  # Seasonal land required
  seasonal_land_required <- land_required %>%
    group_by(feed, season_name) %>%
    summarise(area_feed_total = sum(area_feed, na.rm = T)) %>%
    pivot_wider(names_from = season_name, values_from = area_feed_total)

  # Expanded table land requirement
  ex_dm_required <- land_required %>%
    group_by(feed) %>%
    summarise(total_dm = sum(feed_item_dm, na.rm = T),
              farm_dm = sum(farm_dm, na.rm = T),
              rough_of_dm = sum(rough_of_dm, na.rm = T),
              conc_of_dm = sum(conc_of_dm, na.rm = T),
              conc_ip_dm = sum(conc_ip_dm, na.rm = T),
              grasses_dm = sum(grasses_dm, na.rm = T),
              tree_legume_dm = sum(tree_legume_dm, na.rm = T))

  # Seasonal DM required
  seasonal_dm_required <- land_required %>%
    group_by(feed, season_name) %>%
    summarise(feed_item_dm_total = sum(feed_item_dm, na.rm = T)) %>%
    pivot_wider(names_from = season_name, values_from = feed_item_dm_total)

  # Join
  land_required <- left_join(seasonal_land_required, ex_land_required, by = "feed")
  dm_required <- left_join(seasonal_dm_required, ex_dm_required, by = "feed")

  total_area_used_for_feed_production_ha <- sum(land_required$total_area, na.rm = T)
  area_required_on_farm_ha <- sum(land_required$farm, na.rm = T)
  area_required_roughages_off_farm_ha <- sum(land_required$rough_of, na.rm = T)
  area_required_concentrates_off_farm_ha <- sum(land_required$conc_of, na.rm = T)
  area_required_imported_concentrates_ha <- sum(land_required$conc_ip, na.rm = T)

  total_dm_used_for_feed_production_kg <- sum(dm_required$total_dm, na.rm = T)
  dm_required_on_farm_kg <- sum(dm_required$farm_dm, na.rm = T)
  dm_required_roughages_off_farm_kg <- sum(dm_required$rough_of_dm, na.rm = T)
  dm_required_concentrates_off_farm_kg <- sum(dm_required$conc_of_dm, na.rm = T)
  dm_required_imported_concentrates_kg <- sum(dm_required$conc_ip_dm, na.rm = T)

  land_and_dm_required <- rbind(total_area_used_for_feed_production_ha,
                                area_required_on_farm_ha,
                                area_required_roughages_off_farm_ha,
                                area_required_concentrates_off_farm_ha,
                                area_required_imported_concentrates_ha,
                                NA,
                                total_dm_used_for_feed_production_kg,
                                dm_required_on_farm_kg,
                                dm_required_roughages_off_farm_kg,
                                dm_required_concentrates_off_farm_kg,
                                dm_required_imported_concentrates_kg)

  Items <- rownames(land_and_dm_required)
  rownames(land_and_dm_required) <- NULL
  land_and_dm_required <- as.data.frame(cbind(Items,land_and_dm_required))
  names(land_and_dm_required) <- c("Names","Value")

  land_required <- list(land_required = land_required,
                        dm_required = dm_required,
                        land_and_dm_required = land_and_dm_required)
  ###############################################################################################
  ## Productivity
  ###############################################################################################


  ###############################################################################################
  ## Soil impact
  ###############################################################################################
  # Plotting N balance
  nitrogen_balance %>%
    group_by(feed) %>%
    summarise(nbalance_kg_n_total = sum(nbalance_kg_n_total, na.rm = T)) %>%
    ggplot2::ggplot(aes(x=feed, y=nbalance_kg_n_total))+
    geom_bar(stat = "identity", width = 0.6)+
    labs(x = "Feed Item", y = "Kg N", title = "Total Nitrogen Balance by Feed Item") +
    geom_text(aes(label = round(nbalance_kg_n_total, 2)), vjust = -0.5, size = 3, angle = 45) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(paste0(directoryPath, "/nbalance.png"), width = 150, height = 100, units = "mm")

  ## OVERALL SOIL IMPACTS
  nitrogen_balance <- nitrogen_balance

  soil_erosion <- soil_erosion %>%
    mutate(rough_of_soil_loss = ifelse(stringr::str_detect(feed_item, "OFR"), as.numeric(soil_loss_plot), 0),
           conc_of_soil_loss = ifelse(stringr::str_detect(feed_item, "OFC"), as.numeric(soil_loss_plot), 0),
           conc_ip_soil_loss = ifelse(stringr::str_detect(feed_item, "IP"), as.numeric(soil_loss_plot), 0),
           farm_soil_loss = (as.numeric(soil_loss_plot) - rough_of_soil_loss - conc_of_soil_loss - conc_ip_soil_loss))

    overal_soil_impact <- data.frame(
    sources = c("total", "on-farm", "rough of", "conc of", "conc ip"),
    balance_N_kg_N_year = c(sum(nitrogen_balance$nbalance_feed_only_kg_n, na.rm = T), sum(nitrogen_balance$farm_kg_n, na.rm = T),
                            sum(nitrogen_balance$rough_of_kg_n, na.rm = T), sum(nitrogen_balance$conc_of_kg_n, na.rm = T), sum(nitrogen_balance$conc_ip_kg_n, na.rm = T)),
    balance_N_kg_N_ha = c(sum(nitrogen_balance$nbalance_feed_only_kg_n_ha, na.rm = T), sum(nitrogen_balance$farm_kg_n_ha, na.rm = T),
                          sum(nitrogen_balance$rough_of_kg_n_ha, na.rm = T), sum(nitrogen_balance$conc_of_kg_n_ha, na.rm = T), sum(nitrogen_balance$conc_ip_kg_n_ha, na.rm = T)),
    percent_area_mining = c(sum(nitrogen_balance$area_mining, na.rm = T)*100/sum(nitrogen_balance$area_total, na.rm = T), sum(nitrogen_balance$farm_area_mining, na.rm = T)*100/sum(nitrogen_balance$farm_area, na.rm = T),
                            sum(nitrogen_balance$rough_of_area_mining, na.rm = T)*100/sum(nitrogen_balance$rough_of_area, na.rm = T),sum(nitrogen_balance$conc_of_nue_area_mining, na.rm = T)*100/sum(nitrogen_balance$conc_of_area, na.rm = T),sum(nitrogen_balance$conc_ip_nue_area_mining, na.rm = T)*100/sum(nitrogen_balance$conc_ip_area, na.rm = T)),
    percent_area_leaching = c(sum(nitrogen_balance$area_leaching, na.rm = T)*100/sum(nitrogen_balance$area_total, na.rm = T), sum(nitrogen_balance$farm_area_leaching, na.rm = T)*100/sum(nitrogen_balance$farm_area, na.rm = T),
                              sum(nitrogen_balance$rough_of_area_leaching, na.rm = T)*100/sum(nitrogen_balance$rough_of_area, na.rm = T),sum(nitrogen_balance$conc_of_nue_area_leaching, na.rm = T)*100/sum(nitrogen_balance$conc_of_area, na.rm = T),sum(nitrogen_balance$conc_ip_nue_area_leaching, na.rm = T)*100/sum(nitrogen_balance$conc_ip_area, na.rm = T)),
    erosion_t_soil_year = c(sum(as.numeric(soil_erosion$soil_loss_plot, na.rm = T)), sum(soil_erosion$rough_of_soil_loss, na.rm = T), sum(soil_erosion$conc_of_soil_loss, na.rm = T),
                            sum(soil_erosion$conc_ip_soil_loss, na.rm = T), sum(soil_erosion$farm_soil_loss, na.rm = T))
  )%>%
      mutate(percent_area_mining = ifelse(!is.finite(percent_area_mining),0,percent_area_mining),
             percent_area_leaching = ifelse(!is.finite(percent_area_leaching),0,percent_area_leaching),
             erosion_t_soil_ha = erosion_t_soil_year/c(sum(nitrogen_balance$area_total, na.rm = T), sum(nitrogen_balance$farm_area, na.rm = T), sum(nitrogen_balance$rough_of_area, na.rm = T),
                                                       sum(nitrogen_balance$conc_of_area, na.rm = T), sum(nitrogen_balance$conc_ip_area, na.rm = T)))

  # Feed items specific N balance
  nitrogen_balance <- nitrogen_balance %>%
      select(c(feed,nin,nout,nbalance_kg_n_total,nbalance_kg_n_ha_total,nbalance_feed_only_kg_n,nbalance_feed_only_kg_n_ha)) %>%
      mutate(nbalance_food_only_kg_n = nbalance_kg_n_total-nbalance_feed_only_kg_n,
             nbalance_food_only_kg_n_ha = nbalance_kg_n_ha_total-nbalance_feed_only_kg_n_ha)

  soil_impacts <- list(overal_soil_impact = overal_soil_impact,
                       nitrogen_balance = nitrogen_balance)

  ###############################################################################################
  ## Water Impacts
  ###############################################################################################
  water_use_per_feed_item <- water_required[["water_use_per_feed_item"]]

  ggplot(water_use_per_feed_item, aes(x = "", y = feed_water_use, fill = feed)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(fill = "Feed Item", title = "Water Use per Feed Crop") +
    theme_bw()+
    geom_text(aes(label = scales::percent(feed_water_use / sum(feed_water_use))),
              position = position_stack(vjust = 0.5),
              size = 1)

  ggsave(paste0(directoryPath, "/water_use_per_feed.png"), width = 150, height = 100, units = "mm")

  ###############################################################################################
  ## GHG Impacts
  ###############################################################################################

  # GHG Balance
  methane	<- 28
  N2O	<- 265

  # On-farm
  enteric_fermentation_methane <- sum(ghg_emissions[["ef"]]$enteric_methane_emissions,na.rm = T)
  enteric_fermentation_methane_tot_kg_co2_e <- enteric_fermentation_methane*methane
  enteric_fermentation_methane_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,enteric_fermentation_methane_tot_kg_co2_e/area_required_on_farm_ha)
  enteric_fermentation_methane_kg_co2_e_per_kg_fpcm <- enteric_fermentation_methane_tot_kg_co2_e/sum(livestock_productivity$total_milk, na.rm = T)

  manure_methane <- sum(ghg_emissions[["eft"]]$emission_factor,na.rm = T)
  manure_methane_tot_kg_co2_e <- manure_methane*methane
  manure_methane_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,manure_methane_tot_kg_co2_e/area_required_on_farm_ha)
  manure_methane_kg_co2_e_per_kg_fpcm <- manure_methane_tot_kg_co2_e/sum(livestock_productivity$total_milk, na.rm = T)

  manure_direct_N2O <- sum(ghg_emissions[["direct_N2O"]]$direct_N2O_emission,na.rm = T)
  manure_direct_N2O_tot_kg_co2_e <- manure_direct_N2O*N2O
  manure_direct_N2O_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,manure_direct_N2O_tot_kg_co2_e/area_required_on_farm_ha)
  manure_direct_N2O_kg_co2_e_per_kg_fpcm <- manure_direct_N2O_tot_kg_co2_e/sum(livestock_productivity$total_milk, na.rm = T)

  manure_Indirect_N2O <- sum(ghg_emissions[["indirect_N2O"]]$indirect_N2O_emission,na.rm = T)
  manure_Indirect_N2O_tot_kg_co2_e <- manure_Indirect_N2O*N2O
  manure_Indirect_N2O_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,manure_Indirect_N2O_tot_kg_co2_e/area_required_on_farm_ha)
  manure_Indirect_N2O_kg_co2_e_per_kg_fpcm <- manure_Indirect_N2O_tot_kg_co2_e/sum(livestock_productivity$total_milk, na.rm = T)

  soil_direct_N2O <- sum(ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]]$anthropogenic_N_input == "farm_n_synthetic_fertilizer_managed_soil","annual_N20N_direct_emission_from_managed_soil"],
                         ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]]$anthropogenic_N_input == "farm_n_from_crop_residue_managed_soil","annual_N20N_direct_emission_from_managed_soil"],
                         ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]]$anthropogenic_N_input == "n_synthetic_fertilizer_flooded_rice","annual_N20N_direct_emission_from_managed_soil"],
                         ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]]$anthropogenic_N_input == "n_organic_manure_flooded_rice","annual_N20N_direct_emission_from_managed_soil"],
                         ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]]$anthropogenic_N_input == "n_from_crop_residue_flooded_rice","annual_N20N_direct_emission_from_managed_soil"],
                         ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]]$anthropogenic_N_input == "cattle_pig_poultry_n_pasture_onfarm","annual_N20N_direct_emission_from_managed_soil"],
                         ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]]$anthropogenic_N_input == "sheep_and_other_n_pasture_onfarm","annual_N20N_direct_emission_from_managed_soil"],na.rm = T)
  soil_direct_N2O_tot_kg_co2_e <- soil_direct_N2O*N2O
  soil_direct_N2O_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,soil_direct_N2O_tot_kg_co2_e/area_required_on_farm_ha)
  soil_direct_N2O_kg_co2_e_per_kg_fpcm <- soil_direct_N2O_tot_kg_co2_e/sum(livestock_productivity$total_milk, na.rm = T)

  soil_indirect_N2O <- sum(ghg_emissions[["ghg_soil"]][["annual_N20N_soil_indirect_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_indirect_emission"]]$anthropogenic_N_input == "farm_n_synthetic_fertilizer_managed_soil", "annual_N20N_from_atmospheric_deposition"],na.rm = T)
  soil_indirect_N2O_tot_kg_co2_e <- soil_indirect_N2O*N2O
  soil_indirect_N2O_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,soil_indirect_N2O_tot_kg_co2_e/area_required_on_farm_ha)
  soil_indirect_N2O_kg_co2_e_per_kg_fpcm <- soil_indirect_N2O_tot_kg_co2_e/sum(livestock_productivity$total_milk, na.rm = T)

  burning <- ((ghg_emissions$ghg_burn[ghg_emissions$ghg_burn$ghg_gas=="CO2",5])+(ghg_emissions$ghg_burn[ghg_emissions$ghg_burn$ghg_gas=="CH4",5]*methane)+(ghg_emissions$ghg_burn[ghg_emissions$ghg_burn$ghg_gas=="Nox",5]*N2O))
  burning_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,burning/area_required_on_farm_ha)
  burning_kg_co2_e_per_kg_fpcm <- burning/sum(livestock_productivity$total_milk, na.rm = T)

  rice_production_methane <- sum(ghg_emissions$ghg_rice$annual_methane_emission,na.rm = T)
  rice_production_methane_tot_kg_co2_e <- rice_production_methane*methane
  rice_production_methane_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,rice_production_methane_tot_kg_co2_e/area_required_on_farm_ha)
  rice_production_methane_kg_co2_e_per_kg_fpcm <- rice_production_methane_tot_kg_co2_e/sum(livestock_productivity$total_milk, na.rm = T)

  on_farm_fertilizer_emission <- sum(ghg_emissions[["fetilizer_ghg"]][["fertlizer_emission_by_crop"]]$farm_fertiliser_emission)
  on_farm_fertilizer_emission_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,on_farm_fertilizer_emission/area_required_on_farm_ha)
  on_farm_fertilizer_emission_kg_co2_e_per_kg_fpcm <- on_farm_fertilizer_emission/sum(livestock_productivity$total_milk, na.rm = T)

  # Roughage off farm
  rough_of_Soil_direct_N2O <- sum(ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]]$anthropogenic_N_input == "rough_of_n_synthetic_fertilizer_managed_soil","annual_N20N_direct_emission_from_managed_soil"],
                                  ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]]$anthropogenic_N_input == "rough_of_n_from_crop_residue_managed_soil","annual_N20N_direct_emission_from_managed_soil"],na.rm = T)
  rough_of_Soil_direct_N2O_tot_kg_co2_e <- rough_of_Soil_direct_N2O*N2O
  rough_of_Soil_direct_N2O_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,rough_of_Soil_direct_N2O_tot_kg_co2_e/area_required_on_farm_ha)
  rough_of_Soil_direct_N2O_kg_co2_e_per_kg_fpcm <- rough_of_Soil_direct_N2O_tot_kg_co2_e/sum(livestock_productivity$total_milk, na.rm = T)

  rough_of_soil_indirect_N2O <- sum(ghg_emissions[["ghg_soil"]][["annual_N20N_soil_indirect_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_indirect_emission"]]$anthropogenic_N_input == "rough_of_n_synthetic_fertilizer_managed_soil", "annual_N20N_from_atmospheric_deposition"],na.rm = T)
  rough_of_soil_indirect_N2O_tot_kg_co2_e <- rough_of_soil_indirect_N2O*N2O
  rough_of_soil_indirect_N2O_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,rough_of_soil_indirect_N2O_tot_kg_co2_e/area_required_on_farm_ha)
  rough_of_soil_indirect_N2O_kg_co2_e_per_kg_fpcm <- rough_of_soil_indirect_N2O_tot_kg_co2_e/sum(livestock_productivity$total_milk, na.rm = T)

  rough_of_fertilizer_emission <- sum(ghg_emissions[["fetilizer_ghg"]][["fertlizer_emission_by_crop"]]$rough_of_fertiliser_emission)
  rough_of_fertilizer_emission_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,rough_of_fertilizer_emission/area_required_on_farm_ha)
  rough_of_fertilizer_emission_kg_co2_e_per_kg_fpcm <- rough_of_fertilizer_emission/sum(livestock_productivity$total_milk, na.rm = T)

  # Concentrates off-farm
  conc_of_Soil_direct_N2O <- sum(ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]]$anthropogenic_N_input == "conc_of_n_synthetic_fertilizer_managed_soil","annual_N20N_direct_emission_from_managed_soil"],
                                  ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]]$anthropogenic_N_input == "conc_of_n_from_crop_residue_managed_soil","annual_N20N_direct_emission_from_managed_soil"],na.rm = T)
  conc_of_Soil_direct_N2O_tot_kg_co2_e <- conc_of_Soil_direct_N2O*N2O
  conc_of_Soil_direct_N2O_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,conc_of_Soil_direct_N2O_tot_kg_co2_e/area_required_on_farm_ha)
  conc_of_Soil_direct_N2O_kg_co2_e_per_kg_fpcm <- conc_of_Soil_direct_N2O_tot_kg_co2_e/sum(livestock_productivity$total_milk, na.rm = T)

  conc_of_soil_indirect_N2O <- sum(ghg_emissions[["ghg_soil"]][["annual_N20N_soil_indirect_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_indirect_emission"]]$anthropogenic_N_input == "conc_of_n_synthetic_fertilizer_managed_soil", "annual_N20N_from_atmospheric_deposition"],na.rm = T)
  conc_of_soil_indirect_N2O_tot_kg_co2_e <- conc_of_soil_indirect_N2O*N2O
  conc_of_soil_indirect_N2O_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,conc_of_soil_indirect_N2O_tot_kg_co2_e/area_required_on_farm_ha)
  conc_of_soil_indirect_N2O_kg_co2_e_per_kg_fpcm <- conc_of_soil_indirect_N2O_tot_kg_co2_e/sum(livestock_productivity$total_milk, na.rm = T)

  conc_of_fertilizer_emission <- sum(ghg_emissions[["fetilizer_ghg"]][["fertlizer_emission_by_crop"]]$conc_of_fertiliser_emission)
  conc_of_fertilizer_emission_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,conc_of_fertilizer_emission/area_required_on_farm_ha)
  conc_of_fertilizer_emission_kg_co2_e_per_kg_fpcm <- conc_of_fertilizer_emission/sum(livestock_productivity$total_milk, na.rm = T)

  # Imported concentrates
  conc_ip_Soil_direct_N2O <- sum(ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]]$anthropogenic_N_input == "conc_ip_n_synthetic_fertilizer_managed_soil","annual_N20N_direct_emission_from_managed_soil"],
                                 ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_direct_emission"]]$anthropogenic_N_input == "conc_ip_n_from_crop_residue_managed_soil","annual_N20N_direct_emission_from_managed_soil"],na.rm = T)
  conc_ip_Soil_direct_N2O_tot_kg_co2_e <- conc_ip_Soil_direct_N2O*N2O
  conc_ip_Soil_direct_N2O_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,conc_ip_Soil_direct_N2O_tot_kg_co2_e/area_required_on_farm_ha)
  conc_ip_Soil_direct_N2O_kg_co2_e_per_kg_fpcm <- conc_ip_Soil_direct_N2O_tot_kg_co2_e/sum(livestock_productivity$total_milk, na.rm = T)

  conc_ip_soil_indirect_N2O <- sum(ghg_emissions[["ghg_soil"]][["annual_N20N_soil_indirect_emission"]][ghg_emissions[["ghg_soil"]][["annual_N20N_soil_indirect_emission"]]$anthropogenic_N_input == "conc_ip_n_synthetic_fertilizer_managed_soil", "annual_N20N_from_atmospheric_deposition"],na.rm = T)
  conc_ip_soil_indirect_N2O_tot_kg_co2_e <- conc_ip_soil_indirect_N2O*N2O
  conc_ip_soil_indirect_N2O_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,conc_ip_soil_indirect_N2O_tot_kg_co2_e/area_required_on_farm_ha)
  conc_ip_soil_indirect_N2O_kg_co2_e_per_kg_fpcm <- conc_ip_soil_indirect_N2O_tot_kg_co2_e/sum(livestock_productivity$total_milk, na.rm = T)

  conc_ip_fertilizer_emission <- sum(ghg_emissions[["fetilizer_ghg"]][["fertlizer_emission_by_crop"]]$conc_ip_fertiliser_emission)
  conc_ip_fertilizer_emission_per_ha_kg_co2_e <- ifelse(area_required_on_farm_ha<0.001,0,conc_ip_fertilizer_emission/area_required_on_farm_ha)
  conc_ip_fertilizer_emission_kg_co2_e_per_kg_fpcm <- conc_ip_fertilizer_emission/sum(livestock_productivity$total_milk, na.rm = T)

  ghg_balance <- data.frame(
    GHG_balance = c("On-farm", "Enteric fermentation-Methane",
                    "Manure-Methane",
                    "Manure-Direct N2O",
                    "Manure-Indirect N2O",
                    "Soil-Direct N2O",
                    "Soil-Indirect N2O",
                    "Burning",
                    "Rice production-Methane",
                    "Production fertilizer",
                    "Roughages off-farm", "Soil-Direct N2O",
                    "Soil-Indirect N2O",
                    "Production fertilizer",
                    "Concentrates off-farm", "Soil-Direct N2O",
                    "Soil-Indirect N2O",
                    "Production fertilizer",
                    "Imported concentrates", "Soil-Direct N2O",
                    "Soil-Indirect N2O",
                    "Production fertilizer"),
    si_units = c("", "kg CH4",
                 "kg CH4",
                 "kg N2O",
                 "kg N2O",
                 "kg N2O",
                 "kg N2O",
                 "kg CO2e",
                 "kg CH4",
                 "kg CO2e",
                 "", "kg N2O",
                 "kg N2O",
                 "kg CO2e",
                 "", "kg N2O",
                 "kg N2O",
                 "kg CO2e",
                 "", "kg N2O",
                 "kg N2O",
                 "kg CO2e"),
    value = c("", enteric_fermentation_methane,
              manure_methane,
              manure_direct_N2O,
              manure_Indirect_N2O,
              soil_direct_N2O,
              soil_indirect_N2O,
              burning,
              rice_production_methane,
              on_farm_fertilizer_emission,
              "", rough_of_Soil_direct_N2O,
              rough_of_soil_indirect_N2O,
              rough_of_fertilizer_emission,
              "", conc_of_Soil_direct_N2O,
              conc_of_soil_indirect_N2O,
              conc_of_fertilizer_emission,
              "", conc_ip_Soil_direct_N2O,
              conc_ip_soil_indirect_N2O,
              conc_ip_fertilizer_emission),
    kg_co2_e_per_ha = c("", enteric_fermentation_methane_per_ha_kg_co2_e,
                        manure_methane_per_ha_kg_co2_e,
                        manure_direct_N2O_per_ha_kg_co2_e,
                        manure_Indirect_N2O_per_ha_kg_co2_e,
                        soil_direct_N2O_per_ha_kg_co2_e,
                        soil_indirect_N2O_per_ha_kg_co2_e,
                        burning_per_ha_kg_co2_e,
                        rice_production_methane_per_ha_kg_co2_e,
                        on_farm_fertilizer_emission_per_ha_kg_co2_e,
                        "", rough_of_Soil_direct_N2O_per_ha_kg_co2_e,
                        rough_of_soil_indirect_N2O_per_ha_kg_co2_e,
                        rough_of_fertilizer_emission_per_ha_kg_co2_e,
                        "", conc_of_Soil_direct_N2O_per_ha_kg_co2_e,
                        conc_of_soil_indirect_N2O_per_ha_kg_co2_e,
                        conc_of_fertilizer_emission_per_ha_kg_co2_e,
                        "", conc_ip_Soil_direct_N2O_per_ha_kg_co2_e,
                        conc_ip_soil_indirect_N2O_per_ha_kg_co2_e,
                        conc_ip_fertilizer_emission_per_ha_kg_co2_e),
    kg_co2_e_tot = c("", enteric_fermentation_methane_tot_kg_co2_e,
                     manure_methane_per_ha_kg_co2_e,
                     manure_direct_N2O_tot_kg_co2_e,
                     manure_Indirect_N2O_tot_kg_co2_e,
                     soil_direct_N2O_tot_kg_co2_e,
                     soil_indirect_N2O_tot_kg_co2_e,
                     burning,
                     rice_production_methane_tot_kg_co2_e,
                     on_farm_fertilizer_emission,
                     "", rough_of_Soil_direct_N2O_tot_kg_co2_e,
                     rough_of_soil_indirect_N2O_tot_kg_co2_e,
                     rough_of_fertilizer_emission,
                     "", conc_of_Soil_direct_N2O_tot_kg_co2_e,
                     conc_of_soil_indirect_N2O_tot_kg_co2_e,
                     conc_of_fertilizer_emission,
                     "", conc_ip_Soil_direct_N2O_tot_kg_co2_e,
                     conc_ip_soil_indirect_N2O_tot_kg_co2_e,
                     conc_ip_fertilizer_emission),
    kg_co2_e_per_kg_fpcm = c("", enteric_fermentation_methane_kg_co2_e_per_kg_fpcm,
                             manure_methane_kg_co2_e_per_kg_fpcm,
                             manure_direct_N2O_kg_co2_e_per_kg_fpcm,
                             manure_Indirect_N2O_kg_co2_e_per_kg_fpcm,
                             soil_direct_N2O_kg_co2_e_per_kg_fpcm,
                             soil_indirect_N2O_kg_co2_e_per_kg_fpcm,
                             burning_kg_co2_e_per_kg_fpcm,
                             rice_production_methane_kg_co2_e_per_kg_fpcm,
                             on_farm_fertilizer_emission_kg_co2_e_per_kg_fpcm,
                             "", rough_of_Soil_direct_N2O_kg_co2_e_per_kg_fpcm,
                             rough_of_soil_indirect_N2O_kg_co2_e_per_kg_fpcm,
                             rough_of_fertilizer_emission_kg_co2_e_per_kg_fpcm,
                             "", conc_of_Soil_direct_N2O_kg_co2_e_per_kg_fpcm,
                             conc_of_soil_indirect_N2O_kg_co2_e_per_kg_fpcm,
                             conc_of_fertilizer_emission_kg_co2_e_per_kg_fpcm,
                             "", conc_ip_Soil_direct_N2O_kg_co2_e_per_kg_fpcm,
                             conc_ip_soil_indirect_N2O_kg_co2_e_per_kg_fpcm,
                             conc_ip_fertilizer_emission_kg_co2_e_per_kg_fpcm)
    )

  # Global warming potential (CO2eq)
  soil_on_farm <- (as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Direct N2O", "kg_co2_e_per_ha"][1]) +
    as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Indirect N2O", "kg_co2_e_per_ha"][1]))/1000

  soil_off_farm <- (sum(as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Direct N2O", "kg_co2_e_per_ha"][-1])) +
    sum(as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Indirect N2O", "kg_co2_e_per_ha"][-1])))/1000

  livestock_manure <- (as.numeric(ghg_balance[ghg_balance$GHG_balance == "Manure-Methane", "kg_co2_e_per_ha"]) +
    as.numeric(ghg_balance[ghg_balance$GHG_balance == "Manure-Direct N2O", "kg_co2_e_per_ha"]) +
    as.numeric(ghg_balance[ghg_balance$GHG_balance == "Manure-Indirect N2O", "kg_co2_e_per_ha"]))/1000

  livestock_enteric_fermentation <- as.numeric(ghg_balance[ghg_balance$GHG_balance == "Enteric fermentation-Methane", "kg_co2_e_per_ha"])/1000

  burning_emission <- as.numeric(ghg_balance[ghg_balance$GHG_balance == "Burning", "kg_co2_e_per_ha"])/1000

  rice <- as.numeric(ghg_balance[ghg_balance$GHG_balance == "Rice production-Methane", "kg_co2_e_per_ha"])/1000

  fertilizer_on_farm <- as.numeric(ghg_balance[ghg_balance$GHG_balance == "Production fertilizer", "kg_co2_e_per_ha"][1])/1000

  soil_off_farm_rough <- (sum(as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Direct N2O", "kg_co2_e_per_ha"][2])) +
                            sum(as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Indirect N2O", "kg_co2_e_per_ha"][2])))/1000

  fertilizer_off_farm_rough <- as.numeric(ghg_balance[ghg_balance$GHG_balance == "Production fertilizer", "kg_co2_e_per_ha"][2])/1000

  soil_off_farm_conc <- (sum(as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Direct N2O", "kg_co2_e_per_ha"][3])) +
                            sum(as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Indirect N2O", "kg_co2_e_per_ha"][3])))/1000

  fertilizer_off_farm_conc <- as.numeric(ghg_balance[ghg_balance$GHG_balance == "Production fertilizer", "kg_co2_e_per_ha"][3])/1000

  soil_ip_farm_conc <- (sum(as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Direct N2O", "kg_co2_e_per_ha"][4])) +
                            sum(as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Indirect N2O", "kg_co2_e_per_ha"][4])))/1000

  fertilizer_ip_farm_conc <- as.numeric(ghg_balance[ghg_balance$GHG_balance == "Production fertilizer", "kg_co2_e_per_ha"][4])/1000

  on_farm_table <- data.frame(
    sources_and_sinks = c("Soil","Off-farm Soil", "Liv. Manure", "Liv.enteric fermentation", "Burning", "Rice", "Fertilizer"),
    t_CO2e_per_ha = c(soil_on_farm, soil_off_farm, livestock_manure, livestock_enteric_fermentation, burning_emission, rice, fertilizer_on_farm)
  )

  off_farm_table <- data.frame(
    sources_and_sinks = c("Roughages off-farm", "Soil off-farm", "Fertilizer off-farm", "Concentrates off-farm", "Soil off-farm", "Fertilizer off-farm", "Imported concentrates", "Soil off-farm", "Fertilizer off-farm"),
    t_CO2e_per_ha = c("", soil_off_farm_rough, fertilizer_off_farm_rough, "", soil_off_farm_conc, fertilizer_off_farm_conc, "", soil_ip_farm_conc, fertilizer_ip_farm_conc)
  )

  global_warming_potential <- rbind(data.frame(sources_and_sinks = c("On-farm"),t_CO2e_per_ha = c("")),
                                    on_farm_table,
                                    off_farm_table)
  ghg_emissions <- list(ghg_balance = ghg_balance,
                        global_warming_potential = global_warming_potential)

  # Plotting GHG emission
  on_farm_table %>%
    ggplot2::ggplot(aes(x=sources_and_sinks, y=t_CO2e_per_ha))+
    geom_bar(stat = "identity", width = 0.6)+
    labs(x = "", y = "t CO2e/ha", title = "GHG emissions") +
    geom_text(aes(label = round(t_CO2e_per_ha, 2)), vjust = -0.5, size = 3, angle = 45) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(paste0(directoryPath, "/ghg_emission.png"), width = 150, height = 100, units = "mm")


  output_list <- list(land_required = land_required,
                      soil_impacts = soil_impacts,
                      water_required = water_required,
                      livestock_productivity = livestock_productivity,
                      ghg_emission = ghg_emission)

  jsonlite::toJSON(output_list, pretty = TRUE)

} #end of output function
