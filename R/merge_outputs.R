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
    labs(x = "Feed Item", y = "Area (Ha)", fill = "Seasons") +
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
  ## OVERALL SOIL IMPACTS
  nitrogen_balance <- nitrogen_balance %>%
    mutate(rough_of_kg_n = ifelse(stringr::str_detect(feed, "OFR"), nbalance_feed_only_kg_n, 0),
           conc_of_kg_n = ifelse(stringr::str_detect(feed, "OFC"), nbalance_feed_only_kg_n, 0),
           conc_ip_kg_n = ifelse(stringr::str_detect(feed, "IP"), nbalance_feed_only_kg_n, 0),
           farm_kg_n = (nbalance_feed_only_kg_n - rough_of_n - conc_of_n - conc_ip_n),
           rough_of_kg_n_ha = ifelse(stringr::str_detect(feed, "OFR"), nbalance_feed_only_kg_n_ha, 0),
           conc_of_kg_n_ha = ifelse(stringr::str_detect(feed, "OFC"), nbalance_feed_only_kg_n_ha, 0),
           conc_ip_kg_n_ha = ifelse(stringr::str_detect(feed, "IP"), nbalance_feed_only_kg_n_ha, 0),
           farm_kg_n_ha = (nbalance_feed_only_kg_n_ha - rough_of_kg_n_ha - conc_of_kg_n_ha - conc_ip_kg_n_ha),
           rough_of_nue = ifelse(stringr::str_detect(feed, "OFR"), nue, 0),
           conc_of_nue = ifelse(stringr::str_detect(feed, "OFC"), nue, 0),
           conc_ip_nue = ifelse(stringr::str_detect(feed, "IP"), nue, 0),
           farm_nue = (nue - rough_of_nue - conc_of_nue - conc_ip_nue),
           area_mining = ifelse(nue>0.9,nue,0),
           farm_area_mining = ifelse(farm_nue>0.9,farm_nue,0),
           rough_of_area_mining = ifelse(rough_of_nue>0.9,rough_of_nue,0),
           conc_of_nue_area_mining = ifelse(conc_of_nue>0.9,conc_of_nue,0),
           conc_ip_nue_area_mining = ifelse(conc_ip_nue>0.9,conc_ip_nue,0),
           rough_of_area = ifelse(stringr::str_detect(feed, "OFR"), area_total, 0),
           conc_of_area = ifelse(stringr::str_detect(feed, "OFC"), area_total, 0),
           conc_ip_area = ifelse(stringr::str_detect(feed, "IP"), area_total, 0),
           farm_area = (area_total - rough_of_area - conc_of_area - conc_ip_area),
           area_leaching = ifelse(nue>0.9,nue,0),
           farm_area_mining = ifelse(farm_nue>0.9,farm_nue,0),
           rough_of_area_mining = ifelse(rough_of_nue>0.9,rough_of_nue,0),
           conc_of_nue_area_mining = ifelse(conc_of_nue>0.9,conc_of_nue,0),
           conc_ip_nue_area_mining = ifelse(conc_ip_nue>0.9,conc_ip_nue,0))

    overal_soil_impact <- data.frame(
    sources = c("total", "on-farm", "rough of", "conc of", "conc ip"),
    balance_N_kg_N_year = c(sum(nitrogen_balance$nbalance_feed_only_kg_n, na.rm = T), sum(nitrogen_balance$farm_kg_n, na.rm = T),
                            sum(nitrogen_balance$rough_of_kg_n, na.rm = T), sum(nitrogen_balance$conc_of_kg_n, na.rm = T), sum(nitrogen_balance$conc_ip_kg_n, na.rm = T)),
    balance_N_kg_N_ha = c(sum(nitrogen_balance$nbalance_feed_only_kg_n_ha, na.rm = T), sum(nitrogen_balance$farm_kg_n_ha, na.rm = T),
                          sum(nitrogen_balance$rough_of_kg_n_ha, na.rm = T), sum(nitrogen_balance$conc_of_kg_n_ha, na.rm = T), sum(nitrogen_balance$conc_ip_kg_n_ha, na.rm = T)),
    percent_area_mining = c(sum(nitrogen_balance$area_mining, na.rm = T)*100/sum(nitrogen_balance$area_total, na.rm = T), sum(nitrogen_balance$farm_area_mining, na.rm = T)*100/sum(nitrogen_balance$farm_area, na.rm = T),
                            sum(nitrogen_balance$rough_of_area_mining, na.rm = T)*100/sum(nitrogen_balance$rough_of_area, na.rm = T),sum(nitrogen_balance$conc_of_nue_area_mining, na.rm = T)*100/sum(nitrogen_balance$conc_of_area, na.rm = T),sum(nitrogen_balance$conc_ip_nue_area_mining, na.rm = T)*100/sum(nitrogen_balance$conc_ip_area, na.rm = T))
  )%>%
      mutate(percent_area_mining = ifelse(!is.finite(percent_area_mining),0,percent_area_mining))


  nitrogen_balance <- nitrogen_balance %>%
    select(c(feed,nin,nout,nbalance_kg_n_total,nbalance_kg_n_ha_total,nbalance_feed_only_kg_n,nbalance_feed_only_kg_n_ha)) %>%
    mutate(nbalance_food_only_kg_n = nbalance_kg_n_total-nbalance_feed_only_kg_n,
           nbalance_food_only_kg_n_ha = nbalance_kg_n_ha_total-nbalance_feed_only_kg_n_ha)





  output_list <- list(feed_basket_quality = feed_basket_quality,
                      energy_required = energy_required,
                      land_required = land_required,
                      soil_erosion = soil_erosion,
                      water_required = water_required,
                      nitrogen_balance = nitrogen_balance,
                      livestock_productivity = livestock_productivity,
                      biomass = biomass,
                      soil_carbon = soil_carbon,
                      ghg_emission = ghg_emission)

  jsonlite::toJSON(output_list, pretty = TRUE)

} #end of output function
