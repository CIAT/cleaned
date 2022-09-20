#' @title Soil Organic Carbon
#'
#' @description It computes soil organic carbon status
#'
#' @param para A JSON file
#'
#' @param stock_change_para A JSON file with stock change parameters
#'
#' @param land_required A dataframe computed using the `land_requirement` function
#'
#' @param biomass A dataframe computed using the `biomass_calculation` function
#'
#' @return dataframe
#'
#' @importFrom dplyr mutate %>% everything
#'
#' @importFrom tidyr unnest
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' feed_basket_quality <- feed_quality(para)
#' energy_required <- energy_requirement(para,feed_basket_quality)
#' land_required <- land_requirement(feed_basket_quality, energy_required, para)
#' soil_erosion <- soil_health(para, land_required)
#' nitrogen_balance <- n_balance(para, land_required, soil_erosion)
#' livestock_productivity <- land_productivity(para)
#' economics <- economics_payback(para, energy_required)
#' biomass <- biomass_calculation(para, land_required)
#' soil_organic_carbon(para, stock_change_para, land_required, biomass)
#' }
#'
#' @export

soil_organic_carbon <- function(para, stock_change_para, land_required, biomass) {

  co2_conversion_factor <- 44/12

  soil_amount <- 1000000*(para[["soil_depth"]]/100)*para[["soil_bulk"]]

  field_soc <- soil_amount*para[["soil_c"]]*0.001

  land_required <- land_required[["land_requirements_all"]] %>%
    as.data.frame()

  cropland_change_carbon_stocks_mineral_soils <- data.frame("initial_landuse" = "cropland") %>%
    mutate(reporting_landuse = "cropland",
           area_last_year_inventory_period = sum(land_required$area_feed)-(sum(land_required$grasses)+sum(land_required$tree_legume)),
           carbon_stock_last_year_inventory_period = ifelse(field_soc>0, field_soc, 30),
           time_dependence_stock_change = 20,
           stock_change_factor_land_use = unnest(stock_change_para[["cropland"]], cols = c(landuse)) %>%
             unnest(cols = c(factor_variables)) %>%
             select(paste0(para[["cropland_system"]])) %>%
             as.numeric(),
           stock_change_factor_management = unnest(stock_change_para[["cropland"]], cols = c(tillage)) %>%
             unnest(cols = c(factor_variables)) %>%
             select(paste0(para[["cropland_tillage"]])) %>%
             as.numeric(),
           stock_change_factor_input = unnest(stock_change_para[["cropland"]], cols = c(input)) %>%
             unnest(cols = c(factor_variables)) %>%
             select(paste0(para[["cropland_orgmatter"]])) %>%
             as.numeric(),
           annual_change_carbon_stocks_mineral_soils = ((carbon_stock_last_year_inventory_period*stock_change_factor_land_use*stock_change_factor_management*stock_change_factor_input)-carbon_stock_last_year_inventory_period)/time_dependence_stock_change*area_last_year_inventory_period)


  grassland_change_carbon_stocks_mineral_soils <- data.frame("initial_landuse" = "grassland") %>%
    mutate(reporting_landuse = "grassland",
           area_last_year_inventory_period = sum(land_required[which(land_required$grasses>0),]$farm),
           carbon_stock_last_year_inventory_period = 40, #hardcoded in the excel sheet 40
           time_dependence_stock_change = 20,
           stock_change_factor_land_use = unnest(stock_change_para[["grassland"]], cols = c(landuse)) %>%
             unnest(cols = c(factor_variables)) %>%
             select(everything("All")) %>%
             as.numeric(),
           stock_change_factor_management = unnest(stock_change_para[["grassland"]], cols = c(management)) %>%
             unnest(cols = c(factor_variables)) %>%
             select(paste0(para[["grassland_management"]])) %>%
             as.numeric(),
           stock_change_factor_input = unnest(stock_change_para[["grassland"]], cols = c(input)) %>%
             unnest(cols = c(factor_variables)) %>%
             select(paste0(para[["grassland_implevel"]])) %>%
             as.numeric(),
           annual_change_carbon_stocks_mineral_soils = ((carbon_stock_last_year_inventory_period*stock_change_factor_land_use*stock_change_factor_management*stock_change_factor_input)-carbon_stock_last_year_inventory_period)/time_dependence_stock_change*area_last_year_inventory_period)

  off_farm_grassland_change_carbon_stocks_mineral_soils <- data.frame("initial_landuse" = "off_farm_grassland") %>%
    mutate(reporting_landuse = "off_farm_grassland",
           area_last_year_inventory_period = sum(land_required[which(land_required$grasses>0),]$rough_of),
           carbon_stock_last_year_inventory_period = 40, #hardcoded in the excel sheet 40
           time_dependence_stock_change = 20,
           stock_change_factor_land_use = unnest(stock_change_para[["grassland"]], cols = c(landuse)) %>%
             unnest(cols = c(factor_variables)) %>%
             select(everything("All")) %>%
             as.numeric(),
           stock_change_factor_management = unnest(stock_change_para[["grassland"]], cols = c(management)) %>%
             unnest(cols = c(factor_variables)) %>%
             select(paste0(para[["grassland_management"]])) %>%
             as.numeric(),
           stock_change_factor_input = unnest(stock_change_para[["grassland"]], cols = c(input)) %>%
             unnest(cols = c(factor_variables)) %>%
             select(paste0(para[["grassland_implevel"]])) %>%
             as.numeric(),
           annual_change_carbon_stocks_mineral_soils = ((carbon_stock_last_year_inventory_period*stock_change_factor_land_use*stock_change_factor_management*stock_change_factor_input)-carbon_stock_last_year_inventory_period)/time_dependence_stock_change*area_last_year_inventory_period)

  annual_change_carbon_stocks_mineral_soils <- rbind(cropland_change_carbon_stocks_mineral_soils,
                                                     grassland_change_carbon_stocks_mineral_soils,
                                                     off_farm_grassland_change_carbon_stocks_mineral_soils)

  # This section is still under research and will be revisited
  annual_change_carbon_stocks_organic_soils <- data.frame("initial_landuse" = "cropland") %>%
    mutate(reporting_landuse = "cropland",
           land_area_cultivated_organic_soil = 0,
           emission_factor_climate_type = 0,
           carbon_loss_cultivated_organic_soils = land_area_cultivated_organic_soil*emission_factor_climate_type)

  # End of the section

  annual_change_carbon_stocks_soils <- data.frame("initial_landuse" = "cropland") %>%
    mutate(reporting_landuse = "cropland",
           annual_change_carbon_stocks_mineral_soils = sum(annual_change_carbon_stocks_mineral_soils$annual_change_carbon_stocks_mineral_soils),
           annual_carbon_loss_cultivated_organic_soils = sum(annual_change_carbon_stocks_organic_soils$carbon_loss_cultivated_organic_soils),
           annual_change_inorganic_carbon_stocks_soils = 0,
           annual_change_carbon_stocks_soils = annual_change_carbon_stocks_mineral_soils+annual_carbon_loss_cultivated_organic_soils+annual_change_inorganic_carbon_stocks_soils)

  # annual_change_carbon_stocks_trees_feed <- data.frame("type" = "trees_feed") %>%
  #   mutate(biomass = sum(biomass[["tier3"]]$carbon_biomass_balance)/1000,
  #          below_ground = biomass*0.25,
  #          annual_change_carbon_stocks = below_ground/1000)

  annual_change_carbon_stocks_trees_non_feed <- data.frame("type" = "trees_non_feed") %>%
    mutate(biomass = sum(biomass[["trees_non_feed_biomass"]]$c_increase_soc),
           below_ground = biomass,
           annual_change_carbon_stocks = below_ground)

  total_annual_change_carbon_soils <- annual_change_carbon_stocks_soils$annual_change_carbon_stocks_soils+annual_change_carbon_stocks_trees_non_feed$annual_change_carbon_stocks

  total_change_co2_soils <- total_annual_change_carbon_soils*co2_conversion_factor

  results <- data.frame(total_annual_change_carbon_soils, total_change_co2_soils)

  return(results)

}
