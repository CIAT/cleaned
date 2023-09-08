#' @title Plot results
#'
#' @description It plots all results
#'
#' @param scenarios_all A dataframe computed using the `differences` function
#'
#' @importFrom dplyr %>%
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
#' ghg_emissions <- ghg_emission(mufindi,energy_required,ghg_para,land_required,nitrogen_balance)
#' combineOutputs(feed_basket_quality,energy_required,land_required,soil_erosion,water_required,
#' nitrogen_balance,livestock_productivity,economics,biomass,soil_carbon,ghg_emissions)
#' calculate_differences(iDir)
#' clean_plotting(outFile)
#' }
#'
#' @export
#'

clean_plotting <- function(outFile,oDir){

  outFile <- jsonlite::fromJSON(outFile, flatten = TRUE)

  outFile$scenario <- as.factor(outFile$scenario)

  oDir <- oDir
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}

  for(i in 2:ncol(outFile)){

    datos <- outFile %>% select(1, all_of(i))

    # titles
    tt <- colnames(datos[2])


    if(tt == "total_milk_produced_kg_fpcm_per_year"){
      title = "Total milk FPCM (kg/yr)"
    }else if(tt == "total_meat_produced_kg_per_year"){
      title = "Total meat (kg/yr)"
    }else if(tt == "total_protein_produced_kg_per_year"){
      title = "Total protein (kg/yr)"
    }else if (tt == "total_tlu"){
      title = "Soil leaching (%)"
    }else if (tt == "total_land_requirement_ha"){
      title = "Erosion (t soil/yr)"
    }else if (tt == "total_land_requirement_ha_per_kg_fpcm"){
      title = "Erosion (t soil/ha/yr)"
    }else if (tt == "total_land_requirement_ha_per_kg_meat"){
      title = "Erosion (kg soil/ kg FPCM)"
    }else if (tt == "total_land_requirement_ha_per_kg_protein"){
      title = "Land required (ha/yr)"
    }else if (tt == "total_land_requirement_ha_per_tlu"){
      title = "Land required (ha/MT FPCM)"
    }else if (tt == "total_n_balance_kg_n_per_year"){
      title = "GHG (t CO2eq/ha)"
    }else if (tt == "percent_area_mining"){
      title = "GHG (t CO2eq/ha/yr)"
    }else if (tt == "percent_area_leaching"){
      title = "GHG (CO2eq/kg milk)"
    }else if (tt == "n_balance_kg_n_per_ha_per_year"){
      title = "GHG (CO2eq/kg meat)"
    }else if (tt == "n_balance_kg_n_per_kg_fpcm"){
      title = "GHG (CO2eq/kg protein)"
    }else if (tt == "n_balance_kg_n_per_kg_meat"){
      title = "% Precipitation use for feed production"
    }else if (tt == "n_balance_kg_n_per_kg_protein"){
      title = "Water (m3/yr)"
    }else if (tt == "erosion_t_soil_year"){
      title = "Water (m3/ha)"
    }else if (tt == "erosion_t_soil_per_ha_per_year"){
      title = "Water (m3/kg FPCM)"
    }else if (tt == "erosion_t_soil_per_kg_fpcm"){
      title = "Water (m3/kg meat)"
    }else if (tt == "erosion_t_soil_per_kg_meat"){
      title = "Water (m3/kg meat)"
    }else if (tt == "erosion_t_soil_per_kg_protein"){
      title = "Water (m3/kg meat)"
    }else if (tt == "ghg_emission_t_co2_eq_per_year"){
      title = "Water (m3/kg meat)"
    }else if (tt == "ghg_emission_t_co2_eq_per_ha_per_year"){
      title = "Water (m3/kg meat)"
    }else if (tt == "ghg_emission_t_co2_eq_per_kg_meat"){
      title = "Water (m3/kg meat)"
    }else if (tt == "ghg_emission_t_co2_eq_per_kg_protein"){
      title = "Water (m3/kg meat)"
    }else if (tt == "percent_precipitation_used_for_feed_production"){
      title = "Water (m3/kg meat)"
    }else if (tt == "total_water_use_m3"){
      title = "Water (m3/kg meat)"
    }else if (tt == "total_water_use_m3_per_ha"){
      title = "Water (m3/kg meat)"
    }else if (tt == "total_water_use_m3_per_kg_fpcm"){
      title = "Water (m3/kg meat)"
    }else if (tt == "total_water_use_m3_per_kg_meat"){
      title = "Water (m3/kg meat)"
    }else if (tt == "total_water_use_m3_per_kg_protein"){
      title = "Water (m3/kg meat)"
    }else if (tt == "carbon_stock_change_t_co2eq_per_year"){
      title = "Water (m3/kg meat)"
    }else if (tt == "carbon_stock_change_t_co2eq_per_ha_per_year"){
      title = "Water (m3/kg meat)"
    }else if (tt == "carbon_stock_change_t_co2eq_per_fpcm"){
      title = "Water (m3/kg meat)"
    }else if (tt == "carbon_stock_change_t_co2eq_per_meat"){
      title = "Water (m3/kg meat)"
    }else if (tt == "carbon_stock_change_t_co2eq_per_protein"){
      title = "Water (m3/kg meat)"
    }else if (tt == "total_milk_produced_energy_kcal_per_year"){
      title = "Water (m3/kg meat)"
    }else if (tt == "total_meat_produced_energy_kcal_per_year"){
      title = "Water (m3/kg meat)"
    }else if (tt == "total_milk_produced_ame_days_per_year"){
      title = "Water (m3/kg meat)"
    }else if (tt == "total_meat_produced_ame_days_per_year"){
      title = "Water (m3/kg meat)"
    }else if (tt == "total_carbon_balance_per_fpcm"){
      title = "Water (m3/kg meat)"
    }else if (tt == "total_carbon_balance_per_meat"){
      title = "Water (m3/kg meat)"
    }else if (tt == "total_carbon_balance_per_protein"){
      title = "Water (m3/kg protein)"
    }else{
      NA
    }

    ggplot(datos, aes_string("scenario", y = tt, fill = "scenario")) +
      geom_bar(stat = "identity",
               position = "dodge",
               width = 0.2) +
      geom_text(aes(label = round(datos[,2],2)), position = position_dodge(width = 0.2), vjust = -0.5, size = 2) +
      labs(x = "", y = tt, title = title, fill = "Scenario") +
      theme_bw()

    ggsave(paste0(oDir, tt, ".png"), width = 150, height = 100, units = "mm")

  }

}
