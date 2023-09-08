#' @title Plot results
#'
#' @description It plots all results
#'
#' @param outFile Path to the new comparison output json computed using the `differences` function
#'
#' @param oDir directory where the plots are to be saved
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' data(ghg_para)
#' feed_basket_quality <- feed_quality(para)
#' energy_required <- energy_requirement(para,feed_basket_quality)
#' land_required <- land_requirement(feed_basket_quality, energy_required, para)
#' soil_erosion <- soil_health(para, land_required)
#' water_required <- water_requirement(para,land_required)
#' nitrogen_balance <- n_balance(para, land_required, soil_erosion)
#' livestock_productivity <- land_productivity(para)
#' biomass <- biomass_calculation(para, land_required)
#' soil_carbon <- soil_organic_carbon(para, land_required, biomass)
#' ghg_emissions <- ghg_emission(para,energy_required,ghg_para,land_required,nitrogen_balance)
#' combineOutputs(para,feed_basket_quality,energy_required,land_required,soil_erosion,water_required,
#' nitrogen_balance,livestock_productivity,economics,biomass,soil_carbon,ghg_emissions,filePath)
#' calculate_differences(outFile,...)
#' clean_plotting(outFile,oDir)
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
      title = "Tropical Livestock Unit"
    }else if (tt == "total_land_requirement_ha"){
      title = "Land required (ha/yr)"
    }else if (tt == "total_land_requirement_ha_per_kg_fpcm"){
      title = "Land required (ha/kg FPCM)"
    }else if (tt == "total_land_requirement_ha_per_kg_meat"){
      title = "Land required (ha/kg meat)"
    }else if (tt == "total_land_requirement_ha_per_kg_protein"){
      title = "Land required (ha/kg protein)"
    }else if (tt == "total_land_requirement_ha_per_tlu"){
      title = "Land required (ha/TLU)"
    }else if (tt == "total_n_balance_kg_n_per_year"){
      title = "N balance (kg N/yr)"
    }else if (tt == "percent_area_mining"){
      title = "Soil mining (%)"
    }else if (tt == "percent_area_leaching"){
      title = "Soil leaching (%)"
    }else if (tt == "n_balance_kg_n_per_ha_per_year"){
      title = "N balance (kg N/ha/yr)"
    }else if (tt == "n_balance_kg_n_per_kg_fpcm"){
      title = "N balance (kg N/kg FPCM)"
    }else if (tt == "n_balance_kg_n_per_kg_meat"){
      title = "N balance (kg N/kg meat)"
    }else if (tt == "n_balance_kg_n_per_kg_protein"){
      title = "N balance (kg N/kg protein)"
    }else if (tt == "erosion_t_soil_year"){
      title = "Erosion (t soil/yr)"
    }else if (tt == "erosion_t_soil_per_ha_per_year"){
      title = "Erosion (t soil/ha/yr)"
    }else if (tt == "erosion_t_soil_per_kg_fpcm"){
      title = "Erosion (t soil/kg FPCM)"
    }else if (tt == "erosion_t_soil_per_kg_meat"){
      title = "Erosion (t soil/kg meat)"
    }else if (tt == "erosion_t_soil_per_kg_protein"){
      title = "Erosion (t soil/kg protein)"
    }else if (tt == "ghg_emission_t_co2_eq_per_year"){
      title = "GHG (t CO2eq/yr)"
    }else if (tt == "ghg_emission_t_co2_eq_per_ha_per_year"){
      title = "GHG (t CO2eq/ha/yr)"
    }else if (tt == "ghg_emission_t_co2_eq_per_kg_fpcm"){
      title = "GHG (CO2eq/kg FPCM)"
    }else if (tt == "ghg_emission_t_co2_eq_per_kg_meat"){
      title = "GHG (CO2eq/kg meat)"
    }else if (tt == "ghg_emission_t_co2_eq_per_kg_protein"){
      title = "GHG (CO2eq/kg protein)"
    }else if (tt == "percent_precipitation_used_for_feed_production"){
      title = "% Precipitation use for feed production"
    }else if (tt == "total_water_use_m3"){
      title = "Water use (m3/yr)"
    }else if (tt == "total_water_use_m3_per_ha"){
      title = "Water use (m3/ha)"
    }else if (tt == "total_water_use_m3_per_kg_fpcm"){
      title = "Water use (m3/kg FPCM)"
    }else if (tt == "total_water_use_m3_per_kg_meat"){
      title = "Water use (m3/kg meat)"
    }else if (tt == "total_water_use_m3_per_kg_protein"){
      title = "Water (m3/kg protein)"
    }else if (tt == "carbon_stock_change_t_co2eq_per_year"){
      title = "Carbon stock changes (t CO2eq/yr)"
    }else if (tt == "carbon_stock_change_t_co2eq_per_ha_per_year"){
      title = "Carbon stock changes (t CO2eq/ha/yr)"
    }else if (tt == "carbon_stock_change_t_co2eq_per_fpcm"){
      title = "Carbon stock changes (t CO2eq/kg FPCM)"
    }else if (tt == "carbon_stock_change_t_co2eq_per_meat"){
      title = "Carbon stock changes (t CO2eq/kg meat)"
    }else if (tt == "carbon_stock_change_t_co2eq_per_protein"){
      title = "Carbon stock changes (t CO2eq/kg protein)"
    }else if (tt == "total_milk_produced_energy_kcal_per_year"){
      title = "Energy (kcal/kg FPCM)"
    }else if (tt == "total_meat_produced_energy_kcal_per_year"){
      title = "Energy (kcal/kg meat)"
    }else if (tt == "total_milk_produced_ame_days_per_year"){
      title = "AME (days/kg FPCM)"
    }else if (tt == "total_meat_produced_ame_days_per_year"){
      title = "AME (days/kg meat)"
    }else if (tt == "total_carbon_balance_per_fpcm"){
      title = "Carbon balance (t CO2eq/kg FPCM)"
    }else if (tt == "total_carbon_balance_per_meat"){
      title = "Carbon balance (t CO2eq/kg meat)"
    }else if (tt == "total_carbon_balance_per_protein"){
      title = "Carbon balance (t CO2eq/kg protein)"
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
