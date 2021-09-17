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
#' economics <- economics_payback(mufindi, energy_required)
#' biomass <- biomass_calculation(mufindi, land_required)
#' soil_carbon <- soil_organic_carbon(para, land_required, biomass)
#' ghg_emissions <- ghg_emission(mufindi,energy_required,ghg_para,land_required,nitrogen_balance)
#' combineOutputs(feed_basket_quality,energy_required,land_required,soil_erosion,water_required,
#' nitrogen_balance,livestock_productivity,economics,biomass,soil_carbon,ghg_emissions)
#' calculate_differences(iDir)
#' clean_plotting(scenarios_all)
#' }
#'
#' @export
#'

clean_plotting <- function(scenarios_all){

  for(i in 2:ncol(scenarios_all)){

    oDir <- paste0(getwd(), "/outputs/", sep="")
    if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}

    datos <- scenarios_all %>% select(1, all_of(i))

    # titles
    tt <- colnames(datos[2])


    if(tt == "average_annual_milk_kg_yr"){
      title = "Average annual milk (kg_yr)"
    }else if(tt == "soil_mining_perc"){
      title = "Soil mining (%)"
    }else if (tt == "soil_leaching_perc"){
      title = "Soil leaching (%)"
    }else if (tt == "erosion_tyr"){
      title = "Erosion (t soil/yr)"
    }else if (tt == "erosion_thayr"){
      title = "Erosion (t soil/ha/yr)"
    }else if (tt == "erosion_kgsoil_kg_fpcm"){
      title = "Erosion (kg soil/ kg FPCM)"
    }else if (tt == "land_requirement_ha"){
      title = "Land required (ha/yr)"
    }else if (tt == "total_land_required_ha_mt_fpcm"){
      title = "Land required (ha/MT FPCM)"
    }else if (tt == "ghgtot_t_co2eq_yr"){
      title = "GHG (t CO2eq/ha)"
    }else if (tt == "ghgtot_t_co2eq_ha_yr"){
      title = "GHG (t CO2eq/ha/yr)"
    }else if (tt == "ghgmilk_kg_co2eq_kg"){
      title = "GHG (CO2eq/kg milk)"
    }else if (tt == "ghgmeat_kg_co2eq_kg"){
      title = "GHG (CO2eq/kg meat)"
    }else if (tt == "ghgprotein_kg_co2eq_kg"){
      title = "GHG (CO2eq/kg protein)"
    }else if (tt == "water_m3_yr"){
      title = "Water (m3/yr)"
    }else if (tt == "waterha_m3_ha"){
      title = "Water (m3/ha)"
    }else if (tt == "watermilk_m3_kg"){
      title = "Water (m3/kg milk)"
    }else if (tt == "waterprotein_m3_kg"){
      title = "Water (m3/kg protein)"
    }else{
      NA
    }

    ggplot(datos, aes_string("scenario", y = tt, fill = "scenario")) +
      geom_bar(stat = "identity",
               position = "dodge",
               width = 0.2,
               colour = "black") +
      ggtitle(label = title)

    ggsave(paste0(oDir, tt, ".png"), width = 150, height = 100, units = "mm")

  }

}
