# plotting differences

library(ggplot2)

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
