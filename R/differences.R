library(jsonlite)

outputList <- list.files(getwd(), pattern = "output_", full.names = TRUE)

scenarioList <- list()

for (i in outputList){
  
  scenario <- paste0(sub(".*_ *(.*?) *_.*", "\\1", i))
  
  output <- fromJSON(paste0(i), flatten = TRUE)
  
  nitrogen_balance <- output[["nitrogen_balance"]]
  
  soil_mining_perc <- sum(nitrogen_balance[nitrogen_balance$area_total > 0.9,]$area_total)*100/sum(nitrogen_balance$area_total)
  
  soil_leaching_perc <- sum(nitrogen_balance[nitrogen_balance$area_total < 0.5,]$area_total)*100/sum(nitrogen_balance$area_total)
  
  erosion_tyr <- sum(as.numeric(output[["soil_erosion"]]$soil_loss_plot))
  
  land_required <- output[["land_required"]] %>% 
    bind_rows()
  
  erosion_thayr <- sum(land_required$area_feed)/erosion_tyr
  
  average_annual_milk_kg_yr <- sum(output[["livestock_productivity"]]$total_milk)
  
  erosion_kgsoil_kg_fpcm <- ifelse(is.na(erosion_tyr/sum(output[["livestock_productivity"]]$total_milk)), 0, erosion_tyr/sum(output[["livestock_productivity"]]$total_milk))*1000
  
  land_requirement_ha <- sum(land_required$area_feed)
  
  total_land_required_ha_mt_fpcm <- ifelse(is.na(land_requirement_ha/sum(output[["livestock_productivity"]]$total_milk)), 0, land_requirement_ha/sum(output[["livestock_productivity"]]$total_milk))*1000
  
  ghg_emission <- output[["ghg_emission"]] # Stephen to fix the next four variables
  
  ghgtot_t_co2eq_yr <- 0
  ghgtot_t_co2eq_ha_yr <- 0
  ghgmilk_kg_co2eq_kg_milk <- 0
  ghgprotein_kg_co2eq_kg_protein <- 0
  
  water_requirement <- output[["water_requirement"]] # not returning any data
  
  water_m3_yr <- 0
  waterha_m3_ha <- 0
  watermilk_m3_kg_milk <- 0
  waterprotein_m3_kg_protein <- 0
  
  scenarioList[[i]] <- data.frame(scenario,
                                  average_annual_milk_kg_yr,
                                  soil_mining_perc, 
                                  soil_leaching_perc, 
                                  erosion_tyr, 
                                  erosion_thayr, 
                                  erosion_kgsoil_kg_fpcm,
                                  land_requirement_ha,
                                  total_land_required_ha_mt_fpcm,
                                  ghgtot_t_co2eq_yr,
                                  ghgtot_t_co2eq_ha_yr,
                                  ghgmilk_kg_co2eq_kg_milk,
                                  ghgprotein_kg_co2eq_kg_protein,
                                  water_m3_yr,
                                  waterha_m3_ha,
                                  watermilk_m3_kg_milk,
                                  waterprotein_m3_kg_protein)
  
}

scenarios_all <- scenarioList %>% bind_rows()
