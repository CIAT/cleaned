#' @title Calculate Differences
#'
#' @description It computes difference in environmental impact between scenarios
#'
#' @param iDir A path to a directory where output files are stored
#'
#' @return dataframe
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
#' }
#'
#' @export

calculate_differences <- function(iDir){

  outputList <- list.files(paste0(iDir), pattern = "output_", full.names = TRUE)

  if (length(outputList)==0) {

    stop("No files in source directory")

    } else {

      scenarioList <- list()

      for (i in outputList){

        scenario <- gsub(".*(output_|\\s)(.*).json", "\\2", i)

        output <- jsonlite::fromJSON(paste0(i), flatten = TRUE)

        nitrogen_balance <- output[["nitrogen_balance"]]

        soil_mining_perc <- sum(nitrogen_balance[nitrogen_balance$area_total > 0.9,]$area_total)*100/sum(nitrogen_balance$area_total)

        soil_leaching_perc <- sum(nitrogen_balance[nitrogen_balance$area_total < 0.5,]$area_total)*100/sum(nitrogen_balance$area_total)

        erosion_tyr <- sum(as.numeric(output[["soil_erosion"]]$soil_loss_plot))

        land_required <- output[["land_required"]] %>%
          dplyr::bind_rows()

        erosion_thayr <- sum(land_required$area_feed)/erosion_tyr

        average_annual_milk_kg_yr <- sum(output[["livestock_productivity"]]$total_milk)

        erosion_kgsoil_kg_fpcm <- ifelse(is.na(erosion_tyr/sum(output[["livestock_productivity"]]$total_milk)), 0, erosion_tyr/sum(output[["livestock_productivity"]]$total_milk))*1000

        land_requirement_ha <- sum(land_required$area_feed)

        total_land_required_ha_mt_fpcm <- ifelse(is.na(land_requirement_ha/sum(output[["livestock_productivity"]]$total_milk)), 0, land_requirement_ha/sum(output[["livestock_productivity"]]$total_milk))*1000

        ghg_emission <- output[["ghg_emission"]]

        ghgtot_t_co2eq_yr <- (sum(ghg_emission$kg_per_ha,na.rm = T)/1000)*land_requirement_ha
        ghgtot_t_co2eq_ha_yr <- sum(ghg_emission$kg_per_ha,na.rm = T)/1000
        ghgmeat_kg_co2eq_kg <- ifelse(is.na(ghgtot_t_co2eq_yr/sum(output[["livestock_productivity"]]$meat_production_animal)), 0, ghgtot_t_co2eq_yr/sum(output[["livestock_productivity"]]$meat_production_animal))
        ghgmilk_kg_co2eq_kg <- ifelse(is.na(ghgtot_t_co2eq_yr/sum(output[["livestock_productivity"]]$total_milk)), 0, ghgtot_t_co2eq_yr/sum(output[["livestock_productivity"]]$total_milk))
        tot_protein_kg_year_meat <- sum(output[["livestock_productivity"]]$protein_kg_year_meat)
        tot_protein_kg_year_milk <- sum(output[["livestock_productivity"]]$protein_kg_year_milk)
        ghgprotein_kg_co2eq_kg <- ifelse(tot_protein_kg_year_meat==0 & tot_protein_kg_year_milk == 0,0,
                                         ifelse(tot_protein_kg_year_meat==0 & tot_protein_kg_year_milk != 0, ghgtot_t_co2eq_yr/tot_protein_kg_year_milk,
                                                ifelse(tot_protein_kg_year_meat!=0 & tot_protein_kg_year_milk == 0,ghgtot_t_co2eq_yr/tot_protein_kg_year_meat,
                                                       (ghgtot_t_co2eq_yr/tot_protein_kg_year_milk)+(ghgtot_t_co2eq_yr/tot_protein_kg_year_meat))))

        water_requirement <- output[["water_required"]]

        water_m3_yr <- sum(land_required$area_feed)*sum(water_requirement$water_use_per_feed_item$ET)
        waterha_m3_ha <- water_m3_yr/sum(land_required$area_feed)
        watermilk_m3_kg <- ifelse(is.na(water_m3_yr/sum(output[["livestock_productivity"]]$total_milk)), 0, water_m3_yr/sum(output[["livestock_productivity"]]$total_milk))
        waterprotein_m3_kg <- ifelse(is.na(water_m3_yr/(sum(output[["livestock_productivity"]]$protein_kg_year_meat)+sum(output[["livestock_productivity"]]$protein_kg_year_milk))), 0, water_m3_yr/(sum(output[["livestock_productivity"]]$protein_kg_year_meat)+sum(output[["livestock_productivity"]]$protein_kg_year_milk)))

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
                                        ghgmilk_kg_co2eq_kg,
                                        ghgmeat_kg_co2eq_kg,
                                        ghgprotein_kg_co2eq_kg,
                                        water_m3_yr,
                                        waterha_m3_ha,
                                        watermilk_m3_kg,
                                        waterprotein_m3_kg)
        }

      }

  results <- scenarioList %>% dplyr::bind_rows()

  return(results)

}


