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
#' #' data(mufindi)
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

        scenario <- paste0(sub(".*_ *(.*?) *_.*", "\\1", i))

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

        ghg_emission <- output[["ghg_emission"]] # to be fixed when Stephen is done with GHG

        ghgtot_t_co2eq_yr <- 0
        ghgtot_t_co2eq_ha_yr <- 0
        ghgmilk_kg_co2eq_kg <- 0
        ghgprotein_kg_co2eq_kg <- 0

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
                                        ghgprotein_kg_co2eq_kg,
                                        water_m3_yr,
                                        waterha_m3_ha,
                                        watermilk_m3_kg,
                                        waterprotein_m3_kg)
        }

      }

  scenarios_all <- scenarioList %>% dplyr::bind_rows()

  return(scenarios_all)

}


