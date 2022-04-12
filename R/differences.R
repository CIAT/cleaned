#' @title Calculate Differences
#'
#' @description It computes difference in environmental impact between scenarios
#'
#' @param outFile Path to the new comparison output json
#'
#' @param ... Paths of the different scenario outputs
#'
#' @return json file
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
#' calculate_differences(...)
#' }
#'
#' @export

calculate_differences <- function(outFile,...){

  outputList <- list(outFile = outFile,...)

  output_path <- outputList[["outFile"]]

  outputList[[1]] <- NULL

  if (length(outputList) == 0) {

    stop("No files in source directory")

    } else {

      scenarioList <- list()

      for (i in 1:length(outputList)){

        scenario <- i

        output <- jsonlite::fromJSON(outputList[[i]], flatten = TRUE)

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
        water_use_for_production <- as.data.frame(water_requirement[["water_use_for_production"]])

        percent_of_precipitation_used_for_feed_production <- water_use_for_production[which(water_use_for_production$Names=="fraction_of_precipitation_used_for_feed_production"),2]*100
        water_m3_yr <-water_use_for_production[which(water_use_for_production$Names=="total_water_use"),2]
        waterha_m3_ha <- water_m3_yr/sum(land_required$area_feed)
        water_use_perkg_fpcm <- water_use_for_production[which(water_use_for_production$Names=="water_use_fpcm"),2]
        water_use_perkg_meat <- water_use_for_production[which(water_use_for_production$Names=="water_use_meat"),2]
        water_use_perkg_protein <- water_use_for_production[which(water_use_for_production$Names=="water_use_protein"),2]


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
                                        percent_of_precipitation_used_for_feed_production,
                                        water_m3_yr,
                                        waterha_m3_ha,
                                        water_use_perkg_fpcm,
                                        water_use_perkg_meat,
                                        water_use_perkg_protein)
        }

      }

  results <- scenarioList %>% dplyr::bind_rows()

  write(jsonlite::toJSON(results, pretty = TRUE),output_path)

}
