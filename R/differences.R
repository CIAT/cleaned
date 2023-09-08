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
#' biomass <- biomass_calculation(mufindi, land_required)
#' soil_carbon <- soil_organic_carbon(para, land_required, biomass)
#' ghg_emissions <- ghg_emission(mufindi,energy_required,ghg_para,land_required,nitrogen_balance)
#' combineOutputs(feed_basket_quality,energy_required,land_required,soil_erosion,water_required,
#' nitrogen_balance,livestock_productivity,biomass,soil_carbon,ghg_emissions)
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

        scenario <- sub("\\.\\w+$", "", basename(outputList[[i]]))

        output <- jsonlite::fromJSON(outputList[[i]], flatten = TRUE)

        # Productivity
        total_milk_produced_kg_fpcm_per_year <- as.numeric(output[["livestock_productivity"]][["consumable_livestock_product"]][output[["livestock_productivity"]][["consumable_livestock_product"]]$produced_item == "Milk (FPCM)","production_kg_per_year"][3])
        total_meat_produced_kg_per_year <- as.numeric(output[["livestock_productivity"]][["consumable_livestock_product"]][output[["livestock_productivity"]][["consumable_livestock_product"]]$produced_item == "Meat","production_kg_per_year"][3])
        total_protein_produced_kg_per_year <- as.numeric(output[["livestock_productivity"]][["consumable_livestock_product"]][output[["livestock_productivity"]][["consumable_livestock_product"]]$produced_item == "Milk (FPCM)","protein_kg_per_year"][3]) +
          as.numeric(output[["livestock_productivity"]][["consumable_livestock_product"]][output[["livestock_productivity"]][["consumable_livestock_product"]]$produced_item == "Meat","protein_kg_per_year"][3])
        total_tlu <- sum(output[["livestock_productivity"]][["manure_produced"]]$tlu, na.rm = T)

        # Land requirement
        total_land_requirement_ha <- as.numeric(output[["land_required"]][["land_and_dm_required"]][output[["land_required"]][["land_and_dm_required"]]$Names == "total_area_used_for_feed_production_ha","Value"])
        total_land_requirement_ha_per_kg_fpcm <- ifelse(!is.finite(total_land_requirement_ha/total_milk_produced_kg_fpcm_per_year),0,total_land_requirement_ha/total_milk_produced_kg_fpcm_per_year)*1000
        total_land_requirement_ha_per_kg_meat <- ifelse(!is.finite(total_land_requirement_ha/total_meat_produced_kg_per_year),0,total_land_requirement_ha/total_meat_produced_kg_per_year)*1000
        total_land_requirement_ha_per_kg_protein <- ifelse(!is.finite(total_land_requirement_ha/total_protein_produced_kg_per_year),0,total_land_requirement_ha/total_protein_produced_kg_per_year)*1000
        total_land_requirement_ha_per_tlu <- ifelse(!is.finite(total_land_requirement_ha/total_tlu),0,total_land_requirement_ha/total_tlu)

        # N balance
        total_n_balance_kg_n_per_year <- output[["soil_impacts"]][["overal_soil_impact"]][output[["soil_impacts"]][["overal_soil_impact"]]$sources == "total", "balance_N_kg_N_year"]
        percent_area_mining <- output[["soil_impacts"]][["overal_soil_impact"]][output[["soil_impacts"]][["overal_soil_impact"]]$sources == "total", "percent_area_mining"]
        percent_area_leaching <- output[["soil_impacts"]][["overal_soil_impact"]][output[["soil_impacts"]][["overal_soil_impact"]]$sources == "total", "percent_area_leaching"]
        n_balance_kg_n_per_ha_per_year <- ifelse(!is.finite(total_n_balance_kg_n_per_year/total_land_requirement_ha),0,total_n_balance_kg_n_per_year/total_land_requirement_ha)
        n_balance_kg_n_per_kg_fpcm <- ifelse(!is.finite(total_n_balance_kg_n_per_year/total_milk_produced_kg_fpcm_per_year),0,total_n_balance_kg_n_per_year/total_milk_produced_kg_fpcm_per_year)
        n_balance_kg_n_per_kg_meat <- ifelse(!is.finite(total_n_balance_kg_n_per_year/total_meat_produced_kg_per_year),0,total_n_balance_kg_n_per_year/total_meat_produced_kg_per_year)
        n_balance_kg_n_per_kg_protein <- ifelse(!is.finite(total_n_balance_kg_n_per_year/total_protein_produced_kg_per_year),0,total_n_balance_kg_n_per_year/total_protein_produced_kg_per_year)

        # Soil Erosion
        erosion_t_soil_year <- output[["soil_impacts"]][["overal_soil_impact"]][output[["soil_impacts"]][["overal_soil_impact"]]$sources == "total", "erosion_t_soil_year"]
        erosion_t_soil_per_ha_per_year <- ifelse(!is.finite(erosion_t_soil_year/total_land_requirement_ha),0,erosion_t_soil_year/total_land_requirement_ha)
        erosion_kgsoil_per_kg_fpcm <- ifelse(!is.finite(erosion_t_soil_year/total_milk_produced_kg_fpcm_per_year), 0, erosion_t_soil_year/total_milk_produced_kg_fpcm_per_year)*1000
        erosion_kgsoil_per_kg_meat <- ifelse(!is.finite(erosion_t_soil_year/total_meat_produced_kg_per_year), 0, erosion_t_soil_year/total_meat_produced_kg_per_year)*1000
        erosion_kgsoil_per_kg_protein <- ifelse(!is.finite(erosion_t_soil_year/total_protein_produced_kg_per_year), 0, erosion_t_soil_year/total_protein_produced_kg_per_year)*1000

        # GHG emission
        ghg_emission_t_co2_eq_per_year <- sum(as.numeric(output[["ghg_emission"]][["ghg_balance"]]$value),na.rm = T)
        ghg_emission_t_co2_eq_per_ha_per_year <- ifelse(!is.finite(ghg_emission_t_co2_eq_per_year/total_land_requirement_ha),0,ghg_emission_t_co2_eq_per_year/total_land_requirement_ha)
        ghg_emission_t_co2_eq_per_kg_fpcm <- ifelse(!is.finite(ghg_emission_t_co2_eq_per_year/total_milk_produced_kg_fpcm_per_year),0,ghg_emission_t_co2_eq_per_year/total_milk_produced_kg_fpcm_per_year)*1000
        ghg_emission_t_co2_eq_per_kg_meat <- ifelse(!is.finite(ghg_emission_t_co2_eq_per_year/total_meat_produced_kg_per_year),0,ghg_emission_t_co2_eq_per_year/total_meat_produced_kg_per_year)*1000
        ghg_emission_t_co2_eq_per_kg_protein <- ifelse(!is.finite(ghg_emission_t_co2_eq_per_year/total_protein_produced_kg_per_year),0,ghg_emission_t_co2_eq_per_year/total_protein_produced_kg_per_year)*1000

        # Water impacts
        percent_precipitation_used_for_feed_production <- output[["water_required"]][["water_use_for_production"]][output[["water_required"]][["water_use_for_production"]]$Names == "fraction_of_precipitation_used_for_feed_production", "Value"]*100
        total_water_use_m3 <- output[["water_required"]][["water_use_for_production"]][output[["water_required"]][["water_use_for_production"]]$Names == "total_water_use", "Value"]
        total_water_use_m3_per_ha <- ifelse(!is.finite(total_water_use_m3/total_land_requirement_ha), 0, total_water_use_m3/total_land_requirement_ha)
        total_water_use_m3_per_kg_fpcm <- output[["water_required"]][["water_use_for_production"]][output[["water_required"]][["water_use_for_production"]]$Names == "water_use_fpcm", "Value"]
        total_water_use_m3_per_kg_meat <- output[["water_required"]][["water_use_for_production"]][output[["water_required"]][["water_use_for_production"]]$Names == "water_use_meat", "Value"]
        total_water_use_m3_per_kg_protein <- output[["water_required"]][["water_use_for_production"]][output[["water_required"]][["water_use_for_production"]]$Names == "water_use_protein", "Value"]

        # Carbon stock changes
        carbon_stock_change_t_co2eq_per_year <- sum(c(output[["soil_carbon"]]$total_change_co2_soils, output[["biomass"]]$co2_increase), na.rm = T)
        carbon_stock_change_t_co2eq_per_ha_per_year <- carbon_stock_change_t_co2eq_per_year/total_land_requirement_ha
        carbon_stock_change_t_co2eq_per_fpcm <- ifelse(!is.finite(carbon_stock_change_t_co2eq_per_year/total_milk_produced_kg_fpcm_per_year),0, carbon_stock_change_t_co2eq_per_year/total_milk_produced_kg_fpcm_per_year)*1000
        carbon_stock_change_t_co2eq_per_meat <- ifelse(!is.finite(carbon_stock_change_t_co2eq_per_year/total_land_requirement_ha_per_kg_meat),0, carbon_stock_change_t_co2eq_per_year/total_land_requirement_ha_per_kg_meat)*1000
        carbon_stock_change_t_co2eq_per_protein <- ifelse(!is.finite(carbon_stock_change_t_co2eq_per_year/total_protein_produced_kg_per_year),0, carbon_stock_change_t_co2eq_per_year/total_protein_produced_kg_per_year)*1000


        # Productivity / Energy
        total_milk_produced_energy_kcal_per_year <- as.numeric(output[["livestock_productivity"]][["consumable_livestock_product"]][output[["livestock_productivity"]][["consumable_livestock_product"]]$produced_item == "Milk (FPCM)","production_energy_kcal_per_year"][3])
        total_meat_produced_energy_kcal_per_year <- as.numeric(output[["livestock_productivity"]][["consumable_livestock_product"]][output[["livestock_productivity"]][["consumable_livestock_product"]]$produced_item == "Meat","production_energy_kcal_per_year"][3])

        total_milk_produced_ame_days_per_year <- as.numeric(output[["livestock_productivity"]][["consumable_livestock_product"]][output[["livestock_productivity"]][["consumable_livestock_product"]]$produced_item == "Milk (FPCM)","ame_days"][3])
        total_meat_produced_ame_days_per_year <- as.numeric(output[["livestock_productivity"]][["consumable_livestock_product"]][output[["livestock_productivity"]][["consumable_livestock_product"]]$produced_item == "Meat","ame_days"][3])

        #Total Carbon balance
        total_carbon_balance_per_fpcm <- ghg_emission_t_co2_eq_per_kg_fpcm-carbon_stock_change_t_co2eq_per_fpcm
        total_carbon_balance_per_meat <- ghg_emission_t_co2_eq_per_kg_meat-carbon_stock_change_t_co2eq_per_meat
        total_carbon_balance_per_protein <- ghg_emission_t_co2_eq_per_kg_protein-carbon_stock_change_t_co2eq_per_protein

        scenarioList[[i]] <- data.frame(scenario,
                                        total_milk_produced_kg_fpcm_per_year,
                                        total_meat_produced_kg_per_year,
                                        total_protein_produced_kg_per_year,
                                        total_tlu,
                                        total_land_requirement_ha,
                                        total_land_requirement_ha_per_kg_fpcm,
                                        total_land_requirement_ha_per_kg_meat,
                                        total_land_requirement_ha_per_kg_protein,
                                        total_land_requirement_ha_per_tlu,
                                        total_n_balance_kg_n_per_year,
                                        percent_area_mining,
                                        percent_area_leaching,
                                        n_balance_kg_n_per_ha_per_year,
                                        n_balance_kg_n_per_kg_fpcm,
                                        n_balance_kg_n_per_kg_meat,
                                        n_balance_kg_n_per_kg_protein,
                                        erosion_t_soil_year,
                                        erosion_t_soil_per_ha_per_year,
                                        erosion_t_soil_per_kg_fpcm,
                                        erosion_t_soil_per_kg_meat,
                                        erosion_t_soil_per_kg_protein,
                                        ghg_emission_t_co2_eq_per_year,
                                        ghg_emission_t_co2_eq_per_ha_per_year,
                                        ghg_emission_t_co2_eq_per_kg_meat,
                                        ghg_emission_t_co2_eq_per_kg_protein,
                                        percent_precipitation_used_for_feed_production,
                                        total_water_use_m3,
                                        total_water_use_m3_per_ha,
                                        total_water_use_m3_per_kg_fpcm,
                                        total_water_use_m3_per_kg_meat,
                                        total_water_use_m3_per_kg_protein,
                                        carbon_stock_change_t_co2eq_per_year,
                                        carbon_stock_change_t_co2eq_per_ha_per_year,
                                        carbon_stock_change_t_co2eq_per_fpcm,
                                        carbon_stock_change_t_co2eq_per_meat,
                                        carbon_stock_change_t_co2eq_per_protein,
                                        total_milk_produced_energy_kcal_per_year,
                                        total_meat_produced_energy_kcal_per_year,
                                        total_milk_produced_ame_days_per_year,
                                        total_meat_produced_ame_days_per_year,
                                        total_carbon_balance_per_fpcm,
                                        total_carbon_balance_per_meat,
                                        total_carbon_balance_per_protein)
        }

      }

  results <- scenarioList %>% dplyr::bind_rows()

  write(jsonlite::toJSON(results, pretty = TRUE),output_path)

}
