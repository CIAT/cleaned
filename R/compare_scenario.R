#' @title Compare Scenarios
#'
#' @description It plots the change in scenarios from the base run
#'
#' @param baseRun name given to the base run
#'
#' @param outFile Path to the comparison json computed using the `differences` function
#'
#' @param oDir2 directory where the plots are to be saved
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
#' compare_scenario(baseRun, outFile, oDir2)
#' }
#'
#' @export
#'
compare_scenario <- function (baseRun, outFile, oDir2)
{
  base_run <- baseRun
  outFile <- jsonlite::fromJSON(outFile, flatten = TRUE)
  oDir2 <- oDir2
  base_run_df <- outFile[outFile$scenario == base_run, ]
  scenarios_df <- outFile[outFile$scenario != base_run, ]
  scenarioList <- list()

  output_list <- list()

  for (i in 1:nrow(scenarios_df)) {
    scenario_df <- scenarios_df[i, ]
    scenario <- scenario_df$scenario
    total_milk_produced_kg_fpcm_per_year <- ifelse(!is.finite(((scenario_df$total_milk_produced_kg_fpcm_per_year -
                                                                  base_run_df$total_milk_produced_kg_fpcm_per_year)/base_run_df$total_milk_produced_kg_fpcm_per_year) *
                                                                100), 0, ((scenario_df$total_milk_produced_kg_fpcm_per_year -
                                                                             base_run_df$total_milk_produced_kg_fpcm_per_year)/base_run_df$total_milk_produced_kg_fpcm_per_year) *
                                                     100)
    total_meat_produced_kg_per_year <- ifelse(!is.finite(((scenario_df$total_meat_produced_kg_per_year -
                                                             base_run_df$total_meat_produced_kg_per_year)/base_run_df$total_meat_produced_kg_per_year) *
                                                           100), 0, ((scenario_df$total_meat_produced_kg_per_year -
                                                                        base_run_df$total_meat_produced_kg_per_year)/base_run_df$total_meat_produced_kg_per_year) *
                                                100)
    total_protein_produced_kg_per_year <- ifelse(!is.finite(((scenario_df$total_protein_produced_kg_per_year -
                                                                base_run_df$total_protein_produced_kg_per_year)/base_run_df$total_protein_produced_kg_per_year) *
                                                              100), 0, ((scenario_df$total_protein_produced_kg_per_year -
                                                                           base_run_df$total_protein_produced_kg_per_year)/base_run_df$total_protein_produced_kg_per_year) *
                                                   100)
    total_tlu <- ifelse(!is.finite(((scenario_df$total_tlu -
                                       base_run_df$total_tlu)/base_run_df$total_tlu) *
                                     100), 0, ((scenario_df$total_tlu - base_run_df$total_tlu)/base_run_df$total_tlu) *
                          100)
    total_land_requirement_ha <- ifelse(!is.finite(((scenario_df$total_land_requirement_ha -
                                                       base_run_df$total_land_requirement_ha)/base_run_df$total_land_requirement_ha) *
                                                     100), 0, ((scenario_df$total_land_requirement_ha -
                                                                  base_run_df$total_land_requirement_ha)/base_run_df$total_land_requirement_ha) *
                                          100)
    total_land_requirement_ha_per_kg_fpcm <- ifelse(!is.finite(((scenario_df$total_land_requirement_ha_per_kg_fpcm -
                                                                   base_run_df$total_land_requirement_ha_per_kg_fpcm)/base_run_df$total_land_requirement_ha_per_kg_fpcm) *
                                                                 100), 0, ((scenario_df$total_land_requirement_ha_per_kg_fpcm -
                                                                              base_run_df$total_land_requirement_ha_per_kg_fpcm)/base_run_df$total_land_requirement_ha_per_kg_fpcm) *
                                                      100)
    total_land_requirement_ha_per_kg_meat <- ifelse(!is.finite(((scenario_df$total_land_requirement_ha_per_kg_meat -
                                                                   base_run_df$total_land_requirement_ha_per_kg_meat)/base_run_df$total_land_requirement_ha_per_kg_meat) *
                                                                 100), 0, ((scenario_df$total_land_requirement_ha_per_kg_meat -
                                                                              base_run_df$total_land_requirement_ha_per_kg_meat)/base_run_df$total_land_requirement_ha_per_kg_meat) *
                                                      100)
    total_land_requirement_ha_per_kg_protein <- ifelse(!is.finite(((scenario_df$total_land_requirement_ha_per_kg_protein -
                                                                      base_run_df$total_land_requirement_ha_per_kg_protein)/base_run_df$total_land_requirement_ha_per_kg_protein) *
                                                                    100), 0, ((scenario_df$total_land_requirement_ha_per_kg_protein -
                                                                                 base_run_df$total_land_requirement_ha_per_kg_protein)/base_run_df$total_land_requirement_ha_per_kg_protein) *
                                                         100)
    total_land_requirement_ha_per_tlu <- ifelse(!is.finite(((scenario_df$total_land_requirement_ha_per_tlu -
                                                               base_run_df$total_land_requirement_ha_per_tlu)/base_run_df$total_land_requirement_ha_per_tlu) *
                                                             100), 0, ((scenario_df$total_land_requirement_ha_per_tlu -
                                                                          base_run_df$total_land_requirement_ha_per_tlu)/base_run_df$total_land_requirement_ha_per_tlu) *
                                                  100)
    total_n_balance_kg_n_per_year <- ifelse(!is.finite(((scenario_df$total_n_balance_kg_n_per_year -
                                                           base_run_df$total_n_balance_kg_n_per_year)/base_run_df$total_n_balance_kg_n_per_year) *
                                                         100), 0, ((scenario_df$total_n_balance_kg_n_per_year -
                                                                      base_run_df$total_n_balance_kg_n_per_year)/base_run_df$total_n_balance_kg_n_per_year) *
                                              100)
    percent_area_mining <- ifelse(!is.finite(((scenario_df$percent_area_mining -
                                                 base_run_df$percent_area_mining)/base_run_df$percent_area_mining) *
                                               100), 0, ((scenario_df$percent_area_mining - base_run_df$percent_area_mining)/base_run_df$percent_area_mining) *
                                    100)
    percent_area_leaching <- ifelse(!is.finite(((scenario_df$percent_area_leaching -
                                                   base_run_df$percent_area_leaching)/base_run_df$percent_area_leaching) *
                                                 100), 0, ((scenario_df$percent_area_leaching - base_run_df$percent_area_leaching)/base_run_df$percent_area_leaching) *
                                      100)
    n_balance_kg_n_per_ha_per_year <- ifelse(!is.finite(((scenario_df$n_balance_kg_n_per_ha_per_year -
                                                            base_run_df$n_balance_kg_n_per_ha_per_year)/base_run_df$n_balance_kg_n_per_ha_per_year) *
                                                          100), 0, ((scenario_df$n_balance_kg_n_per_ha_per_year -
                                                                       base_run_df$n_balance_kg_n_per_ha_per_year)/base_run_df$n_balance_kg_n_per_ha_per_year) *
                                               100)
    n_balance_kg_n_per_kg_fpcm <- ifelse(!is.finite(((scenario_df$n_balance_kg_n_per_kg_fpcm -
                                                        base_run_df$n_balance_kg_n_per_kg_fpcm)/base_run_df$n_balance_kg_n_per_kg_fpcm) *
                                                      100), 0, ((scenario_df$n_balance_kg_n_per_kg_fpcm -
                                                                   base_run_df$n_balance_kg_n_per_kg_fpcm)/base_run_df$n_balance_kg_n_per_kg_fpcm) *
                                           100)
    n_balance_kg_n_per_kg_meat <- ifelse(!is.finite(((scenario_df$n_balance_kg_n_per_kg_meat -
                                                        base_run_df$n_balance_kg_n_per_kg_meat)/base_run_df$n_balance_kg_n_per_kg_meat) *
                                                      100), 0, ((scenario_df$n_balance_kg_n_per_kg_meat -
                                                                   base_run_df$n_balance_kg_n_per_kg_meat)/base_run_df$n_balance_kg_n_per_kg_meat) *
                                           100)
    n_balance_kg_n_per_kg_protein <- ifelse(!is.finite(((scenario_df$n_balance_kg_n_per_kg_protein -
                                                           base_run_df$n_balance_kg_n_per_kg_protein)/base_run_df$n_balance_kg_n_per_kg_protein) *
                                                         100), 0, ((scenario_df$n_balance_kg_n_per_kg_protein -
                                                                      base_run_df$n_balance_kg_n_per_kg_protein)/base_run_df$n_balance_kg_n_per_kg_protein) *
                                              100)
    erosion_t_soil_year <- ifelse(!is.finite(((scenario_df$erosion_t_soil_year -
                                                 base_run_df$erosion_t_soil_year)/base_run_df$erosion_t_soil_year) *
                                               100), 0, ((scenario_df$erosion_t_soil_year - base_run_df$erosion_t_soil_year)/base_run_df$erosion_t_soil_year) *
                                    100)
    erosion_t_soil_per_ha_per_year <- ifelse(!is.finite(((scenario_df$erosion_t_soil_per_ha_per_year -
                                                            base_run_df$erosion_t_soil_per_ha_per_year)/base_run_df$erosion_t_soil_per_ha_per_year) *
                                                          100), 0, ((scenario_df$erosion_t_soil_per_ha_per_year -
                                                                       base_run_df$erosion_t_soil_per_ha_per_year)/base_run_df$erosion_t_soil_per_ha_per_year) *
                                               100)
    erosion_kgsoil_per_kg_fpcm <- ifelse(!is.finite(((scenario_df$erosion_kgsoil_per_kg_fpcm -
                                                        base_run_df$erosion_kgsoil_per_kg_fpcm)/base_run_df$erosion_kgsoil_per_kg_fpcm) *
                                                      100), 0, ((scenario_df$erosion_kgsoil_per_kg_fpcm -
                                                                   base_run_df$erosion_kgsoil_per_kg_fpcm)/base_run_df$erosion_kgsoil_per_kg_fpcm) *
                                           100)
    erosion_kgsoil_per_kg_meat <- ifelse(!is.finite(((scenario_df$erosion_kgsoil_per_kg_meat -
                                                        base_run_df$erosion_kgsoil_per_kg_meat)/base_run_df$erosion_kgsoil_per_kg_meat) *
                                                      100), 0, ((scenario_df$erosion_kgsoil_per_kg_meat -
                                                                   base_run_df$erosion_kgsoil_per_kg_meat)/base_run_df$erosion_kgsoil_per_kg_meat) *
                                           100)
    erosion_kgsoil_per_kg_protein <- ifelse(!is.finite(((scenario_df$erosion_kgsoil_per_kg_protein -
                                                           base_run_df$erosion_kgsoil_per_kg_protein)/base_run_df$erosion_kgsoil_per_kg_protein) *
                                                         100), 0, ((scenario_df$erosion_kgsoil_per_kg_protein -
                                                                      base_run_df$erosion_kgsoil_per_kg_protein)/base_run_df$erosion_kgsoil_per_kg_protein) *
                                              100)
    ghg_emission_t_co2_eq_per_year <- ifelse(!is.finite(((scenario_df$ghg_emission_t_co2_eq_per_year -
                                                            base_run_df$ghg_emission_t_co2_eq_per_year)/base_run_df$ghg_emission_t_co2_eq_per_year) *
                                                          100), 0, ((scenario_df$ghg_emission_t_co2_eq_per_year -
                                                                       base_run_df$ghg_emission_t_co2_eq_per_year)/base_run_df$ghg_emission_t_co2_eq_per_year) *
                                               100)
    ghg_emission_t_co2_eq_per_ha_per_year <- ifelse(!is.finite(((scenario_df$ghg_emission_t_co2_eq_per_ha_per_year -
                                                                   base_run_df$ghg_emission_t_co2_eq_per_ha_per_year)/base_run_df$ghg_emission_t_co2_eq_per_ha_per_year) *
                                                                 100), 0, ((scenario_df$ghg_emission_t_co2_eq_per_ha_per_year -
                                                                              base_run_df$ghg_emission_t_co2_eq_per_ha_per_year)/base_run_df$ghg_emission_t_co2_eq_per_ha_per_year) *
                                                      100)
    ghg_emission_t_co2_eq_per_kg_fpcm <- ifelse(!is.finite(((scenario_df$ghg_emission_t_co2_eq_per_kg_fpcm -
                                                               base_run_df$ghg_emission_t_co2_eq_per_kg_fpcm)/base_run_df$ghg_emission_t_co2_eq_per_kg_fpcm) *
                                                             100), 0, ((scenario_df$ghg_emission_t_co2_eq_per_kg_fpcm -
                                                                          base_run_df$ghg_emission_t_co2_eq_per_kg_fpcm)/base_run_df$ghg_emission_t_co2_eq_per_kg_fpcm) *
                                                  100)
    ghg_emission_t_co2_eq_per_kg_meat <- ifelse(!is.finite(((scenario_df$ghg_emission_t_co2_eq_per_kg_meat -
                                                               base_run_df$ghg_emission_t_co2_eq_per_kg_meat)/base_run_df$ghg_emission_t_co2_eq_per_kg_meat) *
                                                             100), 0, ((scenario_df$ghg_emission_t_co2_eq_per_kg_meat -
                                                                          base_run_df$ghg_emission_t_co2_eq_per_kg_meat)/base_run_df$ghg_emission_t_co2_eq_per_kg_meat) *
                                                  100)
    ghg_emission_t_co2_eq_per_kg_protein <- ifelse(!is.finite(((scenario_df$ghg_emission_t_co2_eq_per_kg_protein -
                                                                  base_run_df$ghg_emission_t_co2_eq_per_kg_protein)/base_run_df$ghg_emission_t_co2_eq_per_kg_protein) *
                                                                100), 0, ((scenario_df$ghg_emission_t_co2_eq_per_kg_protein -
                                                                             base_run_df$ghg_emission_t_co2_eq_per_kg_protein)/base_run_df$ghg_emission_t_co2_eq_per_kg_protein) *
                                                     100)
    percent_precipitation_used_for_feed_production <- ifelse(!is.finite(((scenario_df$percent_precipitation_used_for_feed_production -
                                                                            base_run_df$percent_precipitation_used_for_feed_production)/base_run_df$percent_precipitation_used_for_feed_production) *
                                                                          100), 0, ((scenario_df$percent_precipitation_used_for_feed_production -
                                                                                       base_run_df$percent_precipitation_used_for_feed_production)/base_run_df$percent_precipitation_used_for_feed_production) *
                                                               100)
    total_water_use_m3 <- ifelse(!is.finite(((scenario_df$total_water_use_m3 -
                                                base_run_df$total_water_use_m3)/base_run_df$total_water_use_m3) *
                                              100), 0, ((scenario_df$total_water_use_m3 - base_run_df$total_water_use_m3)/base_run_df$total_water_use_m3) *
                                   100)
    total_water_use_m3_per_ha <- ifelse(!is.finite(((scenario_df$total_water_use_m3_per_ha -
                                                       base_run_df$total_water_use_m3_per_ha)/base_run_df$total_water_use_m3_per_ha) *
                                                     100), 0, ((scenario_df$total_water_use_m3_per_ha -
                                                                  base_run_df$total_water_use_m3_per_ha)/base_run_df$total_water_use_m3_per_ha) *
                                          100)
    total_water_use_m3_per_kg_fpcm <- ifelse(!is.finite(((scenario_df$total_water_use_m3_per_kg_fpcm -
                                                            base_run_df$total_water_use_m3_per_kg_fpcm)/base_run_df$total_water_use_m3_per_kg_fpcm) *
                                                          100), 0, ((scenario_df$total_water_use_m3_per_kg_fpcm -
                                                                       base_run_df$total_water_use_m3_per_kg_fpcm)/base_run_df$total_water_use_m3_per_kg_fpcm) *
                                               100)
    total_water_use_m3_per_kg_meat <- ifelse(!is.finite(((scenario_df$total_water_use_m3_per_kg_meat -
                                                            base_run_df$total_water_use_m3_per_kg_meat)/base_run_df$total_water_use_m3_per_kg_meat) *
                                                          100), 0, ((scenario_df$total_water_use_m3_per_kg_meat -
                                                                       base_run_df$total_water_use_m3_per_kg_meat)/base_run_df$total_water_use_m3_per_kg_meat) *
                                               100)
    total_water_use_m3_per_kg_protein <- ifelse(!is.finite(((scenario_df$total_water_use_m3_per_kg_protein -
                                                               base_run_df$total_water_use_m3_per_kg_protein)/base_run_df$total_water_use_m3_per_kg_protein) *
                                                             100), 0, ((scenario_df$total_water_use_m3_per_kg_protein -
                                                                          base_run_df$total_water_use_m3_per_kg_protein)/base_run_df$total_water_use_m3_per_kg_protein) *
                                                  100)
    carbon_stock_change_t_co2eq_per_year <- ifelse(!is.finite(((scenario_df$carbon_stock_change_t_co2eq_per_year -
                                                                  base_run_df$carbon_stock_change_t_co2eq_per_year)/base_run_df$carbon_stock_change_t_co2eq_per_year) *
                                                                100), 0, ((scenario_df$carbon_stock_change_t_co2eq_per_year -
                                                                             base_run_df$carbon_stock_change_t_co2eq_per_year)/base_run_df$carbon_stock_change_t_co2eq_per_year) *
                                                     100)
    carbon_stock_change_t_co2eq_per_ha_per_year <- ifelse(!is.finite(((scenario_df$carbon_stock_change_t_co2eq_per_ha_per_year -
                                                                         base_run_df$carbon_stock_change_t_co2eq_per_ha_per_year)/base_run_df$carbon_stock_change_t_co2eq_per_ha_per_year) *
                                                                       100), 0, ((scenario_df$carbon_stock_change_t_co2eq_per_ha_per_year -
                                                                                    base_run_df$carbon_stock_change_t_co2eq_per_ha_per_year)/base_run_df$carbon_stock_change_t_co2eq_per_ha_per_year) *
                                                            100)
    carbon_stock_change_t_co2eq_per_fpcm <- ifelse(!is.finite(((scenario_df$carbon_stock_change_t_co2eq_per_fpcm -
                                                                  base_run_df$carbon_stock_change_t_co2eq_per_fpcm)/base_run_df$carbon_stock_change_t_co2eq_per_fpcm) *
                                                                100), 0, ((scenario_df$carbon_stock_change_t_co2eq_per_fpcm -
                                                                             base_run_df$carbon_stock_change_t_co2eq_per_fpcm)/base_run_df$carbon_stock_change_t_co2eq_per_fpcm) *
                                                     100)
    carbon_stock_change_t_co2eq_per_meat <- ifelse(!is.finite(((scenario_df$carbon_stock_change_t_co2eq_per_meat -
                                                                  base_run_df$carbon_stock_change_t_co2eq_per_meat)/base_run_df$carbon_stock_change_t_co2eq_per_meat) *
                                                                100), 0, ((scenario_df$carbon_stock_change_t_co2eq_per_meat -
                                                                             base_run_df$carbon_stock_change_t_co2eq_per_meat)/base_run_df$carbon_stock_change_t_co2eq_per_meat) *
                                                     100)
    carbon_stock_change_t_co2eq_per_protein <- ifelse(!is.finite(((scenario_df$carbon_stock_change_t_co2eq_per_protein -
                                                                     base_run_df$carbon_stock_change_t_co2eq_per_protein)/base_run_df$carbon_stock_change_t_co2eq_per_protein) *
                                                                   100), 0, ((scenario_df$carbon_stock_change_t_co2eq_per_protein -
                                                                                base_run_df$carbon_stock_change_t_co2eq_per_protein)/base_run_df$carbon_stock_change_t_co2eq_per_protein) *
                                                        100)
    total_milk_produced_energy_kcal_per_year <- ifelse(!is.finite(((scenario_df$total_milk_produced_energy_kcal_per_year -
                                                                      base_run_df$total_milk_produced_energy_kcal_per_year)/base_run_df$total_milk_produced_energy_kcal_per_year) *
                                                                    100), 0, ((scenario_df$total_milk_produced_energy_kcal_per_year -
                                                                                 base_run_df$total_milk_produced_energy_kcal_per_year)/base_run_df$total_milk_produced_energy_kcal_per_year) *
                                                         100)
    total_meat_produced_energy_kcal_per_year <- ifelse(!is.finite(((scenario_df$total_meat_produced_energy_kcal_per_year -
                                                                      base_run_df$total_meat_produced_energy_kcal_per_year)/base_run_df$total_meat_produced_energy_kcal_per_year) *
                                                                    100), 0, ((scenario_df$total_meat_produced_energy_kcal_per_year -
                                                                                 base_run_df$total_meat_produced_energy_kcal_per_year)/base_run_df$total_meat_produced_energy_kcal_per_year) *
                                                         100)
    total_milk_produced_ame_days_per_year <- ifelse(!is.finite(((scenario_df$total_milk_produced_ame_days_per_year -
                                                                   base_run_df$total_milk_produced_ame_days_per_year)/base_run_df$total_milk_produced_ame_days_per_year) *
                                                                 100), 0, ((scenario_df$total_milk_produced_ame_days_per_year -
                                                                              base_run_df$total_milk_produced_ame_days_per_year)/base_run_df$total_milk_produced_ame_days_per_year) *
                                                      100)
    total_meat_produced_ame_days_per_year <- ifelse(!is.finite(((scenario_df$total_meat_produced_ame_days_per_year -
                                                                   base_run_df$total_meat_produced_ame_days_per_year)/base_run_df$total_meat_produced_ame_days_per_year) *
                                                                 100), 0, ((scenario_df$total_meat_produced_ame_days_per_year -
                                                                              base_run_df$total_meat_produced_ame_days_per_year)/base_run_df$total_meat_produced_ame_days_per_year) *
                                                      100)
    total_carbon_balance_per_fpcm <- ifelse(!is.finite(((scenario_df$total_carbon_balance_per_fpcm -
                                                           base_run_df$total_carbon_balance_per_fpcm)/base_run_df$total_carbon_balance_per_fpcm) *
                                                         100), 0, ((scenario_df$total_carbon_balance_per_fpcm -
                                                                      base_run_df$total_carbon_balance_per_fpcm)/base_run_df$total_carbon_balance_per_fpcm) *
                                              100)
    total_carbon_balance_per_meat <- ifelse(!is.finite(((scenario_df$total_carbon_balance_per_meat -
                                                           base_run_df$total_carbon_balance_per_meat)/base_run_df$total_carbon_balance_per_meat) *
                                                         100), 0, ((scenario_df$total_carbon_balance_per_meat -
                                                                      base_run_df$total_carbon_balance_per_meat)/base_run_df$total_carbon_balance_per_meat) *
                                              100)
    total_carbon_balance_per_protein <- ifelse(!is.finite(((scenario_df$total_carbon_balance_per_protein -
                                                              base_run_df$total_carbon_balance_per_protein)/base_run_df$total_carbon_balance_per_protein) *
                                                            100), 0, ((scenario_df$total_carbon_balance_per_protein -
                                                                         base_run_df$total_carbon_balance_per_protein)/base_run_df$total_carbon_balance_per_protein) *
                                                 100)
    scenarioList[[i]] <- data.frame(scenario, total_milk_produced_kg_fpcm_per_year,
                                    total_meat_produced_kg_per_year, total_protein_produced_kg_per_year,
                                    total_tlu, total_land_requirement_ha, total_land_requirement_ha_per_kg_fpcm,
                                    total_land_requirement_ha_per_kg_meat, total_land_requirement_ha_per_kg_protein,
                                    total_land_requirement_ha_per_tlu, total_n_balance_kg_n_per_year,
                                    percent_area_mining, percent_area_leaching, n_balance_kg_n_per_ha_per_year,
                                    n_balance_kg_n_per_kg_fpcm, n_balance_kg_n_per_kg_meat,
                                    n_balance_kg_n_per_kg_protein, erosion_t_soil_year,
                                    erosion_t_soil_per_ha_per_year, erosion_kgsoil_per_kg_fpcm,
                                    erosion_kgsoil_per_kg_meat, erosion_kgsoil_per_kg_protein,
                                    ghg_emission_t_co2_eq_per_year, ghg_emission_t_co2_eq_per_ha_per_year,
                                    ghg_emission_t_co2_eq_per_kg_fpcm, ghg_emission_t_co2_eq_per_kg_meat,
                                    ghg_emission_t_co2_eq_per_kg_protein, percent_precipitation_used_for_feed_production,
                                    total_water_use_m3, total_water_use_m3_per_ha, total_water_use_m3_per_kg_fpcm,
                                    total_water_use_m3_per_kg_meat, total_water_use_m3_per_kg_protein,
                                    carbon_stock_change_t_co2eq_per_year, carbon_stock_change_t_co2eq_per_ha_per_year,
                                    carbon_stock_change_t_co2eq_per_fpcm, carbon_stock_change_t_co2eq_per_meat,
                                    carbon_stock_change_t_co2eq_per_protein, total_milk_produced_energy_kcal_per_year,
                                    total_meat_produced_energy_kcal_per_year, total_milk_produced_ame_days_per_year,
                                    total_meat_produced_ame_days_per_year, total_carbon_balance_per_fpcm,
                                    total_carbon_balance_per_meat, total_carbon_balance_per_protein)
  }
  results <- scenarioList %>% dplyr::bind_rows()
<<<<<<< HEAD

  # Save results in excel
  excel_output_path <- paste0(oDir2, "/scenario_comparison.xlsx")
  write.xlsx(results, excel_output_path, overwrite = TRUE)

  # Generate plots

  for(i in 2:ncol(results)){

=======
  for (i in 2:ncol(results)) {
>>>>>>> cleaned_v0.6.0
    datos <- results %>% select(1, all_of(i))
    tt <- colnames(datos[2])
    if (tt == "total_milk_produced_kg_fpcm_per_year") {
      title = "Total milk FPCM (kg/yr)"
      y_title = "% change in milk FPCM (kg/yr)"
    }
    else if (tt == "total_meat_produced_kg_per_year") {
      title = "Total meat (kg/yr)"
      y_title = "% change in meat (kg/yr)"
    }
    else if (tt == "total_protein_produced_kg_per_year") {
      title = "Total protein (kg/yr)"
      y_title = "% change in protein (kg/yr)"
    }
    else if (tt == "total_tlu") {
      title = "Tropical Livestock Unit"
      y_title = "% change in Tropical Livestock Unit"
    }
    else if (tt == "total_land_requirement_ha") {
      title = "Land required (ha/yr)"
      y_title = "% change in land required (ha/yr)"
    }
    else if (tt == "total_land_requirement_ha_per_kg_fpcm") {
      title = "Land required (ha/kg FPCM)"
      y_title = "% change in land required (ha/kg FPCM)"
    }
    else if (tt == "total_land_requirement_ha_per_kg_meat") {
      title = "Land required (ha/kg meat)"
      y_title = "% change in land required (ha/kg meat)"
    }
    else if (tt == "total_land_requirement_ha_per_kg_protein") {
      title = "Land required (ha/kg protein)"
      y_title = "% change in land required (ha/kg protein)"
    }
    else if (tt == "total_land_requirement_ha_per_tlu") {
      title = "Land required (ha/TLU)"
      y_title = "% change in land required (ha/TLU)"
    }
    else if (tt == "total_n_balance_kg_n_per_year") {
      title = "N balance (kg N/yr)"
      y_title = "% change in N balance (kg N/yr)"
    }
    else if (tt == "percent_area_mining") {
      title = "Soil mining (%)"
      y_title = "% change in soil mining"
    }
    else if (tt == "percent_area_leaching") {
      title = "Soil leaching (%)"
      y_title = "% change in soil leaching"
    }
    else if (tt == "n_balance_kg_n_per_ha_per_year") {
      title = "N balance (kg N/ha/yr)"
      y_title = "% change in N balance (kg N/ha/yr)"
    }
    else if (tt == "n_balance_kg_n_per_kg_fpcm") {
      title = "N balance (kg N/kg FPCM)"
      y_title = "% change in N balance (kg N/kg FPCM)"
    }
    else if (tt == "n_balance_kg_n_per_kg_meat") {
      title = "N balance (kg N/kg meat)"
      y_title = "% change in N balance (kg N/kg meat)"
    }
    else if (tt == "n_balance_kg_n_per_kg_protein") {
      title = "N balance (kg N/kg protein)"
      y_title = "% change in N balance (kg N/kg protein)"
    }
    else if (tt == "erosion_t_soil_year") {
      title = "Erosion (t soil/yr)"
      y_title = "% change in erosion (t soil/yr)"
    }
    else if (tt == "erosion_t_soil_per_ha_per_year") {
      title = "Erosion (t soil/ha/yr)"
      y_title = "% change in erosion (t soil/ha/yr)"
    }
    else if (tt == "erosion_kgsoil_per_kg_fpcm") {
      title = "Erosion (t soil/kg FPCM)"
      y_title = "% change in erosion (t soil/kg FPCM)"
    }
    else if (tt == "erosion_kgsoil_per_kg_meat") {
      title = "Erosion (t soil/kg meat)"
      y_title = "% change in erosion (t soil/kg meat)"
    }
    else if (tt == "erosion_kgsoil_per_kg_protein") {
      title = "Erosion (t soil/kg protein)"
      y_title = "% change in erosion (t soil/kg protein)"
    }
    else if (tt == "ghg_emission_t_co2_eq_per_year") {
      title = "GHG (t CO2eq/yr)"
      y_title = "% change in GHG (t CO2eq/yr)"
    }
    else if (tt == "ghg_emission_t_co2_eq_per_ha_per_year") {
      title = "GHG (t CO2eq/ha/yr)"
      y_title = "% change in GHG (t CO2eq/ha/yr)"
    }
    else if (tt == "ghg_emission_t_co2_eq_per_kg_fpcm") {
      title = "GHG (CO2eq/kg FPCM)"
      y_title = "% change in GHG (CO2eq/kg FPCM)"
    }
    else if (tt == "ghg_emission_t_co2_eq_per_kg_meat") {
      title = "GHG (CO2eq/kg meat)"
      y_title = "% change in GHG (CO2eq/kg meat)"
    }
    else if (tt == "ghg_emission_t_co2_eq_per_kg_protein") {
      title = "GHG (CO2eq/kg protein)"
      y_title = "% change in GHG (CO2eq/kg protein)"
    }
    else if (tt == "percent_precipitation_used_for_feed_production") {
      title = "% Precipitation use for feed production"
      y_title = "% change in % precipitation use for feed production"
    }
    else if (tt == "total_water_use_m3") {
      title = "Water use (m3/yr)"
      y_title = "% change water use (m3/yr)"
    }
    else if (tt == "total_water_use_m3_per_ha") {
      title = "Water use (m3/ha)"
      y_title = "% change in water use (m3/ha)"
    }
    else if (tt == "total_water_use_m3_per_kg_fpcm") {
      title = "Water use (m3/kg FPCM)"
      y_title = "% change in water use (m3/kg FPCM)"
    }
    else if (tt == "total_water_use_m3_per_kg_meat") {
      title = "Water use (m3/kg meat)"
      y_title = "% change in water use (m3/kg meat)"
    }
    else if (tt == "total_water_use_m3_per_kg_protein") {
      title = "Water (m3/kg protein)"
      y_title = "% change in water (m3/kg protein)"
    }
    else if (tt == "carbon_stock_change_t_co2eq_per_year") {
      title = "Carbon stock changes (t CO2eq/yr)"
      y_title = "% change in carbon stock changes (t CO2eq/yr)"
    }
    else if (tt == "carbon_stock_change_t_co2eq_per_ha_per_year") {
      title = "Carbon stock changes (t CO2eq/ha/yr)"
      y_title = "% change in carbon stock changes (t CO2eq/ha/yr)"
    }
    else if (tt == "carbon_stock_change_t_co2eq_per_fpcm") {
      title = "Carbon stock changes (t CO2eq/kg FPCM)"
      y_title = "% change in carbon stock changes (t CO2eq/kg FPCM)"
    }
    else if (tt == "carbon_stock_change_t_co2eq_per_meat") {
      title = "Carbon stock changes (t CO2eq/kg meat)"
      y_title = "% change in carbon stock changes (t CO2eq/kg meat)"
    }
    else if (tt == "carbon_stock_change_t_co2eq_per_protein") {
      title = "Carbon stock changes (t CO2eq/kg protein)"
      y_title = "% change in carbon stock changes (t CO2eq/kg protein)"
    }
    else if (tt == "total_milk_produced_energy_kcal_per_year") {
      title = "Energy (kcal/kg FPCM)"
      y_title = "% change in energy (kcal/kg FPCM)"
    }
    else if (tt == "total_meat_produced_energy_kcal_per_year") {
      title = "Energy (kcal/kg meat)"
      y_title = "% change in energy (kcal/kg meat)"
    }
    else if (tt == "total_milk_produced_ame_days_per_year") {
      title = "AME (days/kg FPCM)"
      y_title = "% change in AME (days/kg FPCM)"
    }
    else if (tt == "total_meat_produced_ame_days_per_year") {
      title = "AME (days/kg meat)"
      y_title = "% change in AME (days/kg meat)"
    }
    else if (tt == "total_carbon_balance_per_fpcm") {
      title = "Carbon balance (t CO2eq/kg FPCM)"
      y_title = "% change in carbon balance (t CO2eq/kg FPCM)"
    }
    else if (tt == "total_carbon_balance_per_meat") {
      title = "Carbon balance (t CO2eq/kg meat)"
      y_title = "% change in carbon balance (t CO2eq/kg meat)"
    }
    else if (tt == "total_carbon_balance_per_protein") {
      title = "Carbon balance (t CO2eq/kg protein)"
      y_title = "% change in carbon balance (t CO2eq/kg protein)"
    }
    else {
      NA
    }

    output_list[[tt]] <- list(
      datos = datos,
      tt = tt,
      title = title,
      y_title = y_title
    )
  }
  return(output_list)
}

