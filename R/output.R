#' @title JSON output
#'
#' @description It generates a json file of all CLEANED_XtRa computation.
#'
#' @param feed_basket_quality A dataframe computed using the `feed_quality` function
#'
#' @param soil_erosion A dataframe computed using the `soil_health` function
#'
#' @param land_required A dataframe computed using the `land_requirement` function
#'
#' @param energy_required A list computed using the `energy_required` function
#'
#' @param water_requirements A dataframe computed using the `water_requirement` function
#'
#' @param nitrogen_balance A dataframe computed using the `n_balance` function
#'
#' @param livestock_productivity A dataframe computed using the `land_productivity` function
#'
#' @param economics A dataframe computed using the `economics_payback` function
#'
#' @param biomass A dataframe computed using the `biomass_calculation` function
#'
#' @param ghg_emissions A dataframe computed using the `n_balance` ghg_emission
#'
#' @return saved json file
#'
#' @importFrom jsonlite toJSON
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' data(ghg_para)
#' feed_basket_quality <- feed_quality(para)
#' energy_required <- energy_requirement(para,feed_basket_quality)
#' land_required <- land_requirement(feed_basket_quality, energy_required, para)
#' soil_erosion <- soil_health(para, land_required)
#' water_requirements <- water_requirement(para,land_required)
#' nitrogen_balance <- n_balance(para, land_required, soil_erosion)
#' livestock_productivity <- land_productivity(para)
#' economics <- economics_payback(para, energy_required)
#' biomass <- biomass_calculations(para, land_required)
#' ghg_emission <- ghg_emission(para,energy_required,ghg_ipcc_data,land_required,nitrogen_balance)
#' output(feed_basket_quality,energy_required,land_required,soil_erosion,water_requirements,
#' nitrogen_balance,livestock_productivity,economics,biomass,ghg_emissions)
#' }
#'
#' @export
#'
output <- function(feed_basket_quality,energy_required,land_required,
                   soil_erosion,water_requirements,nitrogen_balance,
                   livestock_productivity,economics,biomass,ghg_emissions){

  output_list <- list(feed_basket_quality = ifelse(exists("feed_basket_quality"), feed_basket_quality,"ERROR: Feed quality was not computed") ,
                      energy_required = ifelse(exists("energy_required"),energy_required,"ERROR: Energy requirement was not computed"),
                      land_required = ifelse(exists("land_required"),land_required,"ERROR: Land requirement was not computed"),
                      soil_erosion = ifelse(exists("soil_erosion"),soil_erosion,"ERROR: Soil erosion was not computed"),
                      water_requirements = ifelse(exists("water_requirements"),water_requirements,"ERROR: Water requirement was not computed"),
                      nitrogen_balance = ifelse(exists("nitrogen_balance"),nitrogen_balance,"ERROR: Nitrogen balance was not computed"),
                      livestock_productivity = ifelse(exists("livestock_productivity"),livestock_productivity,"ERROR: Livestock productivity was not computed"),
                      economics = ifelse(exists("economics"),economics,"ERROR: Farm economics were not computed"),
                      biomass = ifelse(exists("biomass"),biomass,"ERROR: Biomass was not computed"),
                      ghg_emissions = ifelse(exists("ghg_emissions"),ghg_emissions,"ERROR: Greenhouse gas emissions were not computed"))

  output <- jsonlite::toJSON(output_list,pretty = TRUE)
  write(output,"output.json")

} #end of output function
