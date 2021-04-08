#' @title JSON output
#'
#' @description It generates a JSON file of all CLEANED model computation.
#'
#' @param feed_basket_quality A dataframe computed using the `feed_quality` function
#'
#' @param soil_erosion A dataframe computed using the `soil_health` function
#'
#' @param land_required A dataframe computed using the `land_requirement` function
#'
#' @param energy_required A list computed using the `energy_required` function
#'
#' @param water_required A dataframe computed using the `water_required` function
#'
#' @param nitrogen_balance A dataframe computed using the `n_balance` function
#'
#' @param livestock_productivity A dataframe computed using the `land_productivity` function
#'
#' @param economics A dataframe computed using the `economics_payback` function
#'
#' @param biomass A dataframe computed using the `biomass_calculation` function
#'
#' @param soil_carbon A dataframe computed using the `soil_organic_carbon` function
#'
#' @param ghg_emission A dataframe computed using the `n_balance` ghg_emission
#'
#' @return saved JSON file
#'
#' @importFrom jsonlite toJSON
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
#' ghg_emission <- ghg_emission(mufindi,energy_required,ghg_para,land_required,nitrogen_balance)
#' combineOutputs(feed_basket_quality,energy_required,land_required,soil_erosion,water_required,
#' nitrogen_balance,livestock_productivity,economics,biomass,soil_carbon,ghg_emission)
#' }
#'
#' @export
#'
combineOutputs <- function(feed_basket_quality, energy_required, land_required,
                                  soil_erosion, water_required, nitrogen_balance,
                                  livestock_productivity, economics, biomass,soil_carbon, ghg_emission){


  if (exists("feed_basket_quality")) {
    feed_basket_quality = split(feed_basket_quality, f=feed_basket_quality$season_name)
  }else {feed_basket_quality = "ERROR: Feed quality was not computed"}

  if (exists("energy_required")) {
    energy_required = energy_required
  }else {energy_required = "ERROR: Energy requirement was not computed"}

  if (exists("land_required")) {
    land_required = split(land_required, f=land_required$season_name)
  }else {land_required = "ERROR: Land requirement was not computed"}

  if (exists("soil_erosion")) {
    soil_erosion = soil_erosion
  }else {soil_erosion = "ERROR: Soil erosion was not computed"}

  if (exists("water_required")) {
    water_required = water_required
  }else {water_required = "ERROR: Water requirement was not computed"}

  if (exists("nitrogen_balance")) {
    nitrogen_balance = nitrogen_balance
  }else {nitrogen_balance = "ERROR: Nitrogen balance was not computed"}

  if (exists("livestock_productivity")) {
    livestock_productivity = livestock_productivity
  }else {livestock_productivity = "ERROR: Livestock productivity was not computed"}

  if (exists("economics")) {
    economics = economics
  }else {economics = "ERROR: Farm economics were not computed"}

  if (exists("biomass")) {
    biomass = biomass
  }else {biomass = "ERROR: Biomass was not computed"}

  if (exists("biomass")) {
    soil_carbon = soil_carbon
  }else {soil_carbon = "ERROR: Soil carbon was not computed"}

  if (exists("ghg_emission")) {
    ghg_emissions = ghg_emissions
  }else {ghg_emissions = "ERROR: Greenhouse gas emissions were not computed"}


  feed_basket_quality <- lapply(feed_basket_quality, function(x) {x <- x[,-1]})
  land_required <- lapply(land_required, function(x) {x <- x[,-1]})

  output_list <- list(feed_basket_quality = feed_basket_quality,
                      energy_required = energy_required,
                      land_required = land_required,
                      soil_erosion = soil_erosion,
                      water_required = water_required,
                      nitrogen_balance = nitrogen_balance,
                      livestock_productivity = livestock_productivity,
                      economics = economics,
                      biomass = biomass,
                      soil_carbon = soil_carbon,
                      ghg_emission = ghg_emission)

  jsonlite::toJSON(output_list, pretty = TRUE)

} #end of output function
