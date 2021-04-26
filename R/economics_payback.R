#' @title Economics payback
#'
#' @description It computes economic payback from livestock products.
#'
#' @param para A JSON file
#'
#' @param energy_required A list computed using the `energy_required` function
#'
#' @param livestock_productivity A dataframe containing productivity values.
#'
#' @importFrom dplyr summarise mutate
#'
#' @return dataframe
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' feed_basket_quality <- feed_quality(para)
#' energy_required <- energy_requirement(para,feed_basket_quality)
#' land_required <- land_requirement(feed_basket_quality, energy_required, para)
#' soil_erosion <- soil_health(para, land_required)
#' nitrogen_balance <- nitrogen_balance(para, land_required, soil_erosion)
#' livestock_productivity <- land_productivity(para)
#' economics_payback(para, energy_required)
#' }
#'
#' @export

economics_payback <- function(para, energy_required, livestock_productivity){

  livestock_df <- para[["livestock"]]

  livestock_category_names <- c(livestock_df$livetype_desc)

  # # products dictionary
  # farm_gate_price <- function(x){
  #
  #   ifelse(x == "Cattle Manure", 0.0058,
  #          ifelse(x == "Sheep Manure", NA,
  #                 ifelse(x == "Goat Manure", NA,
  #                        ifelse(x == "Beef", 3,
  #                               ifelse(x == "Buffalo meat", NA,
  #                                      ifelse(x == "Goat/Lamb/Mutton", NA,
  #                                             ifelse(x == "Pork", NA,
  #                                                    ifelse(x == "Cow Milk", 0.35,
  #                                                           ifelse(x == "Buffalo Milk", NA,
  #                                                                  ifelse(x == "Goat/sheep milk", NA,
  #                                                                         ifelse(x == "Labour", 3.5,
  #                                                                                ifelse(x == "Urea", NA,
  #                                                                                       ifelse(x == "NPK", NA, NA)))))))))))))
  # }

  # Earnings
  economics_all <- livestock_productivity %>%
    mutate(meat_earnings = meat_production_animal*livestock_df$meat_price,
           milk_earnings = milk_production_animal*livestock_df$milk_price,
           manure_earnings = sum(energy_required[["annual_results"]]$annual_manure_produced)*0.0058) %>%
    select(livetype_name, meat_earnings, milk_earnings, manure_earnings)

  # # Cattle manure
  # cattle_manure <- energy_required[["annual_results"]] %>%
  #   as.data.frame() %>%
  #   summarise(product="Cattle Manure",
  #             total_production_year= sum(annual_manure_produced),
  #             estimated_production=sum(manure_collected))

  # # Beef
  # beef <- livestock_productivity %>%
  #   as.data.frame() %>%
  #   summarise(product="Beef",
  #             total_production_year= sum(meat_production_animal),
  #             estimated_production="")
  #
  # # Beef
  # cow_milk <- livestock_productivity %>%
  #   as.data.frame() %>%
  #   summarise(product="Cow Milk",
  #             total_production_year= sum(milk_production_animal),
  #             estimated_production="")

  #economics_all <- rbind.fill(list(cattle_manure, beef, cow_milk))

  # economics_all <- plyr::rbind.fill(list(cattle_manure,
  #                                        beef,
  #                                        cow_milk)) %>%
  #   mutate(retail_price_kg = farm_gate_price(product),
  #          total_value_production= retail_price_kg*total_production_year,
  #          estimated_production_value = ifelse(product=="Cattle Manure", retail_price_kg*as.numeric(estimated_production), ""))

}
