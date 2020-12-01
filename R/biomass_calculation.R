#' @title Biomass calculation
#'
#' @description It computes biomass.
#'
#' @param para A json file
#'
#' @param land_required A dataframe computed using the `land_requirement` function
#'
#' @return list
#'
#' @importFrom dplyr summarise select mutate group_by na_if %>%
#'
#' @importFrom utils de
#'
#' @importFrom tidyr unnest
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' feed_basket_quality <- feed_quality(para)
#' energy_required <- energy_requirement(para,feed_basket_quality)
#' land_required <- land_requirement(feed_basket_quality, energy_required, para)
#' soil_erosion <- soil_health(para, land_required)
#' nitrogen_balance <- n_balance(para, land_required, soil_erosion)
#' livestock_productivity <- land_productivity(para)
#' economics <- economics_payback(para, energy_required)
#' biomass_calculation(para, land_required)
#' }
#'
#' @export
#'

biomass_calculation <- function(para, land_required){

  tier1 <- data.frame()

  # Land requirement for feed production per associated crop (ha)
  land_requirement_per_feed <- land_required %>%
    select(feed, area_feed) %>%
    group_by(feed) %>%
    summarise(area_feed = sum(area_feed))

  # add feed category
  feed_production <- unnest(para[["feed_production"]], cols = c(feed_type_name))

  feed_production <- na_if(feed_production, "NA") %>%
    as.data.frame()

  land_requirement_per_feed$feed_category <- feed_production$feed_category[match(land_requirement_per_feed$feed,
                                                                                 feed_production$feed_type_name)]
  # To be added by rein
  dbh <- ""
  trees_annual_growth <- 0
  trees_annual_removal <- 0

  # based on MICCA project (Kuyah et al 2012; Chave et Al. 2005
  tier3 <- land_requirement_per_feed %>%
    mutate(area_ha = ifelse(feed_category=="tree crop" | feed_category=="tree legume",
                            area_feed, 0),
           nb_trees = 0,
           dbh = ifelse(dbh>0, dbh, 0),
           agbest = ifelse(dbh=="", 0, 0.091*dbh^2.472),
           agb = agbest+((2.69/100)*agbest),
           carbon_content = 0.48,
           carbon_content_tree = agb*carbon_content,
           total_carbon_stock_ha = ifelse(is.na(agb), 0, nb_trees*carbon_content_tree),
           total_carbon_stock = area_ha*total_carbon_stock_ha,
           tree_annual_growth = trees_annual_growth,
           annual_growth_ha = nb_trees*carbon_content*tree_annual_growth,
           annual_growth = area_ha*annual_growth_ha,
           annual_removal = ifelse(tree_annual_growth>0, tree_annual_growth, 0),
           total_annual_removal = carbon_content*annual_removal,
           carbon_biomass_balance = annual_growth-annual_removal,
           carbon_stock_change_biomass = carbon_biomass_balance*44/12) %>%
    select (-c(area_feed, feed_category))

  carbon_stocks_change <- list(tier1 = tier1, tier3 = tier3)

  #returning results
  return(carbon_stocks_change)

}
