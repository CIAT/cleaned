#' @title Biomass calculation
#'
#' @description It computes biomass.
#'
#' @param para A JSON file
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

  tier1 <- data.frame("initial_landuse" = "cropland") %>%
    mutate(reporting_landuse = "cropland",
           annual_area_cropland_perennial_woody_biomass=0,
           annual_growth_rate_perenial_woody_biomass=2.6,
           annual_carbon_stock_biomass_removed=2.5,
           annual_change_carbon_stocks_biomass=annual_area_cropland_perennial_woody_biomass*(annual_growth_rate_perenial_woody_biomass-annual_carbon_stock_biomass_removed))

  # Land requirement for feed production per associated crop (ha)
  land_requirement_per_feed <- land_required[["land_requirements_all"]] %>%
    select(feed, area_feed) %>%
    group_by(feed) %>%
    summarise(area_feed = sum(area_feed))

  # add feed category
  feed_production <- unnest(para[["feed_items"]], cols = c(feed_type_name))

  feed_production <- na_if(feed_production, "NA") %>%
    as.data.frame()

  # add area_feed to feed_production sheet
  feed_production$area_feed <- land_requirement_per_feed$area_feed[match(feed_production$feed_item_name,
                                                                         land_requirement_per_feed$feed)]

  # based on MICCA project (Kuyah et al 2012; Chave et Al. 2005
  tier3 <- feed_production %>%
    mutate(feed = feed_item_name,
           area_ha = ifelse(category=="tree crop" | category=="tree legume", area_feed, 0),
           nb_trees = trees_ha,
           dbh = ifelse(trees_dhb>0, trees_dhb, 0),
           agbest = ifelse(trees_dhb=="", 0, 0.091*trees_dhb^2.472),
           agb = agbest+((2.69/100)*agbest),
           carbon_content = 0.48,
           carbon_content_tree = agb*carbon_content,
           total_carbon_stock_ha = ifelse(is.na(agb), 0, nb_trees*carbon_content_tree),
           total_carbon_stock = area_ha*total_carbon_stock_ha,
           tree_annual_growth = trees_growth,
           annual_growth_ha = nb_trees*carbon_content*trees_growth,
           annual_growth = area_ha*annual_growth_ha,
           annual_removal = ifelse(trees_growth>0, trees_growth, 0),
           total_annual_removal = carbon_content*trees_removal,
           carbon_biomass_balance = annual_growth-trees_removal,
           carbon_stock_change_biomass = carbon_biomass_balance*44/12) %>%
    select(feed, area_ha, nb_trees, dbh, agbest, agb, carbon_content, carbon_content_tree,
    total_carbon_stock_ha, total_carbon_stock, tree_annual_growth, annual_growth_ha,
    annual_growth, annual_removal, total_annual_removal, carbon_biomass_balance,
    carbon_stock_change_biomass)

  feeds <- unique(feed_production$feed_item_name)

  trees_non_feed_biomass <- list()

  for (f in feeds){

    # selected feed from feed production
    selected_feed_production <- feed_production %>%
      dplyr::filter(feed_item_name == f)

    # selected feed from land requirements
    selected_land_required <- land_required[["land_requirements_all"]] %>%
      dplyr::filter(feed == f)

    #Trees non-feed (silvopastoral systems)
    trees_non_feed_biomass[[f]] <- selected_feed_production %>%
      select(feed_item_name, trees_ha_dbh25, increase_dbh25, trees_ha_dbh2550, increase_dbh2550, trees_ha_dbh50, increase_dbh50, time_horizon, average_dbh25, average_dbh2550, average_dbh50) %>%
      mutate(area_ha = sum(selected_land_required$farm),
             total_trees_25 = area_ha*trees_ha_dbh25,
             total_trees_2550 = area_ha*trees_ha_dbh2550,
             total_trees_50 = area_ha*trees_ha_dbh50,
             years = time_horizon,
             dbh_25_year0 = ifelse(is.na(average_dbh25), 0, average_dbh25),
             dbh_25_yearN = ifelse(is.na(years+dbh_25_year0*increase_dbh25), 0, years+dbh_25_year0*increase_dbh25),
             biomass_25_year0 = ifelse(is.na(exp(-1.996+2.32*log(dbh_25_year0))), 0, exp(-1.996+2.32*log(dbh_25_year0))),
             biomass_25_yearN = ifelse(is.na(exp(-1.996+2.32*log(dbh_25_yearN))), 0, exp(-1.996+2.32*log(dbh_25_yearN))),
             biomass_25_increase = (biomass_25_yearN-biomass_25_year0)*total_trees_25/1000,
             dbh_2550_year0 = ifelse(is.na(average_dbh2550), 0, average_dbh2550),
             dbh_2550_yearN = ifelse(is.na(years+dbh_2550_year0*increase_dbh2550), 0, years+dbh_2550_year0*increase_dbh2550),
             biomass_2550_year0 = ifelse(is.na(exp(-1.996+2.32*log(dbh_2550_year0))), 0, exp(-1.996+2.32*log(dbh_2550_year0))),
             biomass_2550_yearN = ifelse(is.na(exp(-1.996+2.32*log(dbh_2550_yearN))), 0, exp(-1.996+2.32*log(dbh_2550_yearN))),
             biomass_2550_increase = (biomass_2550_yearN-biomass_2550_year0)*total_trees_2550/1000,
             dbh_50_year0 = ifelse(is.na(average_dbh50), 0, average_dbh50),
             dbh_50_yearN = ifelse(is.na(years+dbh_50_year0*increase_dbh50), 0, years+dbh_50_year0*increase_dbh50),
             biomass_50_year0 = ifelse(is.na(exp(-1.996+2.32*log(dbh_50_year0))), 0, exp(-1.996+2.32*log(dbh_50_year0))),
             biomass_50_yearN = ifelse(is.na(exp(-1.996+2.32*log(dbh_50_yearN))), 0, exp(-1.996+2.32*log(dbh_50_yearN))),
             biomass_50_increase = (biomass_50_yearN-biomass_50_year0)*total_trees_50/1000,
             biomass_increase_total = ifelse(is.na((sum(biomass_25_increase, biomass_2550_increase, biomass_50_increase))/years), 0, (sum(biomass_25_increase, biomass_2550_increase, biomass_50_increase))/years),
             biomass_ccontent = 0.48,
             c_increase = biomass_increase_total*biomass_ccontent,
             co2_increase = c_increase*44/12,
             c_increase_soc = c_increase*0.25,
             co2_increase_soc = co2_increase*0.25,
             total_c_increase = sum(c_increase, c_increase_soc),
             total_co2_increase = sum(co2_increase, co2_increase_soc)) %>%
      select(-c(time_horizon, average_dbh25, average_dbh2550, average_dbh50))

  }

  trees_non_feed_biomass <- trees_non_feed_biomass %>%
    bind_rows()

  carbon_stocks_change <- list(tier1 = tier1,
                               tier3 = tier3,
                               trees_non_feed_biomass = trees_non_feed_biomass)

  #returning results
  return(carbon_stocks_change)

}
