#' @title Water requirement
#'
#' @description It computes water requirement
#'
#' @param para A json file
#'
#' @param land_required A dataframe computed using the `land_requirement` function
#'
#' @return list
#'
#' @importFrom dplyr summarise
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' feed_basket_quality <- feed_quality(para)
#' energy_required <- energy_requirement(para,feed_basket_quality)
#' land_required <- land_requirement(feed_basket_quality, energy_required, para)
#' soil_erosion <- soil_health(para, land_required)
#' water_requirement(para, land_required)
#' }
#'
#' @export

water_requirement <- function(para,land_required){
  #getting the crop parameters
  feed_production <- unnest(para[["feed_production"]], cols = c(feed_type_name))

  #convert columns to numeric
  cols_nam <- colnames(feed_production%>%select(kc_initial,kc_midseason,kc_late))
  feed_production[cols_nam] <- sapply(feed_production[cols_nam],as.numeric)
  #change NAs to 0
  feed_production[is.na(feed_production)] <- 0

  #getting the livestock parameters
  livestock <- unnest(para[["livestock"]])

  #convert columns to numeric
  cols_nam <- colnames(livestock%>%select(-livestock_category_code,-livestock_category_name))
  livestock[cols_nam] <- sapply(livestock[cols_nam],as.numeric)
  #change NAs to 0
  livestock[is.na(livestock)] <- 0

  #area evapotranspiration
  et <- as.numeric(para[["et"]])
  #annual precipitation
  annual_precipitation <- as.numeric(para[["annual_precipitation"]])

  #computing water use per feed item
  water_use_per_feed_item <- land_required%>%
    group_by(feed)%>%
    summarise(area_feed = sum(area_feed, na.rm = T),
              area_non_feed = sum(area_non_feed, na.rm = T),
              area_total = sum(area_total, na.rm = T))%>% #reformating land required
    left_join(feed_production, by = c("feed"="feed_type_name"))%>%
    mutate(fraction_of_land_required = area_feed/sum(area_feed,na.rm = T),
           kc_average = (kc_initial+kc_midseason+kc_late)/3,
           kc_frac = fraction_of_land_required*kc_average,
           ET = kc_frac*et,
           water_use = ET*sum(area_feed),
           feed_water_use = ifelse(is.nan(water_use*(1-(area_non_feed/area_total))),0,(water_use*(1-(area_non_feed/area_total)))),
           non_feed_water_use = water_use-feed_water_use)%>%
    select(feed,area_feed,area_non_feed,area_total,kc_average,kc_frac,ET,water_use,feed_water_use,non_feed_water_use)

  #computing water use for production
  ET <- et*sum(water_use_per_feed_item$kc_frac)
  fraction_of_precipitation_used_for_feed_production <- ET/annual_precipitation
  total_water_use <- ET*sum(water_use_per_feed_item$area_feed)
  water_use_fpcm <- total_water_use/sum(livestock$herd_composition*livestock$annual_milk*(0.337+(0.116*livestock$fat_content)+(0.06*livestock$protein_milkcontent)))
  water_use_meat <- total_water_use/sum(livestock$herd_composition*livestock$annual_growth*livestock$carcass_fraction)
  water_use_protein <- total_water_use/(sum(livestock$herd_composition*livestock$annual_growth*livestock$carcass_fraction*(livestock$protein_meatcontent/100))+
                                          sum(livestock$herd_composition*livestock$annual_milk*(0.337+(0.116*livestock$fat_content)+(0.06*livestock$protein_milkcontent))*(livestock$protein_milkcontent/100)))

  #merging water use items for production
  water_use_for_production <- cbind(ET,fraction_of_precipitation_used_for_feed_production,total_water_use,water_use_fpcm,water_use_meat,water_use_protein)

  water_use <- list(water_use_per_feed_item,water_use_for_production)

  #returning results
  return(water_use)
} #end of water function
