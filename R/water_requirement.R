#' @title Water requirement
#'
#' @description It computes water requirement
#'
#' @param para A JSON file containing user inputs
#'
#' @param land_required A list computed using the `land_requirement` function
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
  feed_production <- unnest(para[["feed_items"]], cols = c(feed_type_name))

  #getting the livestock parameters
  livestock <- unnest(para[["livestock"]], cols = c())

  #area evapotranspiration
  et <- as.numeric(para[["et"]])

  #annual precipitation
  annual_precipitation <- as.numeric(para[["annual_prec"]])

  #computing water use per feed item
  water_use_per_feed_item <- land_required[["land_requirements_all"]] %>%
    group_by(feed)%>%
    summarise(area_feed = sum(area_feed, na.rm = T),
              area_non_feed = sum(area_non_feed, na.rm = T),
              area_total = sum(area_total, na.rm = T))%>% #reformating land required
    left_join(feed_production, by = c("feed"="feed_item_name"))%>%
    mutate(fraction_of_land_required = area_feed/sum(area_feed,na.rm = T),
           kc_average = (kc_initial+kc_midseason+kc_late)/3,
           kc_frac = fraction_of_land_required*kc_average,
           ET = kc_frac*et,
           water_use = ifelse(is.nan(ET*sum(area_feed)),0,ET*sum(area_feed)),
           feed_water_use = ifelse(is.nan(water_use*(1-(area_non_feed/area_total))),0,
                                   (water_use*(1-(area_non_feed/area_total)))),
           non_feed_water_use = water_use-feed_water_use,
           kc_water_use_of_roughages = ifelse(grepl("OFR",feed),feed_water_use,0),
           kc_water_use_of_concentrates = ifelse(grepl("OFC",feed),feed_water_use,0),
           kc_water_use_ip_concentrates = ifelse(grepl("IP",feed),feed_water_use,0),
           kc_water_use_on_farm = feed_water_use-kc_water_use_of_roughages-kc_water_use_ip_concentrates,
           kc_water_use_m3_per_ha = ifelse(is.nan(feed_water_use/area_feed),0,(feed_water_use/area_feed)))%>%
    select(feed,
           area_feed,
           kc_average,
           kc_frac,
           water_use,
           feed_water_use,
           non_feed_water_use,
           kc_water_use_of_roughages,
           kc_water_use_of_concentrates,
           kc_water_use_ip_concentrates,
           kc_water_use_on_farm,
           kc_water_use_m3_per_ha)

  #computing water use for production
  ET <- et*sum(water_use_per_feed_item$kc_frac)
  fraction_of_precipitation_used_for_feed_production <- ET/annual_precipitation
  total_water_use <- ET*sum(water_use_per_feed_item$area_feed)
  water_use_fpcm <- total_water_use/sum(livestock$herd_composition*livestock$annual_milk*(0.337+(0.116*livestock$fat_content)+(0.06*livestock$protein_milkcontent)))
  water_use_fpcm <- ifelse(!is.finite(water_use_fpcm),0,water_use_fpcm)
  water_use_meat <- total_water_use/sum(livestock$herd_composition*livestock$annual_growth*livestock$carcass_fraction)
  water_use_meat <- ifelse(!is.finite(water_use_meat),0,water_use_meat)
  water_use_protein <- total_water_use/(sum(livestock$herd_composition*livestock$annual_growth*livestock$carcass_fraction*(livestock$protein_meatcontent/100))+
                                          sum(livestock$herd_composition*livestock$annual_milk*(0.337+(0.116*livestock$fat_content)+(0.06*livestock$protein_milkcontent))*(livestock$protein_milkcontent/100)))
  water_use_protein <- ifelse(!is.finite(water_use_protein),0,water_use_protein)

  #merging water use items for production
  water_use_for_production <- as.data.frame(t(cbind(ET,
                                                    fraction_of_precipitation_used_for_feed_production,
                                                    total_water_use,
                                                    water_use_fpcm,
                                                    water_use_meat,
                                                    water_use_protein)))
  Items <- rownames(water_use_for_production)
  rownames(water_use_for_production) <- NULL
  water_use_for_production <- as.data.frame(cbind(Items,water_use_for_production))
  names(water_use_for_production) <- c("Names","Value")

  water_use <- list(water_use_per_feed_item = water_use_per_feed_item,
                    water_use_for_production = water_use_for_production)

  #returning results
  return(water_use)
} #end of water function
