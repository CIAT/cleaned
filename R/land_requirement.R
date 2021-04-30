#' @title Land requirement
#'
#' @description It compute land requirements
#'
#' @param para A JSON file
#'
#' @param feed_basket_quality A dataframe computed using the `feed_quality` function
#'
#' @param energy_required A list computed using the `energy_required` function
#'
#' @return dataframe
#'
#' @importFrom dplyr summarise mutate filter left_join %>% mutate_if
#'
#' @importFrom tidyr gather spread
#'
#' @importFrom stringr str_detect
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' feed_basket_quality <- feed_quality(para)
#' energy_required <- energy_requirement(para,feed_basket_quality)
#' land_requirement(feed_basket_quality, energy_required, para)
#' }
#'
#' @export

land_requirement <- function(feed_basket_quality, energy_required, para){

  livestock_category_code <- unique(feed_basket_quality$livestock_category_code)

  livestock_requirements <- list()

  for (livestock in livestock_category_code){

    livestock_selected <- feed_basket_quality %>%
      dplyr::filter(livestock_category_code == livestock)

    seasons <- unique(feed_basket_quality$season_name)

    seasonal_requirements <- list()

    for (season in seasons){

      # select feed and transpose the data
      season_feeds <- livestock_selected %>%
        dplyr::filter(season_name == season) %>%
        gather(feed,value,-season_name,-livestock_category_code,-livestock_category_name,-feed_variables)%>%
        spread(feed_variables,value)

      # select form energy requirment sheet
      season_selected_energy <- energy_required[["seasonal_results"]] %>%
        as.data.frame() %>%
        dplyr::filter(livestock_category_code == livestock, season_name == season)

      # replace NAs, Inf etc
      season_selected_energy <- season_selected_energy %>%
        dplyr::mutate_if(is.numeric, list(~na_if(.,Inf))) %>%
        replace(is.na(.), 0)

      feed_items <- unique(season_feeds$feed)

      land_requirements <- list()

      feed_items_frac <- list()

      for (i in feed_items){

        # select feed item
        feed_item_selected <- unnest(para[["feed_items"]], cols = c(feed_type_name)) %>%
          dplyr::filter(feed_item_name == i)

        #feed_item_selected <- feed_production[feed_production$feed_item_name == i,]

        # # get main product removal
        # feed_item_selected <- as.data.frame(feed_selected[["feed_items"]])

        # selected feed from season feeds above
        selected_feed <- season_feeds[season_feeds$feed == i,]

        # to be removed from JSON file
        feed_item_selected <- feed_item_selected %>%
          mutate(dry_yield = selected_feed$dm_content * fresh_yield,
                 residue_fresh_yield = ifelse(is.na(fresh_yield*((1-harvest_index)/harvest_index)), 0, fresh_yield*((1-harvest_index)/harvest_index)),
                 residue_dm_content = 1-(water_content/100),
                 residue_dry_yield = residue_dm_content*residue_fresh_yield,
                 residue_n_dm = dry_yield*residue_n)

        feed_items_frac[[i]] <- feed_item_selected

        land_requirements[[i]] <- selected_feed %>%
          mutate(feed_item_dm = selected_feed$fraction_dry_matter*season_selected_energy$dmi_s,
                 crop_yield = as.numeric(feed_item_selected$dry_yield)*1000,
                 crop_removal = as.numeric(feed_item_selected$main_product_removal),
                 cr_yield = as.numeric(feed_item_selected$residue_dry_yield)*1000,
                 crop_residue_removal = ifelse(feed_item_selected$source_type == "Residue",
                                               as.numeric(feed_item_selected$residue_removal), 0),
                 area_total = ifelse(feed_item_selected$source_type == "Main",
                                     feed_item_dm/(crop_yield*crop_removal),
                                     ifelse(feed_item_selected$source_type != "Main",
                                            feed_item_dm/(cr_yield*crop_residue_removal), 0)),
                 area_non_feed = ifelse(crop_residue_removal > 0,
                                        area_total*(crop_yield*crop_removal)/(crop_yield*crop_removal+cr_yield*crop_residue_removal), 0),
                 area_feed = ifelse(crop_residue_removal > 0,
                                    area_total*(cr_yield*crop_residue_removal)/(crop_yield*crop_removal+cr_yield*crop_residue_removal),
                                    area_total*(crop_yield*crop_removal+cr_yield*crop_residue_removal)/(crop_yield*crop_removal+cr_yield*crop_residue_removal)),
                 rough_of = ifelse(stringr::str_detect(selected_feed$feed, "OFR"), area_feed, 0),
                 conc_of = ifelse(stringr::str_detect(selected_feed$feed, "OFC"), area_feed, 0),
                 conc_ip = ifelse(stringr::str_detect(selected_feed$feed, "IP"), area_feed, 0),
                 farm = sum(area_feed, rough_of, conc_of, conc_ip),
                 grasses = ifelse(feed_item_selected$category == "grass", area_feed, 0),
                 tree_legume = ifelse(feed_item_selected$category == "tree crop" | feed_item_selected$category == "tree legume", area_feed, 0)) %>%
          dplyr::mutate_if(is.numeric, list(~na_if(.,Inf))) %>%
          replace(is.na(.), 0)

      }

      land_requirements <- land_requirements %>% bind_rows()

      feed_items_frac <- feed_items_frac %>% bind_rows()


      # land_requirements <- cbind(season_name = rep(selected_feed$season_name, times = nrow(land_requirements)),
      #                            livestock_category_code = rep(selected_feed$livestock_category_code, times = nrow(land_requirements)),
      #                            livestock_category_name = rep(selected_feed$livestock_category_name, times = nrow(land_requirements)),
      #                            land_requirements)

      # bind by rows and add into seasonal requirement list
      seasonal_requirements[[season]] <- land_requirements %>% bind_rows()



    }

    livestock_requirements[[livestock]] <- seasonal_requirements %>% bind_rows()

  }

  land_requirements_all <- livestock_requirements %>% bind_rows()

  results <- list(land_requirements_all = land_requirements_all,
                  feed_items_frac = feed_items_frac)

  return(results)

}
