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
      dplyr::filter(livestock == livestock_category_code)

    seasons <- unique(feed_basket_quality$season_name)

    seasonal_requirements <- list()

    for (season in seasons){

      # select feed and transpose the data
      season_feeds <- livestock_selected %>%
        dplyr::filter(season == season_name) %>%
        gather(feed,value,-season_name,-livestock_category_code,-livestock_category_name,-feed_variables)%>%
        spread(feed_variables,value)

      # select form energy requirment sheet
      season_selected_energy <- energy_required[["seasonal_results"]] %>%
        as.data.frame() %>%
        dplyr::filter(livestock == livestock_category_code, season == season_name)

      # replace NAs, Inf etc
      season_selected_energy <- season_selected_energy %>%
        dplyr::mutate_if(is.numeric, list(~na_if(.,Inf))) %>%
        replace(is.na(.), 0)

      feed_items <- unique(season_feeds$feed)

      land_requirements <- list()

      for (i in feed_items){

        # get crop yield
        feed_production <- unnest(para[["feed_items"]], cols = c(feed_type_name))
        feed_item_selected <- feed_production[feed_production$feed_item_name == i,]

        # # get main product removal
        # feed_item_selected <- as.data.frame(feed_selected[["feed_items"]])

        # selected feed from season feeds above
        selected_feed <- season_feeds[season_feeds$feed == i,]

        land_requirements[[i]] <- selected_feed %>%
          select(feed) %>%
          mutate(feed_item_dm = selected_feed$fraction_dry_matter*season_selected_energy$dmi_s,
                 crop_yield = as.numeric(feed_item_selected$dry_yield)*1000,
                 crop_removal = as.numeric(feed_item_selected$main_product_removal),
                 cr_yield = as.numeric(feed_item_selected$residue_dry_yield)*1000,
                 crop_residue_removal = ifelse(feed_item_selected$source_type == "Residue",
                                               as.numeric(feed_item_selected$residue_removal), 0),
                 area_total = ifelse(feed_item_selected$source_type == "Main",
                                     feed_item_dm/(crop_yield*crop_removal),
                                     ifelse(feed_item_selected$source_type != "Main", feed_item_dm/(cr_yield*crop_residue_removal), 0)),
                 area_non_feed = ifelse(crop_residue_removal > 0,
                                        area_total*(crop_yield*crop_removal)/(crop_yield*crop_removal+cr_yield*crop_residue_removal), 0),
                 area_feed = ifelse(crop_residue_removal > 0,
                                    area_total*(cr_yield*crop_residue_removal)/(crop_yield*crop_removal+cr_yield*crop_residue_removal),
                                    area_total*(crop_yield*crop_removal+cr_yield*crop_residue_removal)/(crop_yield*crop_removal+cr_yield*crop_residue_removal)),
                 grasses = ifelse(feed_item_selected$category == "grass", area_feed, 0),
                 tree_legume = ifelse(feed_item_selected$category == "tree crop" | feed_item_selected$category == "tree legume", area_feed, 0)) %>%
          dplyr::mutate_if(is.numeric, list(~na_if(.,Inf))) %>%
          replace(is.na(.), 0)

      }

      land_requirements <- land_requirements %>% bind_rows()


      land_requirements <- cbind(season_name = rep(selected_feed$season_name, times = nrow(land_requirements)),
                                 livestock_category_code = rep(selected_feed$livestock_category_code, times = nrow(land_requirements)),
                                 livestock_category_name = rep(selected_feed$livestock_category_name, times = nrow(land_requirements)),
                                 land_requirements)

      # bind by rows and add into seasonal requirement list
      seasonal_requirements[[season]] <- land_requirements %>% bind_rows()



    }

    livestock_requirements[[livestock]] <- seasonal_requirements %>% bind_rows()

  }

  land_requirements_all <- livestock_requirements %>% bind_rows()


}
