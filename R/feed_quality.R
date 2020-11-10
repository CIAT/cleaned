#' @title Feed quality
#'
#' @description It computes feed quality
#'
#' @param para A json file
#'
#' @return dataframe
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' feed_quality(para)
#' }
#'
#' @export

feed_quality <- function(para) {

  livestock_df <- para[["livestock"]]

  livestock_category_names <- c(livestock_df$livestock_category_code)

  livestock_allocation <- list()

  for (livestock in livestock_category_names) {

    seasons <- para[["seasons"]]

    # Compute feeding season ratio
    seasons <- seasons %>% mutate(feeding_ratio = season_length/sum(seasons$season_length))

    season_allocation <- list()

    for (season in 1:nrow(seasons)) {

      feed_production <- unnest(para[["feed_production"]], cols = c(feed_type_name))

      feed_types <- unique(feed_production$feed_type_name)

      feed_allocation <- list()

      for (i in 1:length(feed_types)) {

        feed_selected <- feed_production %>% filter(feed_type_name %in%
                                                      feed_production$feed_type_name[i])

        feed_item <- as.data.frame(feed_selected[["feed_items"]])

        # calculate me and dm fresh
        feed_item <- feed_item %>%
          mutate_at(c("cp_content","me_content", "dm_content"), as.numeric) %>%
          select(feed_item_code, feed_item_name, cp_content, me_content, dm_content) %>%
          mutate(me_content_fresh = dm_content * me_content/100,
                 cp_content_fresh = dm_content * cp_content/100,
                 de_fraction = me_content * 0.066) %>%
          select(feed_item_code, feed_item_name, cp_content_fresh, de_fraction, dm_content, me_content_fresh)


        # Extracting allocation
        feeding_seasons <- unnest(para[["livestock_feeding_seasons"]],
                                  cols = c(livestock_categories)) %>% filter(season_name %in%
                                                                               seasons$season_name[season])

        feeding_seasons <- feeding_seasons %>%
         mutate(livestock_category_name = ifelse(feeding_seasons$livestock_category_code  == livestock_df$livestock_category_code, livestock_df$livestock_category_name, NA))


        livestock_selected <- feeding_seasons[feeding_seasons$livestock_category_code == livestock, ]

        feed_item_select <- as.data.frame(livestock_selected[["allocation:"]])

        # select feed item
        feed_item_selected <- feed_item_select[feed_item_select$feed_item_code == feed_item$feed_item_code, ]

        feed_allocation[[i]] <- feed_item %>% mutate(fraction_as_fed = as.numeric(feed_item_selected$allocation)/100)
      }

      # Bind by rows
      feed_allocation_all <- feed_allocation %>% bind_rows() %>%
        select(-feed_item_code)

      # Gather
      feed_allocation_all <- feed_allocation_all %>%
        gather(feed_variables, value, cp_content_fresh:fraction_as_fed) %>%
        spread(feed_item_name, value) %>%
        mutate_at(-1, as.numeric)

      # calculate fraction of dry matter
      feed_allocation_all <- rbind(feed_allocation_all, c(feed_variables = "fraction_dry_matter",
                                                          feed_allocation_all[feed_allocation_all$feed_variables == "fraction_as_fed", -1] * feed_allocation_all[feed_allocation_all$feed_variables == "dm_content", -1]/sum(unlist(feed_allocation_all[feed_allocation_all$feed_variables == "fraction_as_fed", -1] * feed_allocation_all[feed_allocation_all$feed_variables == "dm_content", -1]))))



      # Bind and add into the season list
      season_allocation[[season]] <- cbind(season_name = rep(livestock_selected$season_name,
                                                             times = nrow(feed_allocation_all)),
                                           livestock_category_code = rep(livestock_selected$livestock_category_code, times = nrow(feed_allocation_all)),
                                           livestock_category_name = rep(livestock_selected$livestock_category_name, times = nrow(feed_allocation_all)),
                                           feed_allocation_all)


    }

    # Bind by rows
    season_feed_allocation <- season_allocation %>% bind_rows()

    livestock_allocation[[livestock]] <- season_feed_allocation

  }

  # Bind by rows
  livestock_feed_allocation <- livestock_allocation %>% bind_rows()

}
