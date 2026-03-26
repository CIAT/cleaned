#' @title Soil health
#'
#' @description It computes soil health
#'
#' @param para A JSON file containing user inputs
#'
#' @param land_required A list computed using the `land_requirement` function
#'
#' @return dataframe
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' feed_basket_quality <- feed_quality(para)
#' energy_required <- energy_requirement(para,feed_basket_quality)
#' land_required <- land_requirement(feed_basket_quality, energy_required, para)
#' soil_health(para, land_required)
#' }
#'
#' @export

soil_health <- function(para, land_required) {

  soil_type <- para[["soil_description"]]

  erosivity_r <- 0.55 * (as.numeric(para[["annual_prec"]]) / as.numeric(para[["rain_length"]])) - 4.7
  erodibility_k <- as.numeric(para[["soil_k_value"]])

  feed_production <- para[["feed_items"]]
  feed_items <- unique(feed_production$feed_item_name)

  soil_erosion_all_feed <- list()

  slope_steepness_length_conversion <- function(x, y) {
    z <- ifelse(x == "Flat (0-5%)" & y >= 0 & y <= 1, 0.3,
         ifelse(x == "Hilly (5-20%)" & y >= 0 & y <= 1, 0.52,
         ifelse(x == "Steep (20-30%)" & y >= 0 & y <= 1, 0.59,
         ifelse(x == "Extremely steep (30%+)" & y >= 0 & y <= 1, 0.65,
         ifelse(x == "Flat (0-5%)" & y > 1 & y <= 3, 0.3,
         ifelse(x == "Hilly (5-20%)" & y > 1 & y <= 3, 0.85,
         ifelse(x == "Steep (20-30%)" & y > 1 & y <= 3, 1.13,
         ifelse(x == "Extremely steep (30%+)" & y > 1 & y <= 3, 1.36,
         ifelse(x == "Flat (0-5%)" & y > 3 & y <= 5, 0.3,
         ifelse(x == "Hilly (5-20%)" & y > 3 & y <= 5, 1.06,
         ifelse(x == "Steep (20-30%)" & y > 3 & y <= 5, 1.53,
         ifelse(x == "Extremely steep (30%+)" & y > 3 & y <= 5, 1.95,
         ifelse(x == "Flat (0-5%)" & y > 5 & y <= 15, 0.49,
         ifelse(x == "Hilly (5-20%)" & y > 5 & y <= 15, 2.22,
         ifelse(x == "Steep (20-30%)" & y > 5 & y <= 15, 3.39,
         ifelse(x == "Extremely steep (30%+)" & y > 5 & y <= 15, 4.45,
         ifelse(x == "Flat (0-5%)" & y > 15 & y <= 30, 0.65,
         ifelse(x == "Hilly (5-20%)" & y > 15 & y <= 30, 3.4,
         ifelse(x == "Steep (20-30%)" & y > 15 & y <= 30, 5.34,
         ifelse(x == "Extremely steep (30%+)" & y > 15 & y <= 30, 7.14,
         ifelse(x == "Flat (0-5%)" & y > 30, 1.01,
         ifelse(x == "Hilly (5-20%)" & y > 30, 6.68,
         ifelse(x == "Steep (20-30%)" & y > 30, 11.01,
         ifelse(x == "Extremely steep (30%+)" & y > 30, 15.14, NA_real_))))))))))))))))))))))))
    z
  }

  for (i in feed_items) {

    feed_selected <- feed_production[feed_production$feed_item_name == i, ]

    slope_desc <- feed_selected$slope_desc
    slope_length <- as.numeric(feed_selected$slope_length)

    ls <- slope_steepness_length_conversion(slope_desc, slope_length)
    c_factor <- as.numeric(feed_selected$landcover_c_factor)
    p_factor <- as.numeric(feed_selected$slope_p_factor)

    soil_loss_ha_year <- erosivity_r * erodibility_k * ls * c_factor * p_factor

    land_required_feed_selected <- land_required[["land_requirements_all"]] %>%
      as.data.frame() %>%
      dplyr::filter(feed == i)

    area_feed <- sum(as.numeric(land_required_feed_selected$area_feed), na.rm = TRUE)

    soil_loss_plot <- soil_loss_ha_year * area_feed

    soil_erosion_per_feed <- data.frame(
      feed_item = i,
      feed_type = i,
      soil_type = soil_type,
      erosivity_r = erosivity_r,
      erodibility_k = erodibility_k,
      ls = as.numeric(ls),
      c_factor = as.numeric(c_factor),
      p_factor = as.numeric(p_factor),
      soil_loss_ha_year = as.numeric(soil_loss_ha_year),
      soil_loss_plot = as.numeric(soil_loss_plot),
      stringsAsFactors = FALSE
    )

    soil_erosion_all_feed[[i]] <- soil_erosion_per_feed
  }

  dplyr::bind_rows(soil_erosion_all_feed)
}
