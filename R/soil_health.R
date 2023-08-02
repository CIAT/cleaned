#' @title Soil health
#'
#' @description It computes soil health
#'
#' @param para A JSON file
#'
#' @param land_required A dataframe computed using the `land_requirement` function
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

  no_days <- 365

  soil_type <- para[["soil_description"]]

  erosivity_r <- 0.55*(as.numeric(para[["annual_prec"]])/(as.numeric(para[["rain_length"]])))-4.7

  erodibility_k <- as.numeric(para[["soil_k_value"]])

  feed_production <- unnest(para[["feed_items"]], cols = c(feed_type_name))

  feed_items <- unique(feed_production$feed_item_name)

  soil_erosion_all_feed <- list()

  for (i in feed_items){

    feed_item <- i

    feed_selected <- feed_production[feed_production$feed_item_name == i,]

    #feed_item_selected <- as.data.frame(feed_selected[["feed_items"]])

    slope_desc <- feed_selected$slope_desc

    slope <- as.numeric(feed_selected$slope)

    slope_length <- feed_selected$slope_length

    slope_steepness_length_conversion <- function(x, y){
      z <- ifelse(x == "Flat (0-5%)" & y >= 0 & y <= 1, 0.3,
                  ifelse(x == "Hilly (5-20%)" & y >= 0 & y <= 1, 0.52,
                         ifelse(x == "Steep (20-30%)" & y >= 0 & y <= 1, 0.59,
                                ifelse(x == "Extremely steep (30%+)" & y >= 0 & x <= 1, 0.65,
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
                                                                                                                                                                            ifelse(x == "Extremely steep (30%+)" & y > 30, 15.14, NA))))))))))))))))))))))))
      return(z)
    }

    # slope_steepness_length_conversion <- function(x, y){
    #   z <- ifelse(x >= 0 & x <= 5 & y == "1", 0.3,
    #               ifelse(x > 5 & x <= 20 & y == "1", 0.52,
    #                      ifelse(x > 20 & x <= 30 & y == "1", 0.59,
    #                             ifelse(x >30 & y == "1", 0.65,
    #                                    ifelse(x >= 0 & x <= 5 & y == "3", 0.3,
    #                                           ifelse(x > 5 & x <= 20 & y == "3", 0.85,
    #                                                  ifelse(x > 20 & x <= 30 & y == "3", 1.13,
    #                                                         ifelse(x >30 & y == "3", 1.36,
    #                                                                ifelse(x >= 0 & x <= 5 & y == "5", 0.3,
    #                                                                       ifelse(x > 5 & x <= 20 & y == "5", 1.06,
    #                                                                              ifelse(x > 20 & x <= 30 & y == "5", 1.53,
    #                                                                                     ifelse(x >30 & y == "5", 1.95,
    #                                                                                            ifelse(x >= 0 & x <= 5 & y == "15", 0.49,
    #                                                                                                   ifelse(x > 5 & x <= 20 & y == "15", 2.22,
    #                                                                                                          ifelse(x > 20 & x <= 30 & y == "15", 3.39,
    #                                                                                                                 ifelse(x >30 & y == "15", 4.45,
    #                                                                                                                        ifelse(x >= 0 & x <= 5 & y == "30", 0.65,
    #                                                                                                                               ifelse(x > 5 & x <= 20 & y == "30", 3.4,
    #                                                                                                                                      ifelse(x > 20 & x <= 30 & y == "30", 5.34,
    #                                                                                                                                             ifelse(x >30 & y == "30", 7.14,
    #                                                                                                                                                    ifelse(x >= 0 & x <= 5 & y == "90", 1.01,
    #                                                                                                                                                           ifelse(x > 5 & x <= 20 & y == "90", 6.68,
    #                                                                                                                                                                  ifelse(x > 20 & x <= 30 & y == "90", 11.01,
    #                                                                                                                                                                         ifelse(x >30 & y == "90", 15.14, 0))))))))))))))))))))))))
    #   return(z)
    # }

    ls <- slope_steepness_length_conversion(slope_desc, slope_length)

    # calculate cover factor
    c_factor <- feed_selected$landcover_c_factor



    # landcover_c_factor_conversion <- function(x){
    #
    #   z <- ifelse(x == "Dense forest", 0.001,
    #               ifelse(x == "Other forest", 0.05,
    #                      ifelse(x == "Badlands hard", 0.05,
    #                             ifelse(x == "Badlands soft", 0.4,
    #                                    ifelse(x == "Sorghum", 0.1,
    #                                           ifelse(x == "Maize", 0.1,
    #                                                  ifelse(x == "Cereals", 0.15,
    #                                                         ifelse(x == "Pulses", 0.15,
    #                                                                ifelse(x == "Dense grass", 0.01,
    #                                                                       ifelse(x == "Degraded grass", 0.05,
    #                                                                              ifelse(x == "Fallow hard", 0.05,
    #                                                                                     ifelse(x == "Fallow plouged", 0.6,
    #                                                                                            ifelse(x == "Ethiopian teff", 0.25,
    #                                                                                                   ifelse(x == "Continuous fallow", 1, NA))))))))))))))
    #   return(z)
    #
    # }
    #
    # c_factor <- landcover_c_factor_conversion(landcover_c_factor)


    # # calculate management factor
    # management_factor_conversion <- function(x){
    #
    #   ifelse(x == "Flat (0-5%)", 0.11,
    #          ifelse(x == "Hilly (5-20%)", 0.13,
    #                 ifelse(x == "Steep (20-30%)", 0.22,
    #                        ifelse(x == "Extremely steep (30%+)", 0.37, 1))))
    #
    # }
    #
    # p_factor <- management_factor_conversion(slope_p_factor)

    p_factor <- slope_p_factor

    # calculate Soil loss (t/ha/year)
    soil_loss_ha_year <- erosivity_r*erodibility_k*ls*c_factor*p_factor

    # select feed from land required dataframe
    land_required_feed_selected <- land_required[["land_requirements_all"]] %>%
      as.data.frame() %>%
      dplyr::filter(feed == i)

    # land requirement for feed production (ha)
    land_required_feed_selected <- sum(land_required_feed_selected$area_feed)

    # calculate Soil loss (t/plot/ season)
    soil_loss_plot <- soil_loss_ha_year*land_required_feed_selected

    # write data into a dataframe
    soil_erosion_per_feed <- as.data.frame(cbind(feed_item, soil_type, erosivity_r, erodibility_k, ls, c_factor, p_factor, soil_loss_ha_year, soil_loss_plot))

    soil_erosion_all_feed[[i]] <- soil_erosion_per_feed

  }

  soil_erosion_all_feed <- soil_erosion_all_feed %>% bind_rows()

}
