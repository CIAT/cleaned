#' @title Soil Organic Carbon
#'
#' @description It computes soil organic carbon status
#'
#' @param para A JSON file containing user inputs
#'
#' @param stock_change_para A JSON file with stock change parameters
#'
#' @param land_required A list computed using the `land_requirement` function
#'
#' @param biomass A dataframe computed using the `biomass_calculation` function
#'
#' @return dataframe
#'
#' @importFrom dplyr mutate %>% everything
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
#' biomass <- biomass_calculation(para, land_required)
#' soil_organic_carbon(para, stock_change_para, land_required, biomass)
#' }
#'
#' @export

soil_organic_carbon <- function(para, stock_change_para, land_required, biomass) {
  
  co2_conversion_factor <- 44 / 12
  
lookup_soc_factor <- function(tbl, key, label) {
  key <- trimws(as.character(key))
  tbl_names <- names(tbl)
  
  # exact match first
  hit <- match(key, tbl_names)
  
  # fallback: case-insensitive match
  if (is.na(hit)) {
    hit <- match(tolower(key), tolower(tbl_names))
  }
  
  if (is.na(hit)) {
    stop(
      paste0(
        "SOC lookup failed for ", label, ". Key not found: ", key,
        ". Available keys: ", paste(tbl_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  val <- tbl[[hit]]
  
  if (length(val) == 0 || all(is.na(val))) {
    stop(
      paste0("SOC lookup returned empty/NA value for ", label, ": ", key),
      call. = FALSE
    )
  }
  
  as.numeric(val[1])
}
  
  soil_amount <- 1000000 * (para[["soil_depth"]] / 100) * para[["soil_bulk"]]
  field_soc <- soil_amount * para[["soil_c"]] * 0.001
  
  land_required <- land_required[["land_requirements_all"]] %>%
    as.data.frame()
  
  # -------------------------
  # SOC lookup tables
  # -------------------------
  crop_landuse_tbl <- tidyr::unnest(stock_change_para[["cropland"]], cols = c(landuse)) %>%
    tidyr::unnest(cols = c(factor_variables))
  
  crop_tillage_tbl <- tidyr::unnest(stock_change_para[["cropland"]], cols = c(tillage)) %>%
    tidyr::unnest(cols = c(factor_variables))
  
  crop_input_tbl <- tidyr::unnest(stock_change_para[["cropland"]], cols = c(input)) %>%
    tidyr::unnest(cols = c(factor_variables))
  
  grass_landuse_tbl <- tidyr::unnest(stock_change_para[["grassland"]], cols = c(landuse)) %>%
    tidyr::unnest(cols = c(factor_variables))
  
  grass_mgmt_tbl <- tidyr::unnest(stock_change_para[["grassland"]], cols = c(management)) %>%
    tidyr::unnest(cols = c(factor_variables))
  
  grass_input_tbl <- tidyr::unnest(stock_change_para[["grassland"]], cols = c(input)) %>%
    tidyr::unnest(cols = c(factor_variables))
  
  # -------------------------
  # Cropland SOC
  # -------------------------
  cropland_change_carbon_stocks_mineral_soils <- data.frame(initial_landuse = "cropland") %>%
    dplyr::mutate(
      reporting_landuse = "cropland",
      area_last_year_inventory_period = sum(land_required$area_feed) -
        (sum(land_required$grasses) + sum(land_required$tree_legume)),
      carbon_stock_last_year_inventory_period = ifelse(field_soc > 0, field_soc, 30),
      time_dependence_stock_change = 20,
      stock_change_factor_land_use = lookup_soc_factor(
        crop_landuse_tbl,
        para[["cropland_system"]],
        "cropland_system"
      ),
      stock_change_factor_management = lookup_soc_factor(
        crop_tillage_tbl,
        para[["cropland_tillage"]],
        "cropland_tillage"
      ),
      stock_change_factor_input = lookup_soc_factor(
        crop_input_tbl,
        para[["cropland_orgmatter"]],
        "cropland_orgmatter"
      ),
      annual_change_carbon_stocks_mineral_soils =
        ((carbon_stock_last_year_inventory_period *
            stock_change_factor_land_use *
            stock_change_factor_management *
            stock_change_factor_input) -
           carbon_stock_last_year_inventory_period) /
        time_dependence_stock_change *
        area_last_year_inventory_period
    )
  
  # -------------------------
  # On-farm grassland SOC
  # -------------------------
  grassland_change_carbon_stocks_mineral_soils <- data.frame(initial_landuse = "grassland") %>%
    dplyr::mutate(
      reporting_landuse = "grassland",
      area_last_year_inventory_period = sum(land_required[which(land_required$grasses > 0), ]$farm),
      carbon_stock_last_year_inventory_period = 40,
      time_dependence_stock_change = 20,
      stock_change_factor_land_use = lookup_soc_factor(
        grass_landuse_tbl,
        "All",
        "grassland landuse"
      ),
      stock_change_factor_management = lookup_soc_factor(
        grass_mgmt_tbl,
        para[["grassland_management"]],
        "grassland_management"
      ),
      stock_change_factor_input = lookup_soc_factor(
        grass_input_tbl,
        para[["grassland_implevel"]],
        "grassland_implevel"
      ),
      annual_change_carbon_stocks_mineral_soils =
        ((carbon_stock_last_year_inventory_period *
            stock_change_factor_land_use *
            stock_change_factor_management *
            stock_change_factor_input) -
           carbon_stock_last_year_inventory_period) /
        time_dependence_stock_change *
        area_last_year_inventory_period
    )
  
  # -------------------------
  # Off-farm grassland SOC
  # -------------------------
  off_farm_grassland_change_carbon_stocks_mineral_soils <- data.frame(initial_landuse = "off_farm_grassland") %>%
    dplyr::mutate(
      reporting_landuse = "off_farm_grassland",
      area_last_year_inventory_period = sum(land_required[which(land_required$grasses > 0), ]$rough_of),
      carbon_stock_last_year_inventory_period = 40,
      time_dependence_stock_change = 20,
      stock_change_factor_land_use = lookup_soc_factor(
        grass_landuse_tbl,
        "All",
        "grassland landuse"
      ),
      stock_change_factor_management = lookup_soc_factor(
        grass_mgmt_tbl,
        para[["grassland_management"]],
        "grassland_management"
      ),
      stock_change_factor_input = lookup_soc_factor(
        grass_input_tbl,
        para[["grassland_implevel"]],
        "grassland_implevel"
      ),
      annual_change_carbon_stocks_mineral_soils =
        ((carbon_stock_last_year_inventory_period *
            stock_change_factor_land_use *
            stock_change_factor_management *
            stock_change_factor_input) -
           carbon_stock_last_year_inventory_period) /
        time_dependence_stock_change *
        area_last_year_inventory_period
    )
  
  annual_change_carbon_stocks_mineral_soils <- rbind(
    cropland_change_carbon_stocks_mineral_soils,
    grassland_change_carbon_stocks_mineral_soils,
    off_farm_grassland_change_carbon_stocks_mineral_soils
  )
  
  # This section is still under research and will be revisited
  annual_change_carbon_stocks_organic_soils <- data.frame(initial_landuse = "cropland") %>%
    dplyr::mutate(
      reporting_landuse = "cropland",
      land_area_cultivated_organic_soil = 0,
      emission_factor_climate_type = 0,
      carbon_loss_cultivated_organic_soils = land_area_cultivated_organic_soil * emission_factor_climate_type
    )
  
  annual_change_carbon_stocks_soils <- data.frame(initial_landuse = "cropland") %>%
    dplyr::mutate(
      reporting_landuse = "cropland",
      annual_change_carbon_stocks_mineral_soils =
        sum(annual_change_carbon_stocks_mineral_soils$annual_change_carbon_stocks_mineral_soils),
      annual_carbon_loss_cultivated_organic_soils =
        sum(annual_change_carbon_stocks_organic_soils$carbon_loss_cultivated_organic_soils),
      annual_change_inorganic_carbon_stocks_soils = 0,
      annual_change_carbon_stocks_soils =
        annual_change_carbon_stocks_mineral_soils +
        annual_carbon_loss_cultivated_organic_soils +
        annual_change_inorganic_carbon_stocks_soils
    )
  
  annual_change_carbon_stocks_trees_non_feed <- data.frame(type = "trees_non_feed") %>%
    dplyr::mutate(
      biomass = sum(biomass[["trees_non_feed_biomass"]]$c_increase_soc),
      below_ground = biomass,
      annual_change_carbon_stocks = below_ground
    )
  
  total_annual_change_carbon_soils <-
    annual_change_carbon_stocks_soils$annual_change_carbon_stocks_soils +
    annual_change_carbon_stocks_trees_non_feed$annual_change_carbon_stocks
  
  total_change_co2_soils <- total_annual_change_carbon_soils * co2_conversion_factor
  
  results <- data.frame(
    total_annual_change_carbon_soils = total_annual_change_carbon_soils,
    total_change_co2_soils = total_change_co2_soils
  )
  
  return(results)
}