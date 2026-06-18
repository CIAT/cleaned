#' @title Calculate Differences
#'
#' @description It computes difference in environmental impact between scenarios
#'
#' @param outFile Path to the new comparison output json
#'
#' @param ... Paths of the different scenario outputs
#'
#' @return json file
#'
#' @importFrom dplyr %>%
#'
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' data(para)
#' data(ghg_para)
#' feed_basket_quality <- feed_quality(para)
#' energy_required <- energy_requirement(para,feed_basket_quality)
#' land_required <- land_requirement(feed_basket_quality, energy_required, para)
#' soil_erosion <- soil_health(para, land_required)
#' water_required <- water_requirement(para,land_required)
#' nitrogen_balance <- n_balance(para, land_required, soil_erosion)
#' livestock_productivity <- land_productivity(para)
#' biomass <- biomass_calculation(para, land_required)
#' soil_carbon <- soil_organic_carbon(para, land_required, biomass)
#' ghg_emissions <- ghg_emission(para,energy_required,ghg_para,land_required,nitrogen_balance)
#' combineOutputs(para,feed_basket_quality,energy_required,land_required,soil_erosion,water_required,
#' nitrogen_balance,livestock_productivity,biomass,soil_carbon,ghg_emissions,filePath)
#' calculate_differences(outFile,...)
#' }
#'
#' @export

calculate_differences <- function(outFile, ...) {

  outputList <- list(outFile = outFile, ...)
  output_path <- outputList[["outFile"]]
  outputList[[1]] <- NULL

  if (length(outputList) == 0) {
    stop("No files in source directory")
  }

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------
  scalar_num <- function(x, default = NA_real_) {
    if (is.null(x) || length(x) == 0) return(default)
    if (is.data.frame(x) || is.list(x)) return(default)
    x <- suppressWarnings(as.numeric(x[1]))
    if (length(x) == 0 || !is.finite(x)) return(default)
    x
  }

  scalar_chr <- function(x, default = NA_character_) {
    if (is.null(x) || length(x) == 0) return(default)
    x <- as.character(x[1])
    if (length(x) == 0 || is.na(x) || x == "") return(default)
    x
  }

safe_div <- function(num, den, default = 0) {
  num <- scalar_num(num, default = NA_real_)
  den <- scalar_num(den, default = NA_real_)

  if (is.na(num) || is.na(den)) return(NA_real_)
  if (den == 0) return(default)

  num / den
}

  safe_sum_vec <- function(x, default = NA_real_) {
    if (is.null(x) || length(x) == 0) return(default)
    x <- suppressWarnings(as.numeric(x))
    x[!is.finite(x)] <- NA_real_
    s <- sum(x, na.rm = TRUE)
    if (is.nan(s) || !is.finite(s)) return(default)
    s
  }

  get_section <- function(output, nm) {
    if (is.null(output) || !is.list(output) || !nm %in% names(output) || is.null(output[[nm]])) {
      return(data.frame())
    }
    x <- output[[nm]]
    if (is.data.frame(x)) return(x)
    tryCatch(as.data.frame(x), error = function(e) data.frame())
  }

  get_nested_section <- function(output, nm1, nm2) {
    if (is.null(output) || !is.list(output) || !nm1 %in% names(output) || is.null(output[[nm1]])) {
      return(data.frame())
    }
    sec <- output[[nm1]]
    if (!is.list(sec) || !nm2 %in% names(sec) || is.null(sec[[nm2]])) {
      return(data.frame())
    }
    x <- sec[[nm2]]
    if (is.data.frame(x)) return(x)
    tryCatch(as.data.frame(x), error = function(e) data.frame())
  }

  get_named_value <- function(df, name_col = "Names", value_col = "Value", target, default = NA_real_) {
    if (!is.data.frame(df) || nrow(df) == 0 || !name_col %in% names(df) || !value_col %in% names(df)) {
      return(default)
    }
    idx <- which(as.character(df[[name_col]]) == target)
    if (length(idx) == 0) return(default)
    scalar_num(df[[value_col]][idx[1]], default = default)
  }

  get_row_value <- function(df, filter_col, filter_value, value_col, default = NA_real_) {
    if (!is.data.frame(df) || nrow(df) == 0 ||
        !filter_col %in% names(df) || !value_col %in% names(df)) {
      return(default)
    }
    idx <- which(as.character(df[[filter_col]]) == filter_value)
    if (length(idx) == 0) return(default)
    scalar_num(df[[value_col]][idx[1]], default = default)
  }

  scenarioList <- list()

  for (i in seq_along(outputList)) {

    scenario <- sub("\\.\\w+$", "", basename(outputList[[i]]))
    output <- jsonlite::fromJSON(outputList[[i]], flatten = TRUE)

    # -------------------------------------------------------------------------
    # Read sections safely
    # -------------------------------------------------------------------------
    # Newer combineOutputs structure
    consumable_livestock_product <- get_section(output, "consumable_livestock_product")
    manure_produced <- get_section(output, "manure_produced")
    land_and_dm_required <- get_section(output, "land_dmi_required")
    ghg_balance <- get_section(output, "ghg_balance")
    global_warming_potential <- get_section(output, "global_warming_potential")
    water_use_for_production <- get_section(output, "water_use_for_production")
    nitrogen_balance <- get_section(output, "nitrogen_balance")
    soil_erosion_detail <- get_section(output, "soil_erosion_detail")
    soil_carbon <- get_section(output, "soil_carbon")
    biomass <- get_section(output, "biomass")

    # Backward-compatible older structure
    old_consumable_livestock_product <- get_nested_section(output, "livestock_productivity", "consumable_livestock_product")
    old_manure_produced <- get_nested_section(output, "livestock_productivity", "manure_produced")
    old_land_and_dm_required <- get_nested_section(output, "land_required", "land_and_dm_required")
    old_overall_soil_impact <- get_nested_section(output, "soil_impacts", "overal_soil_impact")
    old_ghg_balance <- get_nested_section(output, "ghg_emission", "ghg_balance")
    old_water_use_for_production <- get_nested_section(output, "water_required", "water_use_for_production")

    # -------------------------------------------------------------------------
    # Productivity
    # -------------------------------------------------------------------------
    if (nrow(consumable_livestock_product) > 0 && "total_milk" %in% names(consumable_livestock_product)) {
      total_milk_produced_kg_fpcm_per_year <- scalar_num(consumable_livestock_product$total_milk)
      total_meat_produced_kg_per_year <- scalar_num(consumable_livestock_product$total_meat)
      total_protein_produced_kg_per_year <- scalar_num(consumable_livestock_product$total_protein_milk) +
        scalar_num(consumable_livestock_product$total_protein_meat)
      total_milk_produced_energy_kcal_per_year <- scalar_num(consumable_livestock_product$total_energy_milk)
      total_meat_produced_energy_kcal_per_year <- scalar_num(consumable_livestock_product$total_energy_meat)
      total_tlu <- scalar_num(consumable_livestock_product$total_tlu)
    } else {
      total_milk_produced_kg_fpcm_per_year <- get_row_value(old_consumable_livestock_product, "produced_item", "Milk (FPCM)", "production_kg_per_year")
      total_meat_produced_kg_per_year <- get_row_value(old_consumable_livestock_product, "produced_item", "Meat", "production_kg_per_year")
      total_protein_produced_kg_per_year <-
        get_row_value(old_consumable_livestock_product, "produced_item", "Milk (FPCM)", "protein_kg_per_year", default = 0) +
        get_row_value(old_consumable_livestock_product, "produced_item", "Meat", "protein_kg_per_year", default = 0)
      total_milk_produced_energy_kcal_per_year <- get_row_value(old_consumable_livestock_product, "produced_item", "Milk (FPCM)", "production_energy_kcal_per_year")
      total_meat_produced_energy_kcal_per_year <- get_row_value(old_consumable_livestock_product, "produced_item", "Meat", "production_energy_kcal_per_year")
      total_tlu <- safe_sum_vec(old_manure_produced$tlu)
    }

    total_milk_produced_ame_days_per_year <- safe_div(total_milk_produced_energy_kcal_per_year, 2100)
    total_meat_produced_ame_days_per_year <- safe_div(total_meat_produced_energy_kcal_per_year, 2100)

    # -------------------------------------------------------------------------
    # Land requirement
    # -------------------------------------------------------------------------
    if (nrow(land_and_dm_required) > 0) {
      total_land_requirement_ha <- get_named_value(
        land_and_dm_required,
        target = "total_area_used_for_feed_production_ha"
      )
    } else {
      total_land_requirement_ha <- get_named_value(
        old_land_and_dm_required,
        target = "total_area_used_for_feed_production_ha"
      )
    }

    total_land_requirement_ha_per_kg_fpcm <- safe_div(total_land_requirement_ha, total_milk_produced_kg_fpcm_per_year) * 1000
    total_land_requirement_ha_per_kg_meat <- safe_div(total_land_requirement_ha, total_meat_produced_kg_per_year) * 1000
    total_land_requirement_ha_per_kg_protein <- safe_div(total_land_requirement_ha, total_protein_produced_kg_per_year) * 1000
    total_land_requirement_ha_per_tlu <- safe_div(total_land_requirement_ha, total_tlu)

    # -------------------------------------------------------------------------
    # N balance
    # -------------------------------------------------------------------------
    if (nrow(nitrogen_balance) > 0 && "nbalance_kg_n_total" %in% names(nitrogen_balance)) {
      total_n_balance_kg_n_per_year <- safe_sum_vec(nitrogen_balance$nbalance_kg_n_total)
      percent_area_mining <- safe_div(
        safe_sum_vec(nitrogen_balance$area_mining, default = 0),
        safe_sum_vec(nitrogen_balance$area_total, default = 0),
        default = 0
      ) * 100
      percent_area_leaching <- safe_div(
        safe_sum_vec(nitrogen_balance$area_leaching, default = 0),
        safe_sum_vec(nitrogen_balance$area_total, default = 0),
        default = 0
      ) * 100
    } else {
      total_n_balance_kg_n_per_year <- get_row_value(old_overall_soil_impact, "sources", "total", "balance_N_kg_N_year")
      percent_area_mining <- get_row_value(old_overall_soil_impact, "sources", "total", "percent_area_mining")
      percent_area_leaching <- get_row_value(old_overall_soil_impact, "sources", "total", "percent_area_leaching")
    }

    n_balance_kg_n_per_ha_per_year <- safe_div(total_n_balance_kg_n_per_year, total_land_requirement_ha)
    n_balance_kg_n_per_kg_fpcm <- safe_div(total_n_balance_kg_n_per_year, total_milk_produced_kg_fpcm_per_year)
    n_balance_kg_n_per_kg_meat <- safe_div(total_n_balance_kg_n_per_year, total_meat_produced_kg_per_year)
    n_balance_kg_n_per_kg_protein <- safe_div(total_n_balance_kg_n_per_year, total_protein_produced_kg_per_year)

    # -------------------------------------------------------------------------
    # Soil erosion
    # -------------------------------------------------------------------------
    if (nrow(soil_erosion_detail) > 0 && "soil_loss_plot" %in% names(soil_erosion_detail)) {
      erosion_t_soil_year <- safe_sum_vec(soil_erosion_detail$soil_loss_plot)
    } else {
      erosion_t_soil_year <- get_row_value(old_overall_soil_impact, "sources", "total", "erosion_t_soil_year")
    }

    erosion_t_soil_per_ha_per_year <- safe_div(erosion_t_soil_year, total_land_requirement_ha)
    erosion_kgsoil_per_kg_fpcm <- safe_div(erosion_t_soil_year, total_milk_produced_kg_fpcm_per_year) * 1000
    erosion_kgsoil_per_kg_meat <- safe_div(erosion_t_soil_year, total_meat_produced_kg_per_year) * 1000
    erosion_kgsoil_per_kg_protein <- safe_div(erosion_t_soil_year, total_protein_produced_kg_per_year) * 1000

    # -------------------------------------------------------------------------
    # GHG emission
    # -------------------------------------------------------------------------
  
    if (nrow(ghg_balance) > 0 && "kg_co2_e_tot" %in% names(ghg_balance)) {
      # current combineOutputs structure:
      # kg_co2_e_tot is in kg CO2e, so convert to tonnes CO2e
      ghg_emission_t_co2_eq_per_year <- safe_sum_vec(ghg_balance$kg_co2_e_tot, default = NA_real_) / 1000

    } else if (nrow(ghg_balance) > 0 && "value" %in% names(ghg_balance)) {
      # backward-compatible app structure
      ghg_emission_t_co2_eq_per_year <- safe_sum_vec(ghg_balance$value, default = NA_real_)

    } else if (nrow(old_ghg_balance) > 0 && "kg_co2_e_tot" %in% names(old_ghg_balance)) {
      # backward-compatible old nested combineOutputs structure
      ghg_emission_t_co2_eq_per_year <- safe_sum_vec(old_ghg_balance$kg_co2_e_tot, default = NA_real_) / 1000

    } else if (nrow(old_ghg_balance) > 0 && "value" %in% names(old_ghg_balance)) {
      # backward-compatible old nested app structure
      ghg_emission_t_co2_eq_per_year <- safe_sum_vec(old_ghg_balance$value, default = NA_real_)

    } else if (nrow(global_warming_potential) > 0 && "gwp_total" %in% names(global_warming_potential)) {
      # future compact structure
      ghg_emission_t_co2_eq_per_year <- scalar_num(global_warming_potential$gwp_total)

    } else if (nrow(ghg_balance) > 0 && "total_ghg" %in% names(ghg_balance)) {
      # future compact structure fallback
      ghg_emission_t_co2_eq_per_year <- scalar_num(ghg_balance$total_ghg)

    } else {
      ghg_emission_t_co2_eq_per_year <- NA_real_
    }

    ghg_emission_t_co2_eq_per_ha_per_year <- safe_div(
      ghg_emission_t_co2_eq_per_year,
      total_land_requirement_ha
    )

    ghg_emission_t_co2_eq_per_kg_fpcm <- safe_div(
      ghg_emission_t_co2_eq_per_year,
      total_milk_produced_kg_fpcm_per_year
    ) * 1000

    ghg_emission_t_co2_eq_per_kg_meat <- safe_div(
      ghg_emission_t_co2_eq_per_year,
      total_meat_produced_kg_per_year
    ) * 1000

    ghg_emission_t_co2_eq_per_kg_protein <- safe_div(
      ghg_emission_t_co2_eq_per_year,
      total_protein_produced_kg_per_year
    ) * 1000

    # -------------------------------------------------------------------------
    # Water impacts
    # -------------------------------------------------------------------------
    if (nrow(water_use_for_production) > 0 && all(c("Names", "Value") %in% names(water_use_for_production))) {
      percent_precipitation_used_for_feed_production <- get_named_value(
        water_use_for_production,
        target = "fraction_of_precipitation_used_for_feed_production"
      ) * 100

      total_water_use_m3 <- get_named_value(
        water_use_for_production,
        target = "total_water_use"
      )

      total_water_use_m3_per_kg_fpcm <- get_named_value(
        water_use_for_production,
        target = "water_use_fpcm"
      )

      total_water_use_m3_per_kg_meat <- get_named_value(
        water_use_for_production,
        target = "water_use_meat"
      )

      total_water_use_m3_per_kg_protein <- get_named_value(
        water_use_for_production,
        target = "water_use_protein"
      )
    } else {
      percent_precipitation_used_for_feed_production <- get_named_value(
        old_water_use_for_production,
        target = "fraction_of_precipitation_used_for_feed_production"
      ) * 100

      total_water_use_m3 <- get_named_value(
        old_water_use_for_production,
        target = "total_water_use"
      )

      total_water_use_m3_per_kg_fpcm <- get_named_value(
        old_water_use_for_production,
        target = "water_use_fpcm"
      )

      total_water_use_m3_per_kg_meat <- get_named_value(
        old_water_use_for_production,
        target = "water_use_meat"
      )

      total_water_use_m3_per_kg_protein <- get_named_value(
        old_water_use_for_production,
        target = "water_use_protein"
      )
    }

    total_water_use_m3_per_ha <- safe_div(total_water_use_m3, total_land_requirement_ha)

    # -------------------------------------------------------------------------
    # Carbon stock changes
    # -------------------------------------------------------------------------
    carbon_stock_change_t_co2eq_per_year <- safe_sum_vec(c(
      if ("total_change_co2_soils" %in% names(soil_carbon)) soil_carbon$total_change_co2_soils else numeric(0),
      if ("co2_increase" %in% names(biomass)) biomass$co2_increase else numeric(0)
    ))

    carbon_stock_change_t_co2eq_per_ha_per_year <- safe_div(carbon_stock_change_t_co2eq_per_year, total_land_requirement_ha)
    carbon_stock_change_t_co2eq_per_fpcm <- safe_div(carbon_stock_change_t_co2eq_per_year, total_milk_produced_kg_fpcm_per_year) * 1000
    carbon_stock_change_t_co2eq_per_meat <- safe_div(carbon_stock_change_t_co2eq_per_year, total_meat_produced_kg_per_year) * 1000
    carbon_stock_change_t_co2eq_per_protein <- safe_div(carbon_stock_change_t_co2eq_per_year, total_protein_produced_kg_per_year) * 1000

    # -------------------------------------------------------------------------
    # Total carbon balance
    # -------------------------------------------------------------------------
    total_carbon_balance_per_fpcm <- scalar_num(ghg_emission_t_co2_eq_per_kg_fpcm) - scalar_num(carbon_stock_change_t_co2eq_per_fpcm)
    total_carbon_balance_per_meat <- scalar_num(ghg_emission_t_co2_eq_per_kg_meat) - scalar_num(carbon_stock_change_t_co2eq_per_meat)
    total_carbon_balance_per_protein <- scalar_num(ghg_emission_t_co2_eq_per_kg_protein) - scalar_num(carbon_stock_change_t_co2eq_per_protein)

    # -------------------------------------------------------------------------
    # Final scenario row - every field forced to length 1
    # -------------------------------------------------------------------------
    scenarioList[[i]] <- data.frame(
      scenario = scalar_chr(scenario),

      total_milk_produced_kg_fpcm_per_year = scalar_num(total_milk_produced_kg_fpcm_per_year),
      total_meat_produced_kg_per_year = scalar_num(total_meat_produced_kg_per_year),
      total_protein_produced_kg_per_year = scalar_num(total_protein_produced_kg_per_year),
      total_tlu = scalar_num(total_tlu),

      total_land_requirement_ha = scalar_num(total_land_requirement_ha),
      total_land_requirement_ha_per_kg_fpcm = scalar_num(total_land_requirement_ha_per_kg_fpcm),
      total_land_requirement_ha_per_kg_meat = scalar_num(total_land_requirement_ha_per_kg_meat),
      total_land_requirement_ha_per_kg_protein = scalar_num(total_land_requirement_ha_per_kg_protein),
      total_land_requirement_ha_per_tlu = scalar_num(total_land_requirement_ha_per_tlu),

      total_n_balance_kg_n_per_year = scalar_num(total_n_balance_kg_n_per_year),
      percent_area_mining = scalar_num(percent_area_mining),
      percent_area_leaching = scalar_num(percent_area_leaching),
      n_balance_kg_n_per_ha_per_year = scalar_num(n_balance_kg_n_per_ha_per_year),
      n_balance_kg_n_per_kg_fpcm = scalar_num(n_balance_kg_n_per_kg_fpcm),
      n_balance_kg_n_per_kg_meat = scalar_num(n_balance_kg_n_per_kg_meat),
      n_balance_kg_n_per_kg_protein = scalar_num(n_balance_kg_n_per_kg_protein),

      erosion_t_soil_year = scalar_num(erosion_t_soil_year),
      erosion_t_soil_per_ha_per_year = scalar_num(erosion_t_soil_per_ha_per_year),
      erosion_kgsoil_per_kg_fpcm = scalar_num(erosion_kgsoil_per_kg_fpcm),
      erosion_kgsoil_per_kg_meat = scalar_num(erosion_kgsoil_per_kg_meat),
      erosion_kgsoil_per_kg_protein = scalar_num(erosion_kgsoil_per_kg_protein),

      ghg_emission_t_co2_eq_per_year = scalar_num(ghg_emission_t_co2_eq_per_year),
      ghg_emission_t_co2_eq_per_ha_per_year = scalar_num(ghg_emission_t_co2_eq_per_ha_per_year),
      ghg_emission_t_co2_eq_per_kg_fpcm = scalar_num(ghg_emission_t_co2_eq_per_kg_fpcm),
      ghg_emission_t_co2_eq_per_kg_meat = scalar_num(ghg_emission_t_co2_eq_per_kg_meat),
      ghg_emission_t_co2_eq_per_kg_protein = scalar_num(ghg_emission_t_co2_eq_per_kg_protein),

      percent_precipitation_used_for_feed_production = scalar_num(percent_precipitation_used_for_feed_production),
      total_water_use_m3 = scalar_num(total_water_use_m3),
      total_water_use_m3_per_ha = scalar_num(total_water_use_m3_per_ha),
      total_water_use_m3_per_kg_fpcm = scalar_num(total_water_use_m3_per_kg_fpcm),
      total_water_use_m3_per_kg_meat = scalar_num(total_water_use_m3_per_kg_meat),
      total_water_use_m3_per_kg_protein = scalar_num(total_water_use_m3_per_kg_protein),

      carbon_stock_change_t_co2eq_per_year = scalar_num(carbon_stock_change_t_co2eq_per_year),
      carbon_stock_change_t_co2eq_per_ha_per_year = scalar_num(carbon_stock_change_t_co2eq_per_ha_per_year),
      carbon_stock_change_t_co2eq_per_fpcm = scalar_num(carbon_stock_change_t_co2eq_per_fpcm),
      carbon_stock_change_t_co2eq_per_meat = scalar_num(carbon_stock_change_t_co2eq_per_meat),
      carbon_stock_change_t_co2eq_per_protein = scalar_num(carbon_stock_change_t_co2eq_per_protein),

      total_milk_produced_energy_kcal_per_year = scalar_num(total_milk_produced_energy_kcal_per_year),
      total_meat_produced_energy_kcal_per_year = scalar_num(total_meat_produced_energy_kcal_per_year),
      total_milk_produced_ame_days_per_year = scalar_num(total_milk_produced_ame_days_per_year),
      total_meat_produced_ame_days_per_year = scalar_num(total_meat_produced_ame_days_per_year),

      total_carbon_balance_per_fpcm = scalar_num(total_carbon_balance_per_fpcm),
      total_carbon_balance_per_meat = scalar_num(total_carbon_balance_per_meat),
      total_carbon_balance_per_protein = scalar_num(total_carbon_balance_per_protein),

      stringsAsFactors = FALSE
    )
  }

  results <- dplyr::bind_rows(scenarioList)

  write(jsonlite::toJSON(results, pretty = TRUE), output_path)

  excel_output_path <- paste0(dirname(output_path), "/runs_comparison.xlsx")
  openxlsx::write.xlsx(results, excel_output_path, overwrite = TRUE)
}