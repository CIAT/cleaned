#' @title JSON output
#'
#' @param para A JSON file containing user inputs
#'
#' @description It generates a JSON file of all CLEANED model computation.
#'
#' @param feed_basket_quality A dataframe computed using the `feed_quality` function
#'
#' @param soil_erosion A dataframe computed using the `soil_health` function
#'
#' @param land_required A list computed using the `land_requirement` function
#'
#' @param energy_required A list computed using the `energy_required` function
#'
#' @param water_required A list computed using the `water_required` function
#'
#' @param nitrogen_balance A dataframe computed using the `n_balance` function
#'
#' @param livestock_productivity A dataframe computed using the `land_productivity` function
#'
#' @param biomass A dataframe computed using the `biomass_calculation` function
#'
#' @param soil_carbon A dataframe computed using the `soil_organic_carbon` function
#'
#' @param ghg_emission A list computed using the `n_balance` ghg_emission
#'
#' @param filePath A path to where the JSON is to be saved
#'
#' @return saved JSON file
#'
#' @importFrom jsonlite toJSON
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
#' livestock_productivity <- land_productivity(para, energy_required)
#' biomass <- biomass_calculation(para, land_required)
#' soil_carbon <- soil_organic_carbon(para, land_required, biomass)
#' ghg_emission <- ghg_emission(para,energy_required,ghg_para,land_required,nitrogen_balance)
#' combineOutputs(para,feed_basket_quality,energy_required,land_required,soil_erosion,water_required,
#' nitrogen_balance,livestock_productivity,biomass,soil_carbon,ghg_emission,filePath)
#' }
#'
#' @export
#'
combineOutputs <- function(
    para,
    feed_basket_quality,
    energy_required,
    land_required,
    soil_erosion,
    water_required,
    nitrogen_balance,
    livestock_productivity,
    biomass,
    soil_carbon,
    ghg_emission,
    filePath,
    primary_excel = NULL
) {

  # ---------------------------------------------------------------------------
  # helpers
  # ---------------------------------------------------------------------------
  to_df <- function(x) {
    if (is.null(x)) return(data.frame())
    if (is.data.frame(x)) return(x)
    if (data.table::is.data.table(x)) return(as.data.frame(x))
    if (is.matrix(x)) return(as.data.frame(x))
    if (is.list(x) && !is.data.frame(x)) {
      out <- tryCatch(as.data.frame(x), error = function(e) data.frame())
      return(out)
    }
    data.frame(value = x, stringsAsFactors = FALSE)
  }

  clean_num <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x[!is.finite(x)] <- NA_real_
    x
  }

  sum_col <- function(df, col, default = 0) {
    if (is.null(df) || !is.data.frame(df) || !col %in% names(df)) return(default)
    x <- clean_num(df[[col]])
    s <- sum(x, na.rm = TRUE)
    if (!is.finite(s)) return(default)
    s
  }

  param_first <- function(col, default = NA_real_) {
    if (is.null(para) || !col %in% names(para) || length(para[[col]]) == 0) return(default)
    x <- para[[col]][1]
    if (is.null(x) || length(x) == 0) return(default)
    x
  }

  scalar_sum <- function(df, col, default = NA_real_) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 || !col %in% names(df)) return(default)
    x <- clean_num(df[[col]])
    s <- sum(x, na.rm = TRUE)
    if (is.nan(s) || !is.finite(s)) return(default)
    s
  }

  scalar_first <- function(x, default = NA_real_) {
    if (is.null(x) || length(x) == 0) return(default)
    if (is.list(x) && !is.data.frame(x)) return(default)
    x <- clean_num(x)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(default)
    x[1]
  }

  safe_div <- function(a, b, default = NA_real_) {
    a <- scalar_first(a, default = default)
    b <- scalar_first(b, default = default)
    if (is.na(a) || is.na(b) || b == 0) return(default)
    a / b
  }

  safe_div0 <- function(a, b) {
    out <- safe_div(a, b, default = 0)
    if (!is.finite(out) || is.na(out)) 0 else out
  }

  pick_df_with_cols <- function(x, cols) {
    if (is.data.frame(x) && all(cols %in% names(x))) return(as.data.frame(x))
    if (is.list(x)) {
      for (nm in names(x)) {
        y <- x[[nm]]
        if (is.data.frame(y) && all(cols %in% names(y))) return(as.data.frame(y))
      }
    }
    data.frame()
  }

  add_sheet_safe <- function(wb, sheet_name, x) {
    x <- to_df(x)
    if (sheet_name %in% names(wb)) openxlsx::removeWorksheet(wb, sheet_name)
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet = sheet_name, x = x)
  }

  app_output_mode <- !is.null(primary_excel) &&
    length(primary_excel) > 0 &&
    !is.na(primary_excel[1]) &&
    nzchar(primary_excel[1])

  # ---------------------------------------------------------------------------
  # unpack core objects safely
  # ---------------------------------------------------------------------------
  land_required_all <- if (is.list(land_required) && "land_requirements_all" %in% names(land_required)) {
    to_df(land_required[["land_requirements_all"]])
  } else {
    to_df(land_required)
  }

  land_required_feed_frac <- if (is.list(land_required) && "feed_items_frac" %in% names(land_required)) {
    to_df(land_required[["feed_items_frac"]])
  } else {
    data.frame()
  }

  energy_annual <- if (is.list(energy_required) && "annual_results" %in% names(energy_required)) {
    to_df(energy_required[["annual_results"]])
  } else {
    data.frame()
  }

  energy_seasonal <- if (is.list(energy_required) && "seasonal_results" %in% names(energy_required)) {
    to_df(energy_required[["seasonal_results"]])
  } else {
    data.frame()
  }

  water_use_per_feed_item <- if (is.list(water_required) && "water_use_per_feed_item" %in% names(water_required)) {
    to_df(water_required[["water_use_per_feed_item"]])
  } else {
    data.frame()
  }

  water_use_per_feed_item <- to_df(water_use_per_feed_item)
  if (nrow(water_use_per_feed_item) == 0 || !all(c("feed", "feed_water_use") %in% names(water_use_per_feed_item))) {
    water_use_per_feed_item <- data.frame(
      feed = character(),
      feed_water_use = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    water_use_per_feed_item$feed <- as.character(water_use_per_feed_item$feed)
    water_use_per_feed_item$feed_water_use <- clean_num(water_use_per_feed_item$feed_water_use)
  }

  water_use_for_production <- if (is.list(water_required) && "water_use_for_production" %in% names(water_required)) {
    to_df(water_required[["water_use_for_production"]])
  } else {
    data.frame()
  }

  # soil erosion
  soil_erosion_detail <- pick_df_with_cols(
    soil_erosion,
    c("feed_type", "ls", "soil_loss_ha_year", "soil_loss_plot")
  )

  overall_soil_impact <- if (is.data.frame(soil_erosion)) {
    to_df(soil_erosion)
  } else if (is.list(soil_erosion) && "overall_soil_impact" %in% names(soil_erosion)) {
    to_df(soil_erosion[["overall_soil_impact"]])
  } else if (nrow(soil_erosion_detail) > 0) {
    soil_erosion_detail
  } else {
    data.frame()
  }

  nitrogen_balance <- to_df(nitrogen_balance)
  nitrogen_balance_detail <- nitrogen_balance

  nitrogen_balance_output <- if (
    nrow(nitrogen_balance) > 0 &&
      all(c("feed", "nbalance_kg_n_total") %in% names(nitrogen_balance))
  ) {
    nitrogen_balance %>%
      dplyr::group_by(feed) %>%
      dplyr::summarise(
        nbalance_kg_n_total = sum(clean_num(nbalance_kg_n_total), na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    nitrogen_balance
  }

  nitrogen_balance_output <- to_df(nitrogen_balance_output)
  if (nrow(nitrogen_balance_output) == 0 || !all(c("feed", "nbalance_kg_n_total") %in% names(nitrogen_balance_output))) {
    nitrogen_balance_output <- data.frame(
      feed = character(),
      nbalance_kg_n_total = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    nitrogen_balance_output$feed <- as.character(nitrogen_balance_output$feed)
    nitrogen_balance_output$nbalance_kg_n_total <- clean_num(nitrogen_balance_output$nbalance_kg_n_total)
  }

  livestock_productivity <- to_df(livestock_productivity)
  biomass <- to_df(biomass)
  soil_carbon <- to_df(soil_carbon)

  ghg_ef <- if (is.list(ghg_emission) && "ef" %in% names(ghg_emission)) to_df(ghg_emission[["ef"]]) else data.frame()
ghg_eft <- if (is.list(ghg_emission) && "eft" %in% names(ghg_emission)) to_df(ghg_emission[["eft"]]) else data.frame()
ghg_n_excretion <- if (is.list(ghg_emission) && "n_excretion" %in% names(ghg_emission)) to_df(ghg_emission[["n_excretion"]]) else data.frame()
ghg_direct_n2o <- if (is.list(ghg_emission) && "direct_N2O" %in% names(ghg_emission)) to_df(ghg_emission[["direct_N2O"]]) else data.frame()
ghg_indirect_n2o <- if (is.list(ghg_emission) && "indirect_N2O" %in% names(ghg_emission)) to_df(ghg_emission[["indirect_N2O"]]) else data.frame()
ghg_land_used <- if (is.list(ghg_emission) && "land_used" %in% names(ghg_emission)) to_df(ghg_emission[["land_used"]]) else data.frame()
ghg_burn <- if (is.list(ghg_emission) && "ghg_burn" %in% names(ghg_emission)) to_df(ghg_emission[["ghg_burn"]]) else data.frame()
ghg_rice <- if (is.list(ghg_emission) && "ghg_rice" %in% names(ghg_emission)) to_df(ghg_emission[["ghg_rice"]]) else data.frame()

# keep raw nested objects for old-style formulas
ghg_soil_raw <- if (is.list(ghg_emission) && "ghg_soil" %in% names(ghg_emission)) ghg_emission[["ghg_soil"]] else NULL
ghg_fertilizer_raw <- if (is.list(ghg_emission) && "fetilizer_ghg" %in% names(ghg_emission)) ghg_emission[["fetilizer_ghg"]] else NULL

# flattened versions only for export sheets
ghg_soil <- to_df(ghg_soil_raw)
# ---------------------------------------------------------------------------
# FIX: properly unpack fertilizer outputs 
# ---------------------------------------------------------------------------
ghg_fertilizer_applied <- if (
  is.list(ghg_fertilizer_raw) &&
  "fertilizer_applied" %in% names(ghg_fertilizer_raw)
) {
  to_df(ghg_fertilizer_raw[["fertilizer_applied"]])
} else {
  data.frame()
}

ghg_fertilizer_by_crop <- if (
  is.list(ghg_fertilizer_raw) &&
  "fertlizer_emission_by_crop" %in% names(ghg_fertilizer_raw)
) {
  to_df(ghg_fertilizer_raw[["fertlizer_emission_by_crop"]])
} else {
  data.frame()
}

  # ---------------------------------------------------------------------------
  # land required summaries
  # ---------------------------------------------------------------------------
  if (nrow(land_required_all) > 0 && all(c("feed", "season_name") %in% names(land_required_all))) {

    land_required_output <- land_required_all %>%
      dplyr::group_by(feed, season_name) %>%
      dplyr::summarise(
        area_feed_total = sum(clean_num(area_feed), na.rm = TRUE),
        .groups = "drop_last"
      ) %>%
      dplyr::mutate(
        cumulative_area = cumsum(area_feed_total),
        label_position = cumulative_area - 0.7 * area_feed_total
      ) %>%
      dplyr::ungroup()

    seasonal_land_required <- land_required_all %>%
      dplyr::group_by(feed, season_name) %>%
      dplyr::summarise(
        area_feed_total = sum(clean_num(area_feed), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(names_from = season_name, values_from = area_feed_total)

    ex_land_required <- land_required_all %>%
      dplyr::group_by(feed) %>%
      dplyr::summarise(
        total_area = sum(clean_num(area_feed), na.rm = TRUE),
        farm = sum(clean_num(farm), na.rm = TRUE),
        rough_of = sum(clean_num(rough_of), na.rm = TRUE),
        conc_of = sum(clean_num(conc_of), na.rm = TRUE),
        conc_ip = sum(clean_num(conc_ip), na.rm = TRUE),
        grasses = sum(clean_num(grasses), na.rm = TRUE),
        tree_legume = sum(clean_num(tree_legume), na.rm = TRUE),
        .groups = "drop"
      )

    seasonal_dm_required <- land_required_all %>%
      dplyr::group_by(feed, season_name) %>%
      dplyr::summarise(
        feed_item_dm_total = sum(clean_num(feed_item_dm), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(names_from = season_name, values_from = feed_item_dm_total)

    ex_dm_required <- land_required_all %>%
      dplyr::group_by(feed) %>%
      dplyr::summarise(
        total_dm = sum(clean_num(feed_item_dm), na.rm = TRUE),
        farm_dm = sum(clean_num(farm_dm), na.rm = TRUE),
        rough_of_dm = sum(clean_num(rough_of_dm), na.rm = TRUE),
        conc_of_dm = sum(clean_num(conc_of_dm), na.rm = TRUE),
        conc_ip_dm = sum(clean_num(conc_ip_dm), na.rm = TRUE),
        grasses_dm = sum(clean_num(grasses_dm), na.rm = TRUE),
        tree_legume_dm = sum(clean_num(tree_legume_dm), na.rm = TRUE),
        .groups = "drop"
      )

    land_required_out <- dplyr::left_join(seasonal_land_required, ex_land_required, by = "feed")
    dm_required_out <- dplyr::left_join(seasonal_dm_required, ex_dm_required, by = "feed")

    total_area_used_for_feed_production_ha <- scalar_first(sum(clean_num(land_required_out$total_area), na.rm = TRUE))
    total_dm_used_for_feed_production_kg <- scalar_first(sum(clean_num(dm_required_out$total_dm), na.rm = TRUE))
    total_milk <- scalar_sum(livestock_productivity, "total_milk")

    land_and_dm_required <- data.frame(
      Names = c(
        "total_area_used_for_feed_production_ha",
        "area_required_per_milk_unit",
        "area_required_on_farm_ha",
        "area_required_roughages_off_farm_ha",
        "area_required_concentrates_off_farm_ha",
        "area_required_imported_concentrates_ha",
        NA,
        "total_dm_used_for_feed_production_kg",
        "dm_required_per_milk_unit",
        "dm_required_on_farm_kg",
        "dm_required_roughages_off_farm_kg",
        "dm_required_concentrates_off_farm_kg",
        "dm_required_imported_concentrates_kg"
      ),
      Value = c(
        scalar_first(total_area_used_for_feed_production_ha),
        scalar_first(safe_div(total_area_used_for_feed_production_ha, total_milk)),
        scalar_first(sum(clean_num(land_required_out$farm), na.rm = TRUE)),
        scalar_first(sum(clean_num(land_required_out$rough_of), na.rm = TRUE)),
        scalar_first(sum(clean_num(land_required_out$conc_of), na.rm = TRUE)),
        scalar_first(sum(clean_num(land_required_out$conc_ip), na.rm = TRUE)),
        NA_real_,
        scalar_first(total_dm_used_for_feed_production_kg),
        scalar_first(safe_div(total_dm_used_for_feed_production_kg, total_milk)),
        scalar_first(sum(clean_num(dm_required_out$farm_dm), na.rm = TRUE)),
        scalar_first(sum(clean_num(dm_required_out$rough_of_dm), na.rm = TRUE)),
        scalar_first(sum(clean_num(dm_required_out$conc_of_dm), na.rm = TRUE)),
        scalar_first(sum(clean_num(dm_required_out$conc_ip_dm), na.rm = TRUE))
      ),
      stringsAsFactors = FALSE
    )

  } else {
    land_required_output <- data.frame()
    land_required_out <- data.frame()
    dm_required_out <- data.frame()
    land_and_dm_required <- data.frame(
      Names = c(
        "total_area_used_for_feed_production_ha",
        "area_required_per_milk_unit",
        "area_required_on_farm_ha",
        "area_required_roughages_off_farm_ha",
        "area_required_concentrates_off_farm_ha",
        "area_required_imported_concentrates_ha",
        NA,
        "total_dm_used_for_feed_production_kg",
        "dm_required_per_milk_unit",
        "dm_required_on_farm_kg",
        "dm_required_roughages_off_farm_kg",
        "dm_required_concentrates_off_farm_kg",
        "dm_required_imported_concentrates_kg"
      ),
      Value = rep(NA_real_, 13),
      stringsAsFactors = FALSE
    )
  }

  land_required_output <- to_df(land_required_output)
  if (nrow(land_required_output) == 0 || !all(c("feed", "season_name", "area_feed_total") %in% names(land_required_output))) {
    land_required_output <- data.frame(
      feed = character(),
      season_name = character(),
      area_feed_total = numeric(),
      cumulative_area = numeric(),
      label_position = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    land_required_output$feed <- as.character(land_required_output$feed)
    land_required_output$season_name <- as.character(land_required_output$season_name)
    land_required_output$area_feed_total <- clean_num(land_required_output$area_feed_total)

    if (!"cumulative_area" %in% names(land_required_output)) {
      land_required_output$cumulative_area <- cumsum(replace(land_required_output$area_feed_total, is.na(land_required_output$area_feed_total), 0))
    } else {
      land_required_output$cumulative_area <- clean_num(land_required_output$cumulative_area)
    }

    if (!"label_position" %in% names(land_required_output)) {
      land_required_output$label_position <- land_required_output$cumulative_area - 0.7 * land_required_output$area_feed_total
    } else {
      land_required_output$label_position <- clean_num(land_required_output$label_position)
    }
  }

  # ---------------------------------------------------------------------------
  # productivity summary
  # ---------------------------------------------------------------------------
  if (nrow(livestock_productivity) > 0) {
    if (!"livetype_name" %in% names(livestock_productivity) && "livestock_category_name" %in% names(livestock_productivity)) {
      livestock_productivity$livetype_name <- livestock_productivity$livestock_category_name
    }

    if (!"total_milk" %in% names(livestock_productivity) && "milk_production_animal" %in% names(livestock_productivity)) {
      livestock_productivity$total_milk <- livestock_productivity$milk_production_animal
    }

    if (!"tlu" %in% names(livestock_productivity) && "herd_tlu" %in% names(livestock_productivity)) {
      livestock_productivity$tlu <- livestock_productivity$herd_tlu
    }
  }

  livestock_names <- if ("livetype_name" %in% names(livestock_productivity)) {
    as.character(livestock_productivity$livetype_name)
  } else {
    rep("", nrow(livestock_productivity))
  }

  sum_livestock_where <- function(col, idx) {
    if (!col %in% names(livestock_productivity) || !length(idx)) return(0)
    sum(clean_num(livestock_productivity[[col]])[idx], na.rm = TRUE)
  }

  energy_livestock_names <- if ("livestock_category_name" %in% names(energy_annual)) {
    as.character(energy_annual$livestock_category_name)
  } else if ("livetype_name" %in% names(energy_annual)) {
    as.character(energy_annual$livetype_name)
  } else {
    rep("", nrow(energy_annual))
  }

  sum_energy_where <- function(col, idx) {
    if (!col %in% names(energy_annual) || !length(idx)) return(0)
    sum(clean_num(energy_annual[[col]])[idx], na.rm = TRUE)
  }

  is_cattle <- grepl("Cattle", livestock_names, ignore.case = TRUE)
  is_buffalo <- grepl("Buffalo", livestock_names, ignore.case = TRUE)
  is_sheep <- grepl("Sheep", livestock_names, ignore.case = TRUE)
  is_goat <- grepl("Goat", livestock_names, ignore.case = TRUE)
  is_pig <- grepl("Pig|Swine", livestock_names, ignore.case = TRUE)
  is_other <- !is_cattle

  is_energy_cattle <- grepl("Cattle", energy_livestock_names, ignore.case = TRUE)
  is_energy_buffalo <- grepl("Buffalo", energy_livestock_names, ignore.case = TRUE)
  is_energy_sheep <- grepl("Sheep", energy_livestock_names, ignore.case = TRUE)
  is_energy_goat <- grepl("Goat", energy_livestock_names, ignore.case = TRUE)
  is_energy_pig <- grepl("Pig|Swine", energy_livestock_names, ignore.case = TRUE)

  cattle_milk_kg <- sum_livestock_where("total_milk", is_cattle)
  cattle_meat_kg <- sum_livestock_where("meat_production_animal", is_cattle)
  other_milk_kg <- sum_livestock_where("total_milk", is_other)
  other_meat_kg <- sum_livestock_where("meat_production_animal", is_other)
  total_milk_kg <- cattle_milk_kg + other_milk_kg
  total_meat_kg <- cattle_meat_kg + other_meat_kg

  cattle_milk_energy <- sum_livestock_where("energy_kcal_year_milk", is_cattle)
  cattle_meat_energy <- sum_livestock_where("energy_kcal_year_meat", is_cattle)
  other_milk_energy <- sum_livestock_where("energy_kcal_year_milk", is_other)
  other_meat_energy <- sum_livestock_where("energy_kcal_year_meat", is_other)
  total_milk_energy <- cattle_milk_energy + other_milk_energy
  total_meat_energy <- cattle_meat_energy + other_meat_energy

  cattle_milk_protein <- sum_livestock_where("protein_kg_year_milk", is_cattle)
  cattle_meat_protein <- sum_livestock_where("protein_kg_year_meat", is_cattle)
  other_milk_protein <- sum_livestock_where("protein_kg_year_milk", is_other)
  other_meat_protein <- sum_livestock_where("protein_kg_year_meat", is_other)
  total_milk_protein <- cattle_milk_protein + other_milk_protein
  total_meat_protein <- cattle_meat_protein + other_meat_protein

  consumable_livestock_product <- data.frame(
    produced_item = c("Cattle", "Milk (FPCM)", "Meat", "Other", "Milk (FPCM)", "Meat", "Total", "Milk (FPCM)", "Meat"),
    production_kg_per_year = c("", cattle_milk_kg, cattle_meat_kg, "", other_milk_kg, other_meat_kg, "", total_milk_kg, total_meat_kg),
    production_energy_kcal_per_year = c("", cattle_milk_energy, cattle_meat_energy, "", other_milk_energy, other_meat_energy, "", total_milk_energy, total_meat_energy),
    protein_kg_per_year = c("", cattle_milk_protein, cattle_meat_protein, "", other_milk_protein, other_meat_protein, "", total_milk_protein, total_meat_protein),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(ame_days = suppressWarnings(as.numeric(production_energy_kcal_per_year)) / 2500)

  manure_produced <- data.frame(
    livestock = c("Cattle", "Buffalo", "Sheep", "Goat", "Pig"),
    livestock_number = c(
      sum_livestock_where("number", is_cattle),
      sum_livestock_where("number", is_buffalo),
      sum_livestock_where("number", is_sheep),
      sum_livestock_where("number", is_goat),
      sum_livestock_where("number", is_pig)
    ),
    annual_manure_produced_kg_per_year = c(
      sum_energy_where("annual_manure_produced", is_energy_cattle),
      sum_energy_where("annual_manure_produced", is_energy_buffalo),
      sum_energy_where("annual_manure_produced", is_energy_sheep),
      sum_energy_where("annual_manure_produced", is_energy_goat),
      sum_energy_where("annual_manure_produced", is_energy_pig)
    ),
    daily_manure_produced_kg_per_day = c(
      sum_energy_where("daily_manure_produced", is_energy_cattle),
      sum_energy_where("daily_manure_produced", is_energy_buffalo),
      sum_energy_where("daily_manure_produced", is_energy_sheep),
      sum_energy_where("daily_manure_produced", is_energy_goat),
      sum_energy_where("daily_manure_produced", is_energy_pig)
    ),
    manure_onfarm_grazing_kg_per_year = c(
      sum_energy_where("manure_onfarm_grazing", is_energy_cattle),
      sum_energy_where("manure_onfarm_grazing", is_energy_buffalo),
      sum_energy_where("manure_onfarm_grazing", is_energy_sheep),
      sum_energy_where("manure_onfarm_grazing", is_energy_goat),
      sum_energy_where("manure_onfarm_grazing", is_energy_pig)
    ),
    manure_collected_kg_per_year = c(
      sum_energy_where("manure_collected", is_energy_cattle),
      sum_energy_where("manure_collected", is_energy_buffalo),
      sum_energy_where("manure_collected", is_energy_sheep),
      sum_energy_where("manure_collected", is_energy_goat),
      sum_energy_where("manure_collected", is_energy_pig)
    ),
    manure_exported_kg_per_year = c(
      sum_energy_where("manure_exported", is_energy_cattle),
      sum_energy_where("manure_exported", is_energy_buffalo),
      sum_energy_where("manure_exported", is_energy_sheep),
      sum_energy_where("manure_exported", is_energy_goat),
      sum_energy_where("manure_exported", is_energy_pig)
    ),
    tlu = c(
      sum_livestock_where("tlu", is_cattle),
      sum_livestock_where("tlu", is_buffalo),
      sum_livestock_where("tlu", is_sheep),
      sum_livestock_where("tlu", is_goat),
      sum_livestock_where("tlu", is_pig)
    ),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(livestock_number > 0)

  # ---------------------------------------------------------------------------
  # app-style soil and nitrogen summaries
  # ---------------------------------------------------------------------------
  soil_erosion_summary <- to_df(soil_erosion)
  if (nrow(soil_erosion_summary) > 0) {
    if (!"feed_item" %in% names(soil_erosion_summary)) {
      if ("feed" %in% names(soil_erosion_summary)) {
        soil_erosion_summary$feed_item <- soil_erosion_summary$feed
      } else if ("feed_type" %in% names(soil_erosion_summary)) {
        soil_erosion_summary$feed_item <- soil_erosion_summary$feed_type
      } else {
        soil_erosion_summary$feed_item <- ""
      }
    }
    if (!"soil_loss_plot" %in% names(soil_erosion_summary)) {
      soil_erosion_summary$soil_loss_plot <- if ("soil_loss_ha_year" %in% names(soil_erosion_summary)) {
        soil_erosion_summary$soil_loss_ha_year
      } else {
        0
      }
    }

    soil_loss_plot <- clean_num(soil_erosion_summary$soil_loss_plot)
    soil_erosion_summary$rough_of_soil_loss <- ifelse(grepl("OFR", soil_erosion_summary$feed_item), soil_loss_plot, 0)
    soil_erosion_summary$conc_of_soil_loss <- ifelse(grepl("OFC", soil_erosion_summary$feed_item), soil_loss_plot, 0)
    soil_erosion_summary$conc_ip_soil_loss <- ifelse(grepl("IP", soil_erosion_summary$feed_item), soil_loss_plot, 0)
    soil_erosion_summary$farm_soil_loss <- soil_loss_plot -
      soil_erosion_summary$rough_of_soil_loss -
      soil_erosion_summary$conc_of_soil_loss -
      soil_erosion_summary$conc_ip_soil_loss
  }

  overall_soil_impact <- data.frame(
    sources = c("total", "on-farm", "rough of", "conc of", "conc ip"),
    balance_N_kg_N_year = c(
      sum_col(nitrogen_balance, "nbalance_feed_only_kg_n"),
      sum_col(nitrogen_balance, "farm_kg_n"),
      sum_col(nitrogen_balance, "rough_of_kg_n"),
      sum_col(nitrogen_balance, "conc_of_kg_n"),
      sum_col(nitrogen_balance, "conc_ip_kg_n")
    ),
    balance_N_kg_N_ha = c(
      sum_col(nitrogen_balance, "nbalance_feed_only_kg_n_ha"),
      sum_col(nitrogen_balance, "farm_kg_n_ha"),
      sum_col(nitrogen_balance, "rough_of_kg_n_ha"),
      sum_col(nitrogen_balance, "conc_of_kg_n_ha"),
      sum_col(nitrogen_balance, "conc_ip_kg_n_ha")
    ),
    percent_area_mining = c(
      safe_div0(sum_col(nitrogen_balance, "area_mining"), sum_col(nitrogen_balance, "area_total")) * 100,
      safe_div0(sum_col(nitrogen_balance, "farm_area_mining"), sum_col(nitrogen_balance, "farm_area")) * 100,
      safe_div0(sum_col(nitrogen_balance, "rough_of_area_mining"), sum_col(nitrogen_balance, "rough_of_area")) * 100,
      safe_div0(sum_col(nitrogen_balance, "conc_of_nue_area_mining"), sum_col(nitrogen_balance, "conc_of_area")) * 100,
      safe_div0(sum_col(nitrogen_balance, "conc_ip_nue_area_mining"), sum_col(nitrogen_balance, "conc_ip_area")) * 100
    ),
    percent_area_leaching = c(
      safe_div0(sum_col(nitrogen_balance, "area_leaching"), sum_col(nitrogen_balance, "area_total")) * 100,
      safe_div0(sum_col(nitrogen_balance, "farm_area_leaching"), sum_col(nitrogen_balance, "farm_area")) * 100,
      safe_div0(sum_col(nitrogen_balance, "rough_of_area_leaching"), sum_col(nitrogen_balance, "rough_of_area")) * 100,
      safe_div0(sum_col(nitrogen_balance, "conc_of_nue_area_leaching"), sum_col(nitrogen_balance, "conc_of_area")) * 100,
      safe_div0(sum_col(nitrogen_balance, "conc_ip_nue_area_leaching"), sum_col(nitrogen_balance, "conc_ip_area")) * 100
    ),
    erosion_t_soil_year = c(
      sum_col(soil_erosion_summary, "soil_loss_plot"),
      sum_col(soil_erosion_summary, "rough_of_soil_loss"),
      sum_col(soil_erosion_summary, "conc_of_soil_loss"),
      sum_col(soil_erosion_summary, "conc_ip_soil_loss"),
      sum_col(soil_erosion_summary, "farm_soil_loss")
    ),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      erosion_t_soil_ha = c(
        safe_div0(erosion_t_soil_year[1], sum_col(nitrogen_balance, "area_total")),
        safe_div0(erosion_t_soil_year[2], sum_col(nitrogen_balance, "farm_area")),
        safe_div0(erosion_t_soil_year[3], sum_col(nitrogen_balance, "rough_of_area")),
        safe_div0(erosion_t_soil_year[4], sum_col(nitrogen_balance, "conc_of_area")),
        safe_div0(erosion_t_soil_year[5], sum_col(nitrogen_balance, "conc_ip_area"))
      )
    )

  legacy_nitrogen_cols <- c(
    "feed",
    "nin",
    "nout",
    "nbalance_kg_n_total",
    "nbalance_kg_n_ha_total",
    "nbalance_feed_only_kg_n",
    "nbalance_feed_only_kg_n_ha"
  )

  legacy_nitrogen_balance <- if (all(legacy_nitrogen_cols %in% names(nitrogen_balance))) {
    nitrogen_balance %>%
      dplyr::select(dplyr::all_of(legacy_nitrogen_cols)) %>%
      dplyr::mutate(
        nbalance_food_only_kg_n = nbalance_kg_n_total - nbalance_feed_only_kg_n,
        nbalance_food_only_kg_n_ha = nbalance_kg_n_ha_total - nbalance_feed_only_kg_n_ha
      )
  } else {
    nitrogen_balance
  }

  # ---------------------------------------------------------------------------
  # ghg summary - OLD STYLE per-hectare source attribution
  # ---------------------------------------------------------------------------

  methane <- 28
  N2O <- 265

  sum_num <- function(x) {
    x <- clean_num(x)
    sum(x, na.rm = TRUE)
  }

  safe_zero_div <- function(a, b) {
    a <- scalar_first(a, default = NA_real_)
    b <- scalar_first(b, default = NA_real_)
    if (is.na(a) || is.na(b) || b < 0.001) return(0)
    a / b
  }

  # land areas from current land_and_dm_required object
  get_named_value <- function(df, nm, default = 0) {
    if (!is.data.frame(df) || nrow(df) == 0) return(default)
    if (!all(c("Names", "Value") %in% names(df))) return(default)
    hit <- df$Value[df$Names == nm]
    if (!length(hit)) return(default)
    scalar_first(hit, default = default)
  }

  area_required_on_farm_ha <- get_named_value(land_and_dm_required, "area_required_on_farm_ha", 0)
  area_required_roughages_off_farm_ha <- get_named_value(land_and_dm_required, "area_required_roughages_off_farm_ha", 0)
  area_required_concentrates_off_farm_ha <- get_named_value(land_and_dm_required, "area_required_concentrates_off_farm_ha", 0)
  area_required_imported_concentrates_ha <- get_named_value(land_and_dm_required, "area_required_imported_concentrates_ha", 0)

  # milk produced
  total_milk_produced_kg_fpcm_per_year <- scalar_sum(livestock_productivity, "total_milk", default = NA_real_)
  if (is.na(total_milk_produced_kg_fpcm_per_year) || total_milk_produced_kg_fpcm_per_year == 0) {
    total_milk_produced_kg_fpcm_per_year <- scalar_sum(consumable_livestock_product, "total_milk", default = NA_real_)
  }

  # -------------------------
  # On-farm
  # -------------------------
  enteric_fermentation_methane <- sum_num(ghg_ef$enteric_methane_emissions)
  enteric_fermentation_methane_tot_kg_co2_e <- enteric_fermentation_methane * methane
  enteric_fermentation_methane_per_ha_kg_co2_e <- safe_zero_div(enteric_fermentation_methane_tot_kg_co2_e, area_required_on_farm_ha)
  enteric_fermentation_methane_kg_co2_e_per_kg_fpcm <- safe_zero_div(enteric_fermentation_methane_tot_kg_co2_e, total_milk_produced_kg_fpcm_per_year)

  manure_methane <- if ("emission_factor" %in% names(ghg_eft)) sum_num(ghg_eft$emission_factor) else 0
  manure_methane_tot_kg_co2_e <- manure_methane * methane
  manure_methane_per_ha_kg_co2_e <- safe_zero_div(manure_methane_tot_kg_co2_e, area_required_on_farm_ha)
  manure_methane_kg_co2_e_per_kg_fpcm <- safe_zero_div(manure_methane_tot_kg_co2_e, total_milk_produced_kg_fpcm_per_year)

  manure_direct_N2O <- if ("direct_N2O_emission" %in% names(ghg_direct_n2o)) {
    sum_num(ghg_direct_n2o$direct_N2O_emission)
  } else if ("direct_n2o_emission" %in% names(ghg_direct_n2o)) {
    sum_num(ghg_direct_n2o$direct_n2o_emission)
  } else {
    0
  }
  manure_direct_N2O_tot_kg_co2_e <- manure_direct_N2O * N2O
  manure_direct_N2O_per_ha_kg_co2_e <- safe_zero_div(manure_direct_N2O_tot_kg_co2_e, area_required_on_farm_ha)
  manure_direct_N2O_kg_co2_e_per_kg_fpcm <- safe_zero_div(manure_direct_N2O_tot_kg_co2_e, total_milk_produced_kg_fpcm_per_year)

  manure_Indirect_N2O <- if ("indirect_N2O_emission" %in% names(ghg_indirect_n2o)) {
    sum_num(ghg_indirect_n2o$indirect_N2O_emission)
  } else if ("indirect_n2o_emission" %in% names(ghg_indirect_n2o)) {
    sum_num(ghg_indirect_n2o$indirect_n2o_emission)
  } else {
    0
  }
  manure_Indirect_N2O_tot_kg_co2_e <- manure_Indirect_N2O * N2O
  manure_Indirect_N2O_per_ha_kg_co2_e <- safe_zero_div(manure_Indirect_N2O_tot_kg_co2_e, area_required_on_farm_ha)
  manure_Indirect_N2O_kg_co2_e_per_kg_fpcm <- safe_zero_div(manure_Indirect_N2O_tot_kg_co2_e, total_milk_produced_kg_fpcm_per_year)

  soil_direct_df <- if (is.list(ghg_soil_raw) && "annual_N20N_soil_direct_emission" %in% names(ghg_soil_raw)) {
    to_df(ghg_soil_raw[["annual_N20N_soil_direct_emission"]])
  } else {
    data.frame()
  }

  soil_indirect_df <- if (is.list(ghg_soil_raw) && "annual_N20N_soil_indirect_emission" %in% names(ghg_soil_raw)) {
    to_df(ghg_soil_raw[["annual_N20N_soil_indirect_emission"]])
  } else {
    data.frame()
  }

  soil_direct_pick <- function(keys) {
    if (!nrow(soil_direct_df)) return(0)
    if (!all(c("anthropogenic_N_input", "annual_N20N_direct_emission_from_managed_soil") %in% names(soil_direct_df))) return(0)
    sum_num(soil_direct_df[soil_direct_df$anthropogenic_N_input %in% keys, "annual_N20N_direct_emission_from_managed_soil"])
  }

  soil_indirect_pick <- function(keys) {
    if (!nrow(soil_indirect_df)) return(0)
    if (!all(c("anthropogenic_N_input", "annual_N20N_from_atmospheric_deposition") %in% names(soil_indirect_df))) return(0)
    sum_num(soil_indirect_df[soil_indirect_df$anthropogenic_N_input %in% keys, "annual_N20N_from_atmospheric_deposition"])
  }

  soil_direct_N2O <- soil_direct_pick(c(
    "farm_n_synthetic_fertilizer_managed_soil",
    "farm_n_from_crop_residue_managed_soil",
    "n_synthetic_fertilizer_flooded_rice",
    "n_organic_manure_flooded_rice",
    "n_from_crop_residue_flooded_rice",
    "cattle_pig_poultry_n_pasture_onfarm",
    "sheep_and_other_n_pasture_onfarm"
  ))

  soil_indirect_N2O <- soil_indirect_pick(c(
    "farm_n_synthetic_fertilizer_managed_soil"
  ))

  soil_direct_N2O_tot_kg_co2_e <- soil_direct_N2O * N2O
  soil_indirect_N2O_tot_kg_co2_e <- soil_indirect_N2O * N2O
  soil_direct_N2O_per_ha_kg_co2_e <- safe_zero_div(soil_direct_N2O_tot_kg_co2_e, area_required_on_farm_ha)
  soil_indirect_N2O_per_ha_kg_co2_e <- safe_zero_div(soil_indirect_N2O_tot_kg_co2_e, area_required_on_farm_ha)
  soil_direct_N2O_kg_co2_e_per_kg_fpcm <- safe_zero_div(soil_direct_N2O_tot_kg_co2_e, total_milk_produced_kg_fpcm_per_year)
  soil_indirect_N2O_kg_co2_e_per_kg_fpcm <- safe_zero_div(soil_indirect_N2O_tot_kg_co2_e, total_milk_produced_kg_fpcm_per_year)

  burning <- 0
  if (nrow(ghg_burn) > 0 && ncol(ghg_burn) >= 5 && "ghg_gas" %in% names(ghg_burn)) {
    burning <- sum_num(ghg_burn[ghg_burn$ghg_gas == "CO2", 5]) +
      sum_num(ghg_burn[ghg_burn$ghg_gas == "CH4", 5]) * methane +
      sum_num(ghg_burn[ghg_burn$ghg_gas == "Nox", 5]) * N2O
  }
  burning_per_ha_kg_co2_e <- safe_zero_div(burning, area_required_on_farm_ha)
  burning_kg_co2_e_per_kg_fpcm <- safe_zero_div(burning, total_milk_produced_kg_fpcm_per_year)

  rice_production_methane <- if ("annual_methane_emission" %in% names(ghg_rice)) sum_num(ghg_rice$annual_methane_emission) else 0
  rice_production_methane_tot_kg_co2_e <- rice_production_methane * methane
  rice_production_methane_per_ha_kg_co2_e <- safe_zero_div(rice_production_methane_tot_kg_co2_e, area_required_on_farm_ha)
  rice_production_methane_kg_co2_e_per_kg_fpcm <- safe_zero_div(rice_production_methane_tot_kg_co2_e, total_milk_produced_kg_fpcm_per_year)

  fertilizer_crop_df <- if (is.list(ghg_fertilizer_raw) && "fertlizer_emission_by_crop" %in% names(ghg_fertilizer_raw)) {
    to_df(ghg_fertilizer_raw[["fertlizer_emission_by_crop"]])
  } else {
    data.frame()
  }

  on_farm_fertilizer_emission <- if ("farm_fertiliser_emission" %in% names(fertilizer_crop_df)) sum_num(fertilizer_crop_df$farm_fertiliser_emission) else 0
  on_farm_fertilizer_emission_per_ha_kg_co2_e <- safe_zero_div(on_farm_fertilizer_emission, area_required_on_farm_ha)
  on_farm_fertilizer_emission_kg_co2_e_per_kg_fpcm <- safe_zero_div(on_farm_fertilizer_emission, total_milk_produced_kg_fpcm_per_year)

  # -------------------------
  # Roughages off-farm
  # -------------------------
  rough_of_Soil_direct_N2O <- soil_direct_pick(c(
    "rough_of_n_synthetic_fertilizer_managed_soil",
    "rough_of_n_from_crop_residue_managed_soil"
  ))
  rough_of_Soil_direct_N2O_tot_kg_co2_e <- rough_of_Soil_direct_N2O * N2O
  rough_of_Soil_direct_N2O_per_ha_kg_co2_e <- safe_zero_div(rough_of_Soil_direct_N2O_tot_kg_co2_e, area_required_roughages_off_farm_ha)
  rough_of_Soil_direct_N2O_kg_co2_e_per_kg_fpcm <- safe_zero_div(rough_of_Soil_direct_N2O_tot_kg_co2_e, total_milk_produced_kg_fpcm_per_year)

  rough_of_soil_indirect_N2O <- soil_indirect_pick(c(
    "rough_of_n_synthetic_fertilizer_managed_soil"
  ))
  rough_of_soil_indirect_N2O_tot_kg_co2_e <- rough_of_soil_indirect_N2O * N2O
  rough_of_soil_indirect_N2O_per_ha_kg_co2_e <- safe_zero_div(rough_of_soil_indirect_N2O_tot_kg_co2_e, area_required_roughages_off_farm_ha)
  rough_of_soil_indirect_N2O_kg_co2_e_per_kg_fpcm <- safe_zero_div(rough_of_soil_indirect_N2O_tot_kg_co2_e, total_milk_produced_kg_fpcm_per_year)

  rough_of_fertilizer_emission <- if ("rough_of_fertiliser_emission" %in% names(fertilizer_crop_df)) sum_num(fertilizer_crop_df$rough_of_fertiliser_emission) else 0
  rough_of_fertilizer_emission_per_ha_kg_co2_e <- safe_zero_div(rough_of_fertilizer_emission, area_required_roughages_off_farm_ha)
  rough_of_fertilizer_emission_kg_co2_e_per_kg_fpcm <- safe_zero_div(rough_of_fertilizer_emission, total_milk_produced_kg_fpcm_per_year)

  # -------------------------
  # Concentrates off-farm
  # -------------------------
  conc_of_Soil_direct_N2O <- soil_direct_pick(c(
    "conc_of_n_synthetic_fertilizer_managed_soil",
    "conc_of_n_from_crop_residue_managed_soil"
  ))
  conc_of_Soil_direct_N2O_tot_kg_co2_e <- conc_of_Soil_direct_N2O * N2O
  conc_of_Soil_direct_N2O_per_ha_kg_co2_e <- safe_zero_div(conc_of_Soil_direct_N2O_tot_kg_co2_e, area_required_concentrates_off_farm_ha)
  conc_of_Soil_direct_N2O_kg_co2_e_per_kg_fpcm <- safe_zero_div(conc_of_Soil_direct_N2O_tot_kg_co2_e, total_milk_produced_kg_fpcm_per_year)

  conc_of_soil_indirect_N2O <- soil_indirect_pick(c(
    "conc_of_n_synthetic_fertilizer_managed_soil"
  ))
  conc_of_soil_indirect_N2O_tot_kg_co2_e <- conc_of_soil_indirect_N2O * N2O
  conc_of_soil_indirect_N2O_per_ha_kg_co2_e <- safe_zero_div(conc_of_soil_indirect_N2O_tot_kg_co2_e, area_required_concentrates_off_farm_ha)
  conc_of_soil_indirect_N2O_kg_co2_e_per_kg_fpcm <- safe_zero_div(conc_of_soil_indirect_N2O_tot_kg_co2_e, total_milk_produced_kg_fpcm_per_year)

  conc_of_fertilizer_emission <- if ("conc_of_fertiliser_emission" %in% names(fertilizer_crop_df)) sum_num(fertilizer_crop_df$conc_of_fertiliser_emission) else 0
  conc_of_fertilizer_emission_per_ha_kg_co2_e <- safe_zero_div(conc_of_fertilizer_emission, area_required_concentrates_off_farm_ha)
  conc_of_fertilizer_emission_kg_co2_e_per_kg_fpcm <- safe_zero_div(conc_of_fertilizer_emission, total_milk_produced_kg_fpcm_per_year)

  # -------------------------
  # Imported concentrates
  # -------------------------
  conc_ip_Soil_direct_N2O <- soil_direct_pick(c(
    "conc_ip_n_synthetic_fertilizer_managed_soil",
    "conc_ip_n_from_crop_residue_managed_soil"
  ))
  conc_ip_Soil_direct_N2O_tot_kg_co2_e <- conc_ip_Soil_direct_N2O * N2O
  conc_ip_Soil_direct_N2O_per_ha_kg_co2_e <- safe_zero_div(conc_ip_Soil_direct_N2O_tot_kg_co2_e, area_required_imported_concentrates_ha)
  conc_ip_Soil_direct_N2O_kg_co2_e_per_kg_fpcm <- safe_zero_div(conc_ip_Soil_direct_N2O_tot_kg_co2_e, total_milk_produced_kg_fpcm_per_year)

  conc_ip_soil_indirect_N2O <- soil_indirect_pick(c(
    "conc_ip_n_synthetic_fertilizer_managed_soil"
  ))
  conc_ip_soil_indirect_N2O_tot_kg_co2_e <- conc_ip_soil_indirect_N2O * N2O
  conc_ip_soil_indirect_N2O_per_ha_kg_co2_e <- safe_zero_div(conc_ip_soil_indirect_N2O_tot_kg_co2_e, area_required_imported_concentrates_ha)
  conc_ip_soil_indirect_N2O_kg_co2_e_per_kg_fpcm <- safe_zero_div(conc_ip_soil_indirect_N2O_tot_kg_co2_e, total_milk_produced_kg_fpcm_per_year)

  conc_ip_fertilizer_emission <- if ("conc_ip_fertiliser_emission" %in% names(fertilizer_crop_df)) sum_num(fertilizer_crop_df$conc_ip_fertiliser_emission) else 0
  conc_ip_fertilizer_emission_per_ha_kg_co2_e <- safe_zero_div(conc_ip_fertilizer_emission, area_required_imported_concentrates_ha)
  conc_ip_fertilizer_emission_kg_co2_e_per_kg_fpcm <- safe_zero_div(conc_ip_fertilizer_emission, total_milk_produced_kg_fpcm_per_year)

  ghg_balance <- data.frame(
    GHG_balance = c("On-farm", "Enteric fermentation-Methane",
                    "Manure-Methane",
                    "Manure-Direct N2O",
                    "Manure-Indirect N2O",
                    "Soil-Direct N2O",
                    "Soil-Indirect N2O",
                    "Burning",
                    "Rice production-Methane",
                    "Production fertilizer",
                    "Roughages off-farm", "Soil-Direct N2O",
                    "Soil-Indirect N2O",
                    "Production fertilizer",
                    "Concentrates off-farm", "Soil-Direct N2O",
                    "Soil-Indirect N2O",
                    "Production fertilizer",
                    "Imported concentrates", "Soil-Direct N2O",
                    "Soil-Indirect N2O",
                    "Production fertilizer"),
    si_units = c("", "kg CH4",
                 "kg CH4",
                 "kg N2O",
                 "kg N2O",
                 "kg N2O",
                 "kg N2O",
                 "kg CO2e",
                 "kg CH4",
                 "kg CO2e",
                 "", "kg N2O",
                 "kg N2O",
                 "kg CO2e",
                 "", "kg N2O",
                 "kg N2O",
                 "kg CO2e",
                 "", "kg N2O",
                 "kg N2O",
                 "kg CO2e"),
    value = c("", enteric_fermentation_methane,
              manure_methane,
              manure_direct_N2O,
              manure_Indirect_N2O,
              soil_direct_N2O,
              soil_indirect_N2O,
              burning,
              rice_production_methane,
              on_farm_fertilizer_emission,
              "", rough_of_Soil_direct_N2O,
              rough_of_soil_indirect_N2O,
              rough_of_fertilizer_emission,
              "", conc_of_Soil_direct_N2O,
              conc_of_soil_indirect_N2O,
              conc_of_fertilizer_emission,
              "", conc_ip_Soil_direct_N2O,
              conc_ip_soil_indirect_N2O,
              conc_ip_fertilizer_emission),
    kg_co2_e_per_ha = c("", enteric_fermentation_methane_per_ha_kg_co2_e,
                        manure_methane_per_ha_kg_co2_e,
                        manure_direct_N2O_per_ha_kg_co2_e,
                        manure_Indirect_N2O_per_ha_kg_co2_e,
                        soil_direct_N2O_per_ha_kg_co2_e,
                        soil_indirect_N2O_per_ha_kg_co2_e,
                        burning_per_ha_kg_co2_e,
                        rice_production_methane_per_ha_kg_co2_e,
                        on_farm_fertilizer_emission_per_ha_kg_co2_e,
                        "", rough_of_Soil_direct_N2O_per_ha_kg_co2_e,
                        rough_of_soil_indirect_N2O_per_ha_kg_co2_e,
                        rough_of_fertilizer_emission_per_ha_kg_co2_e,
                        "", conc_of_Soil_direct_N2O_per_ha_kg_co2_e,
                        conc_of_soil_indirect_N2O_per_ha_kg_co2_e,
                        conc_of_fertilizer_emission_per_ha_kg_co2_e,
                        "", conc_ip_Soil_direct_N2O_per_ha_kg_co2_e,
                        conc_ip_soil_indirect_N2O_per_ha_kg_co2_e,
                        conc_ip_fertilizer_emission_per_ha_kg_co2_e),
    kg_co2_e_tot = c("", enteric_fermentation_methane_tot_kg_co2_e,
                     manure_methane_tot_kg_co2_e,
                     manure_direct_N2O_tot_kg_co2_e,
                     manure_Indirect_N2O_tot_kg_co2_e,
                     soil_direct_N2O_tot_kg_co2_e,
                     soil_indirect_N2O_tot_kg_co2_e,
                     burning,
                     rice_production_methane_tot_kg_co2_e,
                     on_farm_fertilizer_emission,
                     "", rough_of_Soil_direct_N2O_tot_kg_co2_e,
                     rough_of_soil_indirect_N2O_tot_kg_co2_e,
                     rough_of_fertilizer_emission,
                     "", conc_of_Soil_direct_N2O_tot_kg_co2_e,
                     conc_of_soil_indirect_N2O_tot_kg_co2_e,
                     conc_of_fertilizer_emission,
                     "", conc_ip_Soil_direct_N2O_tot_kg_co2_e,
                     conc_ip_soil_indirect_N2O_tot_kg_co2_e,
                     conc_ip_fertilizer_emission),
    kg_co2_e_per_kg_fpcm = c("", enteric_fermentation_methane_kg_co2_e_per_kg_fpcm,
                             manure_methane_kg_co2_e_per_kg_fpcm,
                             manure_direct_N2O_kg_co2_e_per_kg_fpcm,
                             manure_Indirect_N2O_kg_co2_e_per_kg_fpcm,
                             soil_direct_N2O_kg_co2_e_per_kg_fpcm,
                             soil_indirect_N2O_kg_co2_e_per_kg_fpcm,
                             burning_kg_co2_e_per_kg_fpcm,
                             rice_production_methane_kg_co2_e_per_kg_fpcm,
                             on_farm_fertilizer_emission_kg_co2_e_per_kg_fpcm,
                             "", rough_of_Soil_direct_N2O_kg_co2_e_per_kg_fpcm,
                             rough_of_soil_indirect_N2O_kg_co2_e_per_kg_fpcm,
                             rough_of_fertilizer_emission_kg_co2_e_per_kg_fpcm,
                             "", conc_of_Soil_direct_N2O_kg_co2_e_per_kg_fpcm,
                             conc_of_soil_indirect_N2O_kg_co2_e_per_kg_fpcm,
                             conc_of_fertilizer_emission_kg_co2_e_per_kg_fpcm,
                             "", conc_ip_Soil_direct_N2O_kg_co2_e_per_kg_fpcm,
                             conc_ip_soil_indirect_N2O_kg_co2_e_per_kg_fpcm,
                             conc_ip_fertilizer_emission_kg_co2_e_per_kg_fpcm),
    stringsAsFactors = FALSE
  )

  soil_on_farm <- (
    as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Direct N2O", "kg_co2_e_per_ha"][1]) +
      as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Indirect N2O", "kg_co2_e_per_ha"][1])
  ) / 1000

  soil_off_farm <- (
    sum(as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Direct N2O", "kg_co2_e_per_ha"][-1]), na.rm = TRUE) +
      sum(as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Indirect N2O", "kg_co2_e_per_ha"][-1]), na.rm = TRUE)
  ) / 1000

  livestock_manure <- (
    as.numeric(ghg_balance[ghg_balance$GHG_balance == "Manure-Methane", "kg_co2_e_per_ha"]) +
      as.numeric(ghg_balance[ghg_balance$GHG_balance == "Manure-Direct N2O", "kg_co2_e_per_ha"]) +
      as.numeric(ghg_balance[ghg_balance$GHG_balance == "Manure-Indirect N2O", "kg_co2_e_per_ha"])
  ) / 1000

  livestock_enteric_fermentation <- as.numeric(
    ghg_balance[ghg_balance$GHG_balance == "Enteric fermentation-Methane", "kg_co2_e_per_ha"]
  ) / 1000

  burning_emission <- as.numeric(
    ghg_balance[ghg_balance$GHG_balance == "Burning", "kg_co2_e_per_ha"]
  ) / 1000

  rice <- as.numeric(
    ghg_balance[ghg_balance$GHG_balance == "Rice production-Methane", "kg_co2_e_per_ha"]
  ) / 1000

  fertilizer_on_farm <- as.numeric(
    ghg_balance[ghg_balance$GHG_balance == "Production fertilizer", "kg_co2_e_per_ha"][1]
  ) / 1000

  soil_off_farm_rough <- (
    as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Direct N2O", "kg_co2_e_per_ha"][2]) +
      as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Indirect N2O", "kg_co2_e_per_ha"][2])
  ) / 1000

  fertilizer_off_farm_rough <- as.numeric(
    ghg_balance[ghg_balance$GHG_balance == "Production fertilizer", "kg_co2_e_per_ha"][2]
  ) / 1000

  soil_off_farm_conc <- (
    as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Direct N2O", "kg_co2_e_per_ha"][3]) +
      as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Indirect N2O", "kg_co2_e_per_ha"][3])
  ) / 1000

  fertilizer_off_farm_conc <- as.numeric(
    ghg_balance[ghg_balance$GHG_balance == "Production fertilizer", "kg_co2_e_per_ha"][3]
  ) / 1000

  soil_ip_farm_conc <- (
    as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Direct N2O", "kg_co2_e_per_ha"][4]) +
      as.numeric(ghg_balance[ghg_balance$GHG_balance == "Soil-Indirect N2O", "kg_co2_e_per_ha"][4])
  ) / 1000

  fertilizer_ip_farm_conc <- as.numeric(
    ghg_balance[ghg_balance$GHG_balance == "Production fertilizer", "kg_co2_e_per_ha"][4]
  ) / 1000

  on_farm_table <- data.frame(
    sources_and_sinks = c("Soil", "Off-farm Soil", "Liv. Manure", "Liv.enteric fermentation", "Burning", "Rice", "Fertilizer"),
    t_CO2e_per_ha = c(soil_on_farm, soil_off_farm, livestock_manure, livestock_enteric_fermentation, burning_emission, rice, fertilizer_on_farm),
    stringsAsFactors = FALSE
  )
  on_farm_table$t_CO2e_per_ha <- clean_num(on_farm_table$t_CO2e_per_ha)

  off_farm_table <- data.frame(
    sources_and_sinks = c("Roughages off-farm", "Soil off-farm", "Fertilizer off-farm",
                          "Concentrates off-farm", "Soil off-farm", "Fertilizer off-farm",
                          "Imported concentrates", "Soil off-farm", "Fertilizer off-farm"),
    t_CO2e_per_ha = c("", soil_off_farm_rough, fertilizer_off_farm_rough,
                      "", soil_off_farm_conc, fertilizer_off_farm_conc,
                      "", soil_ip_farm_conc, fertilizer_ip_farm_conc),
    stringsAsFactors = FALSE
  )

  global_warming_potential <- rbind(
    data.frame(sources_and_sinks = "On-farm", t_CO2e_per_ha = "", stringsAsFactors = FALSE),
    on_farm_table,
    off_farm_table
  )

  product_waste <- data.frame(
    waste = c("waste - prod", "waste - distribution", "waste - processing", "waste - consume"),
    milk = c(
      param_first("waste_production_milk"),
      param_first("waste_distribution_milk"),
      param_first("waste_processing_milk"),
      param_first("waste_consume_milk")
    ),
    meat = c(
      param_first("waste_production_meat"),
      param_first("waste_distribution_meat"),
      param_first("waste_processing_meat"),
      param_first("waste_consume_meat")
    ),
    stringsAsFactors = FALSE
  )

  legacy_land_required <- list(
    land_required = land_required_out,
    dm_required = dm_required_out,
    land_and_dm_required = land_and_dm_required
  )

  legacy_soil_impacts <- list(
    overal_soil_impact = overall_soil_impact,
    nitrogen_balance = legacy_nitrogen_balance
  )

  legacy_water_required <- list(
    water_use_per_feed_item = water_use_per_feed_item,
    water_use_for_production = water_use_for_production
  )

  legacy_livestock_productivity <- list(
    consumable_livestock_product = consumable_livestock_product,
    manure_produced = manure_produced
  )

  legacy_ghg_emission <- list(
    ghg_balance = ghg_balance,
    global_warming_potential = global_warming_potential
  )

  # ---------------------------------------------------------------------------
  # workbook
  # ---------------------------------------------------------------------------
  wb <- if (app_output_mode && file.exists(primary_excel[1])) {
    openxlsx::loadWorkbook(primary_excel[1])
  } else {
    openxlsx::createWorkbook()
  }

  if (app_output_mode) {
    add_sheet_safe(wb, "Land Required", land_required_out)
    add_sheet_safe(wb, "DM Required", dm_required_out)
    add_sheet_safe(wb, "Land and DM Required", land_and_dm_required)
    add_sheet_safe(wb, "Overall Soil Impact", overall_soil_impact)
    add_sheet_safe(wb, "Nitrogen Balance", legacy_nitrogen_balance)
    add_sheet_safe(wb, "Water Use Per Feed Item", water_use_per_feed_item)
    add_sheet_safe(wb, "Water Use For Production", water_use_for_production)
    add_sheet_safe(wb, "Consumable Livestock Product", consumable_livestock_product)
    add_sheet_safe(wb, "Manure Produced", manure_produced)
    add_sheet_safe(wb, "GHG Balance", ghg_balance)
    add_sheet_safe(wb, "Global Warming Potential", global_warming_potential)
    add_sheet_safe(wb, "Biomass", biomass)
    add_sheet_safe(wb, "Soil Carbon", soil_carbon)
    add_sheet_safe(wb, "Product Waste", product_waste)

    desired_order <- c(
      "README",
      "Land Required",
      "DM Required",
      "Land and DM Required",
      "Overall Soil Impact",
      "Nitrogen Balance",
      "Water Use Per Feed Item",
      "Water Use For Production",
      "Consumable Livestock Product",
      "Manure Produced",
      "GHG Balance",
      "Global Warming Potential",
      "Biomass",
      "Soil Carbon",
      "Product Waste",
      "Cattle Benchmark ",
      "Sheep Benchmark",
      "Goat Benchmark",
      "Camel Benchmark",
      "buffalo Benchmark",
      "pig Benchmark"
    )
  } else {
    add_sheet_safe(wb, "Land Required", land_required_out)
    add_sheet_safe(wb, "DM Required", dm_required_out)
    add_sheet_safe(wb, "Land and DM Required", land_and_dm_required)
    add_sheet_safe(wb, "Overall Soil Impact", overall_soil_impact)
    add_sheet_safe(wb, "Soil Erosion Detail", soil_erosion_detail)
    add_sheet_safe(wb, "Nitrogen Balance", nitrogen_balance_detail)
    add_sheet_safe(wb, "Water Use Per Feed Item", water_use_per_feed_item)
    add_sheet_safe(wb, "Water Use For Production", water_use_for_production)
    add_sheet_safe(wb, "Consumable Livestock Product", consumable_livestock_product)
    add_sheet_safe(wb, "Manure Produced", manure_produced)
    add_sheet_safe(wb, "GHG Balance", ghg_balance)
    add_sheet_safe(wb, "Global Warming Potential", global_warming_potential)
    add_sheet_safe(wb, "Biomass", biomass)
    add_sheet_safe(wb, "Soil Carbon", soil_carbon)
    add_sheet_safe(wb, "Product Waste", product_waste)
    add_sheet_safe(wb, "Feed Basket Quality", to_df(feed_basket_quality))
    add_sheet_safe(wb, "Energy Required Annual", energy_annual)
    add_sheet_safe(wb, "Energy Required Seasonal", energy_seasonal)
    add_sheet_safe(wb, "Land Required Feed Fractions", land_required_feed_frac)
    add_sheet_safe(wb, "Livestock Productivity", livestock_productivity)
    add_sheet_safe(wb, "GHG EF", ghg_ef)
    add_sheet_safe(wb, "GHG EFT", ghg_eft)
    add_sheet_safe(wb, "GHG N Excretion", ghg_n_excretion)
    add_sheet_safe(wb, "GHG Direct N2O", ghg_direct_n2o)
    add_sheet_safe(wb, "GHG Indirect N2O", ghg_indirect_n2o)
    add_sheet_safe(wb, "GHG Land Used", ghg_land_used)
    add_sheet_safe(wb, "GHG Burn", ghg_burn)
    add_sheet_safe(wb, "GHG Rice", ghg_rice)
    add_sheet_safe(wb, "GHG Soil", ghg_soil)
    add_sheet_safe(wb, "GHG Fertilizer Applied", ghg_fertilizer_applied)
    add_sheet_safe(wb, "GHG Fertilizer By Crop", ghg_fertilizer_by_crop)

    desired_order <- c(
      "Land Required",
      "DM Required",
      "Land and DM Required",
      "Overall Soil Impact",
      "Soil Erosion Detail",
      "Nitrogen Balance",
      "Water Use Per Feed Item",
      "Water Use For Production",
      "Consumable Livestock Product",
      "Manure Produced",
      "GHG Balance",
      "Global Warming Potential",
      "Biomass",
      "Soil Carbon",
      "Product Waste",
      "Feed Basket Quality",
      "Energy Required Annual",
      "Energy Required Seasonal",
      "Land Required Feed Fractions",
      "Livestock Productivity",
      "GHG EF",
      "GHG EFT",
      "GHG N Excretion",
      "GHG Direct N2O",
      "GHG Indirect N2O",
      "GHG Land Used",
      "GHG Burn",
      "GHG Rice",
      "GHG Soil",
      "GHG Fertilizer Applied",
      "GHG Fertilizer By Crop"
    )
  }

  existing_sheets <- names(wb)
  desired_order <- desired_order[desired_order %in% existing_sheets]
  remaining <- existing_sheets[!existing_sheets %in% desired_order]
  final_order <- c(desired_order, remaining)
  order_idx <- match(final_order, existing_sheets)
  order_idx <- order_idx[!is.na(order_idx)]

  if (length(order_idx) == length(existing_sheets)) {
    openxlsx::worksheetOrder(wb) <- order_idx
  }

  out_file <- if (grepl("\\.xlsx$", filePath, ignore.case = TRUE)) {
    sub("\\.xlsx$", " emissions.xlsx", filePath, ignore.case = TRUE)
  } else {
    paste0(filePath, ".xlsx")
  }
  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)

  # ---------------------------------------------------------------------------
  # return everything needed for scenario aggregation
  # ---------------------------------------------------------------------------
  batch_output <- list(
    land_required_summary = land_required_out,
    dmi_required = dm_required_out,
    land_dmi_required = land_and_dm_required,
    overall_soil_impact = overall_soil_impact,
    soil_erosion_detail = soil_erosion_detail,
    nitrogen_balance_detail = nitrogen_balance_detail,
    water_use_per_feed_item = water_use_per_feed_item,
    water_use_for_production = water_use_for_production,
    consumable_livestock_product = consumable_livestock_product,
    manure_produced = manure_produced,
    ghg_balance = ghg_balance,
    global_warming_potential = global_warming_potential,
    biomass = biomass,
    soil_carbon = soil_carbon,
    product_waste = product_waste,
    feed_basket_quality = to_df(feed_basket_quality),
    energy_required_annual = energy_annual,
    energy_required_seasonal = energy_seasonal,
    land_required_feed_fractions = land_required_feed_frac,
    livestock_productivity = livestock_productivity,
    ghg_ef = ghg_ef,
    ghg_eft = ghg_eft,
    ghg_n_excretion = ghg_n_excretion,
    ghg_direct_N2O = ghg_direct_n2o,
    ghg_indirect_N2O = ghg_indirect_n2o,
    ghg_land_used = ghg_land_used,
    ghg_burn = ghg_burn,
    ghg_rice = ghg_rice,
    ghg_soil = ghg_soil,
    ghg_fertilizer_applied = ghg_fertilizer_applied,
    ghg_fertilizer_by_crop = ghg_fertilizer_by_crop
  )

  legacy_output <- list(
    land_required = legacy_land_required,
    soil_impacts = legacy_soil_impacts,
    water_required = legacy_water_required,
    livestock_productivity = legacy_livestock_productivity,
    ghg_emission = legacy_ghg_emission,
    biomass = biomass,
    soil_carbon = soil_carbon,
    product_waste = product_waste
  )

  app_output <- list(
    json_output = jsonlite::toJSON(
      if (app_output_mode) legacy_output else c(legacy_output, batch_output),
      pretty = TRUE
    ),
    on_farm_table = on_farm_table,
    nitrogen_balance = nitrogen_balance_output,
    land_required = land_required_output,
    water_use_per_feed_item = water_use_per_feed_item
  )

  c(app_output, batch_output[setdiff(names(batch_output), names(app_output))])
}