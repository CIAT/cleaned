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
  ghg_soil <- if (is.list(ghg_emission) && "ghg_soil" %in% names(ghg_emission)) to_df(ghg_emission[["ghg_soil"]]) else data.frame()
  ghg_fertilizer <- if (is.list(ghg_emission) && "ghg_fertilizer" %in% names(ghg_emission)) to_df(ghg_emission[["ghg_fertilizer"]]) else data.frame()

  # ---------------------------------------------------------------------------
  # land required summaries
  # ---------------------------------------------------------------------------
  if (nrow(land_required_all) > 0 && all(c("feed", "season_name") %in% names(land_required_all))) {

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

  consumable_livestock_product <- data.frame(
    total_milk = scalar_sum(livestock_productivity, "total_milk"),
    total_meat = scalar_sum(livestock_productivity, "meat_production_animal"),
    total_protein_milk = scalar_sum(livestock_productivity, "protein_kg_year_milk"),
    total_protein_meat = scalar_sum(livestock_productivity, "protein_kg_year_meat"),
    total_energy_milk = scalar_sum(livestock_productivity, "energy_kcal_year_milk"),
    total_energy_meat = scalar_sum(livestock_productivity, "energy_kcal_year_meat"),
    total_tlu = scalar_sum(livestock_productivity, "tlu"),
    stringsAsFactors = FALSE
  )

  manure_produced <- data.frame(
    annual_manure_produced = scalar_sum(energy_annual, "annual_manure_produced"),
    daily_manure_produced = scalar_sum(energy_annual, "daily_manure_produced"),
    manure_onfarm_grazing = scalar_sum(energy_annual, "manure_onfarm_grazing"),
    manure_collected = scalar_sum(energy_annual, "manure_collected"),
    manure_exported = scalar_sum(energy_annual, "manure_exported"),
    stringsAsFactors = FALSE
  )

  # ---------------------------------------------------------------------------
# ghg summary
# ---------------------------------------------------------------------------

# helper to find the first existing numeric column from a list of candidates
scalar_sum_any <- function(df, candidates, default = NA_real_) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(default)
  hit <- candidates[candidates %in% names(df)]
  if (!length(hit)) return(default)
  scalar_sum(df, hit[1], default = default)
}

# enteric / manure CH4
enteric_methane_emissions <- scalar_sum_any(
  ghg_ef,
  c("enteric_methane_emissions", "enteric_methane_emission", "enteric_CH4")
)

manure_methane_emissions <- scalar_sum_any(
  ghg_ef,
  c("manure_methane_emissions", "manure_methane_emission", "manure_CH4")
)

# direct / indirect N2O are usually in separate objects, not in ghg_ef
direct_n2o_emissions <- scalar_sum_any(
  ghg_direct_n2o,
  c("direct_n2o_emissions", "direct_n2o_emission", "direct_N2O_emission", "emission", "value")
)

indirect_n2o_emissions <- scalar_sum_any(
  ghg_indirect_n2o,
  c("indirect_n2o_emissions", "indirect_n2o_emission", "indirect_N2O_emission", "emission", "value")
)

total_ghg <- sum(
  c(enteric_methane_emissions,
    manure_methane_emissions,
    direct_n2o_emissions,
    indirect_n2o_emissions),
  na.rm = TRUE
)

if (!is.finite(total_ghg)) total_ghg <- NA_real_

ghg_balance <- data.frame(
  enteric_methane_emissions = enteric_methane_emissions,
  manure_methane_emissions = manure_methane_emissions,
  direct_n2o_emissions = direct_n2o_emissions,
  indirect_n2o_emissions = indirect_n2o_emissions,
  total_ghg = total_ghg,
  stringsAsFactors = FALSE
)

# GWP summary:
# use explicit GWP columns if present; otherwise derive from totals using AR6-style factors
gwp_enteric_methane <- scalar_sum_any(
  ghg_eft,
  c("gwp_enteric_methane", "gwp_enteric_CH4", "enteric_methane_gwp", "enteric_gwp")
)

gwp_manure_methane <- scalar_sum_any(
  ghg_eft,
  c("gwp_manure_methane", "gwp_manure_CH4", "manure_methane_gwp", "manure_gwp")
)

gwp_direct_n2o <- scalar_sum_any(
  ghg_eft,
  c("gwp_direct_n2o", "gwp_direct_N2O", "direct_n2o_gwp", "direct_gwp")
)

gwp_indirect_n2o <- scalar_sum_any(
  ghg_eft,
  c("gwp_indirect_n2o", "gwp_indirect_N2O", "indirect_n2o_gwp", "indirect_gwp")
)

# fallback derivation if eft does not already contain those summaries
if (is.na(gwp_enteric_methane) && !is.na(enteric_methane_emissions)) {
  gwp_enteric_methane <- enteric_methane_emissions * 27.2
}
if (is.na(gwp_manure_methane) && !is.na(manure_methane_emissions)) {
  gwp_manure_methane <- manure_methane_emissions * 27.2
}
if (is.na(gwp_direct_n2o) && !is.na(direct_n2o_emissions)) {
  gwp_direct_n2o <- direct_n2o_emissions * 273
}
if (is.na(gwp_indirect_n2o) && !is.na(indirect_n2o_emissions)) {
  gwp_indirect_n2o <- indirect_n2o_emissions * 273
}

gwp_total <- sum(
  c(gwp_enteric_methane,
    gwp_manure_methane,
    gwp_direct_n2o,
    gwp_indirect_n2o),
  na.rm = TRUE
)

if (!is.finite(gwp_total)) gwp_total <- NA_real_

global_warming_potential <- data.frame(
  gwp_enteric_methane = gwp_enteric_methane,
  gwp_manure_methane = gwp_manure_methane,
  gwp_direct_n2o = gwp_direct_n2o,
  gwp_indirect_n2o = gwp_indirect_n2o,
  gwp_total = gwp_total,
  stringsAsFactors = FALSE
)

  product_waste <- data.frame(
    manure_exported = scalar_sum(energy_annual, "manure_exported"),
    stringsAsFactors = FALSE
  )

  # ---------------------------------------------------------------------------
  # workbook
  # ---------------------------------------------------------------------------
  wb <- openxlsx::createWorkbook()

  add_sheet_safe(wb, "Land Required", land_required_out)
  add_sheet_safe(wb, "DM Required", dm_required_out)
  add_sheet_safe(wb, "Land and DM Required", land_and_dm_required)
  add_sheet_safe(wb, "Overall Soil Impact", overall_soil_impact)
  add_sheet_safe(wb, "Soil Erosion Detail", soil_erosion_detail)
  add_sheet_safe(wb, "Nitrogen Balance", nitrogen_balance)
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
  add_sheet_safe(wb, "GHG Fertilizer", ghg_fertilizer)

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
    "GHG Fertilizer"
  )

  existing_sheets <- names(wb)
  desired_order <- desired_order[desired_order %in% existing_sheets]
  remaining <- existing_sheets[!existing_sheets %in% desired_order]
  final_order <- c(desired_order, remaining)
  order_idx <- match(final_order, existing_sheets)
  order_idx <- order_idx[!is.na(order_idx)]

  if (length(order_idx) == length(existing_sheets)) {
    openxlsx::worksheetOrder(wb) <- order_idx
  }

  out_file <- sub("\\.xlsx$", " emissions.xlsx", filePath)
  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)

  # ---------------------------------------------------------------------------
  # return everything needed for scenario aggregation
  # ---------------------------------------------------------------------------
  list(
    land_required = land_required_out,
    dmi_required = dm_required_out,
    land_dmi_required = land_and_dm_required,
    overall_soil_impact = overall_soil_impact,
    soil_erosion_detail = soil_erosion_detail,
    nitrogen_balance = nitrogen_balance,
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
    ghg_fertilizer = ghg_fertilizer
  )
}
