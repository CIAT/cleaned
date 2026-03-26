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
  # Helpers
  # ---------------------------------------------------------------------------
  to_df <- function(x) {
    if (is.null(x)) return(data.frame())
    if (is.data.frame(x)) return(x)
    as.data.frame(x)
  }

  clean_num <- function(x) {
    x <- as.numeric(x)
    x[!is.finite(x)] <- NA_real_
    x
  }

  add_sheet_safe <- function(wb, sheet_name, x) {
    x <- to_df(x)
    if (sheet_name %in% names(wb)) {
      openxlsx::removeWorksheet(wb, sheet_name)
    }
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet = sheet_name, x = x)
    invisible(NULL)
  }

  # ---------------------------------------------------------------------------
  # 1) Derived outputs
  # ---------------------------------------------------------------------------

  # DM required
  DMI_required <- land_required[["land_requirements_all"]] %>%
    dplyr::group_by(feed, season_name) %>%
    dplyr::summarise(
      feed_item_dm = sum(feed_item_dm, na.rm = TRUE),
      rough_of_dm  = sum(rough_of_dm,  na.rm = TRUE),
      conc_of_dm   = sum(conc_of_dm,   na.rm = TRUE),
      conc_ip_dm   = sum(conc_ip_dm,   na.rm = TRUE),
      farm_dm      = sum(farm_dm,      na.rm = TRUE),
      grasses_dm   = sum(grasses_dm,   na.rm = TRUE),
      tree_legume_dm = sum(tree_legume_dm, na.rm = TRUE),
      .groups = "drop"
    )

  # Land required
  land_required_df <- land_required[["land_requirements_all"]] %>%
    dplyr::group_by(feed, season_name) %>%
    dplyr::summarise(
      area_total   = sum(area_total,   na.rm = TRUE),
      area_non_feed = sum(area_non_feed, na.rm = TRUE),
      area_feed    = sum(area_feed,    na.rm = TRUE),
      rough_of     = sum(rough_of,     na.rm = TRUE),
      conc_of      = sum(conc_of,      na.rm = TRUE),
      conc_ip      = sum(conc_ip,      na.rm = TRUE),
      farm         = sum(farm,         na.rm = TRUE),
      grasses      = sum(grasses,      na.rm = TRUE),
      tree_legume  = sum(tree_legume,  na.rm = TRUE),
      .groups = "drop"
    )

  # Land + DM required
  land_dmi_required <- dplyr::left_join(
    land_required_df,
    DMI_required,
    by = c("feed", "season_name")
  )

  # Consumable livestock product
  total_milk <- sum(clean_num(livestock_productivity$total_milk), na.rm = TRUE)
  total_protein <- sum(clean_num(livestock_productivity$protein_milk), na.rm = TRUE)
  total_energy <- sum(clean_num(livestock_productivity$energy_kcal_year_milk), na.rm = TRUE)

  total_area_used_for_feed_production_ha <- sum(clean_num(land_required_df$area_feed), na.rm = TRUE)
  total_dm_used_for_feed_production_kg <- sum(clean_num(DMI_required$feed_item_dm), na.rm = TRUE)

  consumable_livestock_product <- data.frame(
    total_milk = total_milk,
    total_protein = total_protein,
    total_energy_kcal = total_energy,
    area_required_per_milk_unit = ifelse(total_milk > 0, total_area_used_for_feed_production_ha / total_milk, 0),
    dm_required_per_milk_unit   = ifelse(total_milk > 0, total_dm_used_for_feed_production_kg / total_milk, 0)
  )

  # Manure produced
  manure_produced <- data.frame(
    annual_manure_produced = sum(clean_num(energy_required$annual_results$annual_manure_produced), na.rm = TRUE),
    daily_manure_produced  = sum(clean_num(energy_required$annual_results$daily_manure_produced),  na.rm = TRUE),
    manure_onfarm_grazing  = sum(clean_num(energy_required$annual_results$manure_onfarm_grazing),  na.rm = TRUE),
    manure_collected       = sum(clean_num(energy_required$annual_results$manure_collected),       na.rm = TRUE),
    manure_exported        = sum(clean_num(energy_required$annual_results$manure_exported),        na.rm = TRUE)
  )

  # GHG balance
  ghg_balance <- if (!is.null(ghg_emission$ef)) {
    data.frame(
      enteric_methane_emissions = sum(clean_num(ghg_emission$ef$enteric_methane_emissions), na.rm = TRUE),
      manure_methane_emissions  = sum(clean_num(ghg_emission$ef$manure_methane_emissions),  na.rm = TRUE),
      direct_n2o_emissions      = sum(clean_num(ghg_emission$ef$direct_n2o_emissions),      na.rm = TRUE),
      indirect_n2o_emissions    = sum(clean_num(ghg_emission$ef$indirect_n2o_emissions),    na.rm = TRUE),
      total_ghg                 = sum(clean_num(ghg_emission$ef$total_ghg),                 na.rm = TRUE)
    )
  } else {
    data.frame()
  }

  # Global warming potential
  global_warming_potential <- if (!is.null(ghg_emission$eft)) {
    data.frame(
      gwp_enteric_methane = sum(clean_num(ghg_emission$eft$gwp_enteric_methane), na.rm = TRUE),
      gwp_manure_methane  = sum(clean_num(ghg_emission$eft$gwp_manure_methane),  na.rm = TRUE),
      gwp_direct_n2o      = sum(clean_num(ghg_emission$eft$gwp_direct_n2o),      na.rm = TRUE),
      gwp_indirect_n2o    = sum(clean_num(ghg_emission$eft$gwp_indirect_n2o),    na.rm = TRUE),
      gwp_total           = sum(clean_num(ghg_emission$eft$gwp_total),           na.rm = TRUE)
    )
  } else {
    data.frame()
  }

  # Product waste
  product_waste <- data.frame(
    manure_exported = sum(clean_num(energy_required$annual_results$manure_exported), na.rm = TRUE)
  )

  # ---------------------------------------------------------------------------
  # 2) Create a fresh workbook
  # ---------------------------------------------------------------------------
  wb <- openxlsx::createWorkbook()

  # ---------------------------------------------------------------------------
  # 3) Add all sheets
  # ---------------------------------------------------------------------------
  add_sheet_safe(wb, "Land Required", land_required_df)
  add_sheet_safe(wb, "DM Required", DMI_required)
  add_sheet_safe(wb, "Land and DM Required", land_dmi_required)
  add_sheet_safe(wb, "Overall Soil Impact", soil_erosion)
  add_sheet_safe(wb, "Nitrogen Balance", nitrogen_balance)

  if (!is.null(water_required$water_use_per_feed_item)) {
    add_sheet_safe(wb, "Water Use Per Feed Item", water_required$water_use_per_feed_item)
  } else {
    add_sheet_safe(wb, "Water Use Per Feed Item", data.frame())
  }

  if (!is.null(water_required$water_use_for_production)) {
    add_sheet_safe(wb, "Water Use For Production", water_required$water_use_for_production)
  } else {
    add_sheet_safe(wb, "Water Use For Production", data.frame())
  }

  add_sheet_safe(wb, "Consumable Livestock Product", consumable_livestock_product)
  add_sheet_safe(wb, "Manure Produced", manure_produced)
  add_sheet_safe(wb, "GHG Balance", ghg_balance)
  add_sheet_safe(wb, "Global Warming Potential", global_warming_potential)
  add_sheet_safe(wb, "Biomass", biomass)
  add_sheet_safe(wb, "Soil Carbon", soil_carbon)
  add_sheet_safe(wb, "Product Waste", product_waste)

  # Optional raw outputs if present
  if (!is.null(feed_basket_quality)) {
    add_sheet_safe(wb, "Feed Basket Quality", feed_basket_quality)
  }
  if (!is.null(energy_required$annual_results)) {
    add_sheet_safe(wb, "Energy Required Annual", energy_required$annual_results)
  }
  if (!is.null(energy_required$seasonal_results)) {
    add_sheet_safe(wb, "Energy Required Seasonal", energy_required$seasonal_results)
  }
  if (!is.null(land_required$feed_items_frac)) {
    add_sheet_safe(wb, "Land Required Feed Fractions", land_required$feed_items_frac)
  }
  if (!is.null(livestock_productivity)) {
    add_sheet_safe(wb, "Livestock Productivity", livestock_productivity)
  }
  if (!is.null(ghg_emission$ef)) {
    add_sheet_safe(wb, "GHG EF", ghg_emission$ef)
  }
  if (!is.null(ghg_emission$eft)) {
    add_sheet_safe(wb, "GHG EFT", ghg_emission$eft)
  }
  if (!is.null(ghg_emission$n_excretion)) {
    add_sheet_safe(wb, "GHG N Excretion", ghg_emission$n_excretion)
  }
  if (!is.null(ghg_emission$direct_N2O)) {
    add_sheet_safe(wb, "GHG Direct N2O", ghg_emission$direct_N2O)
  }
  if (!is.null(ghg_emission$indirect_N2O)) {
    add_sheet_safe(wb, "GHG Indirect N2O", ghg_emission$indirect_N2O)
  }
  if (!is.null(ghg_emission$land_used)) {
    add_sheet_safe(wb, "GHG Land Used", ghg_emission$land_used)
  }
  if (!is.null(ghg_emission$ghg_burn)) {
    add_sheet_safe(wb, "GHG Burn", ghg_emission$ghg_burn)
  }
  if (!is.null(ghg_emission$ghg_rice)) {
    add_sheet_safe(wb, "GHG Rice", ghg_emission$ghg_rice)
  }

 # ---------------------------------------------------------------------------
# 4) Dynamic worksheet order (SAFE VERSION)
# ---------------------------------------------------------------------------
desired_order <- c(
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
  "GHG Rice"
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

  # ---------------------------------------------------------------------------
  # 5) Save workbook
  # ---------------------------------------------------------------------------
  out_file <- sub("\\.xlsx$", " emissions.xlsx", filePath)
  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)

  # ---------------------------------------------------------------------------
  # 6) Return all outputs for downstream aggregation
  # ---------------------------------------------------------------------------
  list(
    land_required = land_required_df,
    dmi_required = DMI_required,
    land_dmi_required = land_dmi_required,
    soil_erosion = to_df(soil_erosion),
    nitrogen_balance = to_df(nitrogen_balance),
    water_use_per_feed_item = to_df(water_required$water_use_per_feed_item),
    water_use_for_production = to_df(water_required$water_use_for_production),
    consumable_livestock_product = consumable_livestock_product,
    manure_produced = manure_produced,
    ghg_balance = ghg_balance,
    global_warming_potential = global_warming_potential,
    biomass = to_df(biomass),
    soil_carbon = to_df(soil_carbon),
    product_waste = product_waste,
    feed_basket_quality = to_df(feed_basket_quality),
    energy_required_annual = to_df(energy_required$annual_results),
    energy_required_seasonal = to_df(energy_required$seasonal_results),
    land_required_feed_fractions = to_df(land_required$feed_items_frac),
    livestock_productivity = to_df(livestock_productivity),
    ghg_ef = to_df(ghg_emission$ef),
    ghg_eft = to_df(ghg_emission$eft),
    ghg_n_excretion = to_df(ghg_emission$n_excretion),
    ghg_direct_N2O = to_df(ghg_emission$direct_N2O),
    ghg_indirect_N2O = to_df(ghg_emission$indirect_N2O),
    ghg_land_used = to_df(ghg_emission$land_used),
    ghg_burn = to_df(ghg_emission$ghg_burn),
    ghg_rice = to_df(ghg_emission$ghg_rice)
  )
}