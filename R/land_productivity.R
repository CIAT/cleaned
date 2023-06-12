#' @title Land productivity
#'
#' @description It computes land productivity. Products include milk and meat.
#'
#' @param para A JSON file which contains an object named \code{livestock} which contains
#' more than one arrays. Each array can contain several objects. Data elements inside
#' each array include: \code{livestock_category_code}: Livestock category code, \code{livestock_category_name}:
#' Livestock category name, \code{herd_composition}: Herd composition (number), \code{annual_growth}:
#' Average annual growth per animal (kilogram), \code{body_weight}: Average Body weight (kilogram),
#' \code{birth_interval}: Birth interval (years), \code{carcass_fraction}: Carcass fraction, \code{energy_meatcontent}:
#' Energy content meat, \code{protein_meatcontent}: Protein content meat (percent), \code{annual_milk}:
#' Average annual milk (kilogram), \code{fat_content}: Fat content milk (percent), \code{protein_milkcontent}:
#' Protein content milk (percent).
#'
#' @return dataframe
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' feed_basket_quality <- feed_quality(para)
#' energy_required <- energy_requirement(para,feed_basket_quality)
#' land_required <- land_requirement(feed_basket_quality, energy_required, para)
#' soil_erosion <- soil_health(para, land_required)
#' nitrogen_balance <- nitrogen_balance(para, land_required, soil_erosion)
#' land_productivity(para)
#' }
#'
#' @export

land_productivity <- function(para){

  livestock_df <- para[["livestock"]]

  livestock_category_names <- c(livestock_df$livetype_desc)

  livestock_production <- list()

  for (livestock in livestock_category_names){

    livestock_selected <- livestock_df[livestock_df$livetype_desc == livestock,]

    # This section was removed
    # livestock_selected <- na_if(livestock_selected, "NA") %>%
    #   as.data.frame()
    #

    livestock_selected[is.na(livestock_selected)] <- 0

    # prodution per livestock
    livestock_production[[livestock]] <- livestock_selected %>%
      mutate(livetype_name = livetype_desc,
             number = as.numeric(herd_composition),
             lwg_per_animal = as.numeric(annual_growth),
             tlu = number*as.numeric(body_weight)/250,
             parturition_interval = as.numeric(livestock_selected$birth_interval),
             total_lwg = number*lwg_per_animal,
             meat_production_animal = total_lwg*as.numeric(carcass_fraction),
             energy_kcal_year_meat = meat_production_animal*as.numeric(energy_meatcontent),
             protein_kg_year_meat = meat_production_animal*as.numeric(protein_meatcontent)/100,
             milk_production_animal = as.numeric(annual_milk),
             total_milk = as.numeric(annual_milk)*(0.337+(0.116*as.numeric(fat_content)+(0.06*as.numeric(protein_milkcontent)))),
             energy_kcal_year_milk = total_milk*as.numeric(energy_milkcontent),
             protein_kg_year_milk = total_milk*as.numeric(protein_milkcontent)/100) %>%
      select(-c(3:50))

  }

  livestock_production_all <- livestock_production %>% bind_rows()

}
