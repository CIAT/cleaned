#' @title N balance
#'
#' @description It computes nitrogen balance
#'
#' @param para A JSON file containing user inputs
#'
#' @param land_required A list computed using the `land_requirement` function
#'
#' @param soil_erosion A dataframe computed using the `soil_health` function
#'
#' @return dataframe
#'
#' @importFrom dplyr summarise
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' feed_basket_quality <- feed_quality(para)
#' energy_required <- energy_requirement(para,feed_basket_quality)
#' land_required <- land_requirement(feed_basket_quality, energy_required, para)
#' soil_erosion <- soil_health(para, land_required)
#' n_balance(para, land_required, soil_erosion)
#' }
#'
#' @export

n_balance <- function(para, land_required, soil_erosion){

  feed_types <- unique(land_required[["land_requirements_all"]]$feed)

  n_balance <- list()

  for (feed in feed_types){

    feed_production <- unnest(para[["feed_items"]], cols = c(feed_type_name))

    feed_selected_frac <- land_required[["feed_items_frac"]] %>%
      as.data.frame() %>%
      dplyr::filter(feed_item_name == feed)

    feed_selected <- feed_production[feed_production$feed_item_name == feed,]

    # feed_production <- unnest(para[["feed_production"]], cols = c(feed_type_name))
    #
    # feed_production <- na_if(feed_production, "NA") %>%
    #   as.data.frame()
    #
    # feed_production[is.na(feed_production)] <- 0
    #
    # feed_selected <- feed_production[feed_production$feed_type_name == feed,]

    dry_yield <- feed_selected_frac$dry_yield

    residue_dry_yield <- feed_selected_frac$residue_dry_yield

    main_n <- feed_selected_frac$main_n

    residue_n <- as.numeric(feed_selected$residue_n)

    n_fixing <- ifelse(feed_selected$category == "Legume", 0.5*(residue_n*residue_dry_yield+main_n*dry_yield)*1000, 0)


    feed_selected_land_required <- land_required[["land_requirements_all"]][land_required[["land_requirements_all"]]$feed == feed,]

    area_total <- sum(feed_selected_land_required$area_feed)

    # feed_item_selected <- as.data.frame(feed_selected[["feed_items"]])
    #
    # feed_item_selected <- na_if(feed_item_selected, "NA") %>%
    #   as.data.frame()
    #
    # feed_item_selected[is.na(feed_item_selected)] <- 0

    manure_fraction <- as.numeric(feed_selected$fraction_as_fertilizer)

    # Computing fertilizer rate
    if(
      length(para[["fertilizer"]]) == 0
    ) {
      ammonia_n_frac <- 0
      ammonium_nitrate_n_frac <- 0
      ammonium_sulfate_n_frac <- 0
      dap_n_frac <- 0
      n_solutions_n_frac <- 0
      npk_n_frac <- 0
      urea_n_frac <- 0
      fertilizer_rate <- 0
    } else {
      ammonia_n_frac <- ifelse(feed_selected$ammonia==0,0,
                               as.numeric(para[["fertilizer"]][which(para[["fertilizer"]]$fertilizer_desc == "Ammonia"),]$fraction))
      ammonia_n_frac <- ifelse(!is.finite(ammonia_n_frac),0,ammonia_n_frac)

      ammonium_nitrate_n_frac <- ifelse(feed_selected$ammonium_nitrate==0,0,
                                        as.numeric(para[["fertilizer"]][which(para[["fertilizer"]]$fertilizer_desc == "Ammonium nitrate"),]$fraction))
      ammonium_nitrate_n_frac <- ifelse(!is.finite(ammonium_nitrate_n_frac),0,ammonium_nitrate_n_frac)

      ammonium_sulfate_n_frac <- ifelse(feed_selected$ammonium_sulfate==0,0,
                                        as.numeric(para[["fertilizer"]][which(para[["fertilizer"]]$fertilizer_desc == "Ammonium sulfate"),]$fraction))
      ammonium_sulfate_n_frac <- ifelse(!is.finite(ammonium_sulfate_n_frac),0,ammonium_sulfate_n_frac)

      dap_n_frac <- ifelse(feed_selected$dap==0,0,
                           as.numeric(para[["fertilizer"]][which(para[["fertilizer"]]$fertilizer_desc == "DAP"),]$fraction))
      dap_n_frac <- ifelse(!is.finite(dap_n_frac),0,dap_n_frac)

      n_solutions_n_frac <- ifelse(feed_selected$n_solutions==0,0,
                                   as.numeric(para[["fertilizer"]][which(para[["fertilizer"]]$fertilizer_desc == "N solutions"),]$fraction))
      n_solutions_n_frac <- ifelse(!is.finite(n_solutions_n_frac),0,n_solutions_n_frac)

      npk_n_frac <- ifelse(feed_selected$npk==0,0,
                           as.numeric(para[["fertilizer"]][which(para[["fertilizer"]]$fertilizer_desc == "NPK"),]$fraction))
      npk_n_frac <- ifelse(!is.finite(npk_n_frac),0,npk_n_frac)

      urea_n_frac <- ifelse(feed_selected$urea==0,0,
                            as.numeric(para[["fertilizer"]][which(para[["fertilizer"]]$fertilizer_desc == "Urea"),]$fraction))
      urea_n_frac <- ifelse(!is.finite(urea_n_frac),0,urea_n_frac)
    }

    fertilizer_rate <- (feed_selected$ammonia*ammonia_n_frac)+(feed_selected$ammonium_nitrate*ammonium_nitrate_n_frac)+(feed_selected$ammonium_sulfate*ammonium_sulfate_n_frac)+
      (feed_selected$dap*dap_n_frac)+(feed_selected$n_solutions*n_solutions_n_frac)+(feed_selected$npk*npk_n_frac)+(feed_selected$urea*urea_n_frac)

    main_product_removal <- as.numeric(feed_selected$main_product_removal)

    residue_removal <- as.numeric(feed_selected$residue_removal)

    sum_n_content_manure_grazing <- energy_required[["annual_results"]] %>%
      as.data.frame() %>%
      summarise(sum(n_content_manure_grazing)) %>%
      as.numeric()

    yield_dm_ha <- as.numeric(dry_yield)*1000

    main_product_removed_kg_ha <- yield_dm_ha*main_product_removal

    n_content_manure_collected <- energy_required[["annual_results"]] %>%
      as.data.frame() %>%
      summarise(sum(n_content_manure_collected)) %>%
      as.numeric()

    animal_manure_collected <- n_content_manure_collected*manure_fraction

    organic_n_imported <- manure_fraction*(as.numeric(para$purchased_manure)+as.numeric(para$purchased_compost)+as.numeric(para$purchased_organic_n)+as.numeric(para$purchased_bedding))

    crop_residue_dm_ha <- as.numeric(feed_selected_frac$residue_dry_yield)*1000

    residue_removal <- as.numeric(feed_selected$residue_removal)

    main_product_removed_kg <- area_total*main_product_removed_kg_ha

    residue_removed_dm_ha <- crop_residue_dm_ha*residue_removal

    residue_removed_kg <- area_total*residue_removed_dm_ha

    annual_precipitation <- as.numeric(para[["annual_prec"]])

    soil_n <- as.numeric(para[["soil_n"]])

    ntot_kg_ha_20cm <- soil_n*20*as.numeric(para[["soil_bulk"]])*10

    n_mineralized_kg_ha_year <- ntot_kg_ha_20cm*0.03

    # Soil type
    soil_type <- para[["soil_description"]]

    # Soil carbon
    soil_c <- as.numeric(para[["soil_c"]])

    # Soil clay
    soil_clay <- as.numeric(para[["soil_clay"]])

    # N content (kg N/kg DM)
    ncrop <- as.numeric(feed_selected$main_n)

    # N content (kg N /kg DM)
    nres <- as.numeric(feed_selected$residue_n)

    # Mineral fertilizer
    in1 <- area_total*fertilizer_rate

    # calculate in2

    # Atmospheric deposition
    in3 <- 0.14*sqrt(annual_precipitation)*area_total

    # Non-symbiotic N fixation
    in4a <- ifelse(area_total > 0, (2 + (annual_precipitation - 1350) * 0.005) * area_total, 0)

    # Symbiotic N-fixation
    in4b <- n_fixing * area_total

    # Crop yield  (kgN)
    out1 <- area_total*main_product_removed_kg_ha*ncrop

    # Crop residue (KgN)
    out2 <- ifelse(feed_selected$source_type == "Main", 0, residue_removed_dm_ha * nres * area_total)

    # soil loss per plot per feed type
    soil_loss_plot <- sum(as.numeric(soil_erosion[soil_erosion$feed_type == feed,]$soil_loss_plot),na.rm = T)

    # Soil erosion
    out5 <- soil_loss_plot*soil_n*1.5

    # N content (kgN/kg DM ) from GHG parameters
    nfertilizer <- 0 # to fox from GHG


    # write data into a dataframe
    n_balance[[feed]] <- as.data.frame(cbind(feed,
                                             n_fixing,
                                             area_total,
                                             fertilizer_rate,
                                             sum_n_content_manure_grazing,
                                             animal_manure_collected,
                                             organic_n_imported,
                                             yield_dm_ha,
                                             crop_residue_dm_ha,
                                             residue_removal,
                                             main_product_removal,
                                             main_product_removed_kg_ha,
                                             main_product_removed_kg,
                                             residue_removed_dm_ha,
                                             residue_removed_kg,
                                             annual_precipitation,
                                             soil_n,
                                             ntot_kg_ha_20cm,
                                             n_mineralized_kg_ha_year,
                                             soil_type,
                                             soil_c,
                                             soil_clay,
                                             ncrop,
                                             nres,
                                             in1,
                                             in3,
                                             in4a,
                                             in4b,
                                             out1,
                                             out2,
                                             out5, nfertilizer))


  }

  n_balance_all <- n_balance %>%
    bind_rows() %>%
    mutate_at(c(-1, -20), as.numeric)

  # Animal manure (N kg) grazing, Organic N (kg N) total, Organic N (kg N/ha) total
  n_balance_all <- n_balance_all %>%
    mutate(animal_manure_grazing = sum_n_content_manure_grazing * (main_product_removed_kg+residue_removed_kg)/(sum(n_balance_all$main_product_removed_kg)+sum(n_balance_all$residue_removed_kg)),
           organic_n_kg_total = animal_manure_grazing+animal_manure_collected+organic_n_imported,
           organic_n_kg_per_ha = ifelse(is.na(organic_n_kg_total/area_total), 0, organic_n_kg_total/area_total)) %>%
    dplyr::mutate_if(is.numeric, list(~na_if(.,Inf))) %>%
    replace(is.na(.), 0)

  # Manure (kgN)
  n_balance_all$in2 <- n_balance_all$organic_n_kg_total

  # N leached (kg N/ha/yr) @clay < 35%, >35% and <55%, >55%, Gaseous losses
  n_balance_all <- n_balance_all %>%
    mutate(out3a = (n_mineralized_kg_ha_year + fertilizer_rate + in2) * (0.021 * (annual_precipitation - 3.9) / 100),
           out3b = (n_mineralized_kg_ha_year + fertilizer_rate + in2)* (0.014 * annual_precipitation + 0.71) / 100,
           out3c = (n_mineralized_kg_ha_year + fertilizer_rate + in2) * (0.0071 * annual_precipitation + 5.4) / 100,
           out3 = ifelse(soil_clay <35, out3a, ifelse(soil_clay >= 55, out3c, out3b)),
           out4 = (n_mineralized_kg_ha_year + fertilizer_rate + organic_n_kg_per_ha) * (-9.4 + 0.13 * soil_clay + 0.01 * annual_precipitation) / 100 * area_total,
           nin = ifelse(area_total>0, in1+in2+in3+in4a+in4b, 0),
           nout = ifelse(area_total>0, out1+out2+out3+out4+out5, 0),
           nue = ifelse(is.na(nout/nin), 0, nout/nin),
           nbalance_kg_n_total = nin-nout,
           nbalance_kg_n_ha_total = ifelse(is.na(nbalance_kg_n_total/area_total), 0, nbalance_kg_n_total/area_total),
           nbalance_feed_only_kg_n = ifelse(nbalance_kg_n_total==0, 0, ifelse(out2==0, nbalance_kg_n_total, nbalance_kg_n_total*out2/(out2+out1))),
           nbalance_feed_only_kg_n_ha = ifelse(is.na(nbalance_feed_only_kg_n/area_total), 0, nbalance_feed_only_kg_n/area_total)) %>%
    dplyr::mutate_if(is.numeric, list(~na_if(.,Inf))) %>%
    replace(is.na(.), 0)%>%
    mutate(rough_of_kg_n = ifelse(stringr::str_detect(feed, "OFR"), nbalance_feed_only_kg_n, 0),
           conc_of_kg_n = ifelse(stringr::str_detect(feed, "OFC"), nbalance_feed_only_kg_n, 0),
           conc_ip_kg_n = ifelse(stringr::str_detect(feed, "IP"), nbalance_feed_only_kg_n, 0),
           farm_kg_n = (nbalance_feed_only_kg_n - rough_of_kg_n - conc_of_kg_n - conc_ip_kg_n),
           rough_of_kg_n_ha = ifelse(stringr::str_detect(feed, "OFR"), nbalance_feed_only_kg_n_ha, 0),
           conc_of_kg_n_ha = ifelse(stringr::str_detect(feed, "OFC"), nbalance_feed_only_kg_n_ha, 0),
           conc_ip_kg_n_ha = ifelse(stringr::str_detect(feed, "IP"), nbalance_feed_only_kg_n_ha, 0),
           farm_kg_n_ha = (nbalance_feed_only_kg_n_ha - rough_of_kg_n_ha - conc_of_kg_n_ha - conc_ip_kg_n_ha),
           rough_of_nue = ifelse(stringr::str_detect(feed, "OFR"), nue, 0),
           conc_of_nue = ifelse(stringr::str_detect(feed, "OFC"), nue, 0),
           conc_ip_nue = ifelse(stringr::str_detect(feed, "IP"), nue, 0),
           farm_nue = (nue - rough_of_nue - conc_of_nue - conc_ip_nue),
           rough_of_area = ifelse(stringr::str_detect(feed, "OFR"), area_total, 0),
           conc_of_area = ifelse(stringr::str_detect(feed, "OFC"), area_total, 0),
           conc_ip_area = ifelse(stringr::str_detect(feed, "IP"), area_total, 0),
           farm_area = (area_total - rough_of_area - conc_of_area - conc_ip_area),
           area_mining = ifelse(nue>0.9,area_total,0),
           farm_area_mining = ifelse(farm_nue>0.9,farm_area,0),
           rough_of_area_mining = ifelse(rough_of_nue>0.9,rough_of_area,0),
           conc_of_nue_area_mining = ifelse(conc_of_nue>0.9,conc_of_area,0),
           conc_ip_nue_area_mining = ifelse(conc_ip_nue>0.9,conc_ip_area,0),
           area_leaching = ifelse(nue<0.5,area_total,0),
           farm_area_leaching = ifelse(farm_nue<0.5,farm_area,0),
           rough_of_area_leaching = ifelse(rough_of_nue<0.5,rough_of_area,0),
           conc_of_nue_area_leaching = ifelse(conc_of_nue<0.5,conc_of_area,0),
           conc_ip_nue_area_leaching = ifelse(conc_ip_nue<0.5,conc_ip_area,0),
           rough_of_min_fert_n = ifelse(stringr::str_detect(feed, "OFR"), in1, 0),
           conc_of_min_fert_n = ifelse(stringr::str_detect(feed, "OFC"), in1, 0),
           conc_ip_min_fert_n = ifelse(stringr::str_detect(feed, "IP"), in1, 0),
           farm_min_fert_n = (in1 - rough_of_min_fert_n - conc_of_min_fert_n - conc_ip_min_fert_n))

  # arrange values
  n_balance_all <- n_balance_all %>%
    select(feed,
           n_fixing,
           area_total,
           fertilizer_rate,
           animal_manure_grazing,
           animal_manure_collected,
           organic_n_imported,
           organic_n_kg_total,
           organic_n_kg_per_ha,
           yield_dm_ha,
           crop_residue_dm_ha,
           residue_removal,
           main_product_removal,
           main_product_removed_kg_ha,
           main_product_removed_kg,
           residue_removed_dm_ha,
           residue_removed_kg,
           annual_precipitation,
           soil_n,
           ntot_kg_ha_20cm,
           n_mineralized_kg_ha_year,
           soil_c,
           soil_clay,
           nfertilizer,
           ncrop,
           nres,
           in1,
           in2,
           in3,
           in4a,
           in4b,
           out1,
           out2,
           out3a,
           out3b,
           out3c,
           soil_type,
           out3,
           out4,
           all_of(out5),
           nin,
           nout,
           nue,
           nbalance_kg_n_total,
           nbalance_kg_n_ha_total,
           nbalance_feed_only_kg_n,
           nbalance_feed_only_kg_n_ha,
           rough_of_kg_n,
           conc_of_kg_n,
           conc_ip_kg_n,
           farm_kg_n,
           rough_of_kg_n_ha,
           conc_of_kg_n_ha,
           conc_ip_kg_n_ha,
           farm_kg_n_ha,
           rough_of_nue,
           conc_of_nue,
           conc_ip_nue,
           farm_nue,
           rough_of_area,
           conc_of_area,
           conc_ip_area,
           farm_area,
           area_mining,
           farm_area_mining,
           rough_of_area_mining,
           conc_of_nue_area_mining,
           conc_ip_nue_area_mining,
           area_leaching,
           farm_area_leaching,
           rough_of_area_leaching,
           conc_of_nue_area_leaching,
           conc_ip_nue_area_leaching,
           rough_of_min_fert_n,
           conc_of_min_fert_n,
           conc_ip_min_fert_n,
           farm_min_fert_n)


}
