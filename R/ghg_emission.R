#' @title Greenhouse gas emissions
#'
#' @description It computes greenhouse gas emissions.
#'
#' @param para A JSON file
#'
#' @param ghg_ipcc_data A JSON file
#'
#' @param land_required A dataframe computed using the `land_requirement` function
#'
#' @param energy_required A list computed using the `energy_required` function
#'
#' @param nitrogen_balance A dataframe computed using the `n_balance` function
#'
#' @return list
#'
#' @importFrom dplyr summarise mutate rename filter
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' data(ghg_para)
#' feed_basket_quality <- feed_quality(para)
#' energy_required <- energy_requirement(para,feed_basket_quality)
#' land_required <- land_requirement(feed_basket_quality, energy_required, para)
#' soil_erosion <- soil_health(para, land_required)
#' nitrogen_balance <- n_balance(para, land_required, soil_erosion)
#' livestock_productivity <- land_productivity(para)
#' economics <- economics_payback(para, energy_required)
#' biomass <- biomass_calculation(para, land_required)
#' ghg_emission(para,energy_required,ghg_ipcc_data,land_required,nitrogen_balance)
#' }
#'
#' @export

ghg_emission <- function(para, energy_required, ghg_ipcc_data, land_required, nitrogen_balance){

  livestock <- para[["livestock"]]

  seasons <- para[["seasons"]]

  no_days <- 365

  annual_energy <- energy_required[["annual_results"]]


  ##########################################################################################################################
  #Computing methane emission from enteric fermentation
  #Computing feed digestibility (DE)
  de1 <- annual_energy%>%
    mutate(de = de_intake/ge_intake)%>%
    select(livestock_category_code,livestock_category_name,de,de_intake,ge_intake,er_growth)

  cp1 <- feed_basket_quality%>%
    gather(feed,value,-season_name,-livestock_category_code,-livestock_category_name,-feed_variables)%>%
    spread(feed_variables,value)%>%
    mutate(cp_by_frac_fed = fraction_as_fed*cp_content_fresh*100,
           dm_by_frac_fed = fraction_as_fed*dm_content*100)%>%
    group_by(season_name,livestock_category_code,livestock_category_name)%>%
    summarise(cp_by_frac_fed2 = sum(cp_by_frac_fed,na.rm = TRUE),
              dm_by_frac_fed2 = sum(dm_by_frac_fed,na.rm = TRUE))%>%
    left_join(seasons, by = "season_name")%>%
    mutate(season_cp = (cp_by_frac_fed2*(season_length/no_days))/(dm_by_frac_fed2*0.01))%>%
    group_by(livestock_category_code,livestock_category_name)%>%
    summarise(cp = sum(season_cp,na.rm = TRUE))

  #Selecting methane conversion factor (Ym)
  table_10.12 <- ghg_ipcc_data[["Table 10.12"]]
  table_10.13 <- ghg_ipcc_data[["Table 10.13"]]


  `%gle%` <- function(e1, e2) {
    stopifnot(identical(length(e2), 2L))
    stopifnot(!anyNA(e2))

    e1 > min(e2) & e1 <= max(e2)
  }

  `%gel%` <- function(e1, e2) {
    stopifnot(identical(length(e2), 2L))
    stopifnot(!anyNA(e2))

    e1 >= min(e2) & e1 < max(e2)
  }

  ym1 <- left_join(livestock,de1, by = c("livetype_code"="livestock_category_code"))%>%
    mutate(ym = ifelse(ipcc_ef_category_t2 == "Dairy cows"& annual_milk > 8500 & de >= 0.7,table_10.12$Ym[1],
                       ifelse(ipcc_ef_category_t2 == "Dairy cows"& annual_milk %gle% c(5000,8500) & de %gel% c(0.63,0.7),table_10.12$Ym[2],
                              ifelse(ipcc_ef_category_t2 == "Dairy cows"& annual_milk <= 5000 & de <= 0.62,table_10.12$Ym[3],
                                     ifelse(ipcc_ef_category_t2 == "Non-dairy" & de <= 0.62,table_10.12$Ym[4],
                                            ifelse(ipcc_ef_category_t2 == "Non-dairy" & de < 0.72 & de > 0.62,table_10.12$Ym[5],
                                                   ifelse(ipcc_ef_category_t2 == "Non-dairy" & de >= 0.72 & de < 0.75,table_10.12$Ym[6],
                                                          ifelse(ipcc_ef_category_t2 == "Non-dairy" & de >= 0.75,table_10.12$Ym[7],
                                                                 ifelse(ipcc_ef_category_t2 == "Sheep",table_10.13$Ym[1],
                                                                        ifelse(ipcc_ef_category_t2 == "Goats",table_10.13$Ym[2],0))))))))))

  #Computing methane enteric emission factor

  ef <- ym1%>%
    mutate(enteric_methane_emissions = (ge_intake*(ym/100)*no_days)/55.65) #equation 10.21

  ############################################################################################################################
  #Computing methane emission from manure management T2
  #Computing volatile solid excretion
  table_m <- ghg_ipcc_data[["Table_m"]]

  vs <- left_join(de1,table_m,by = "livestock_category_name")%>%
    mutate(volatile_solid_excretion = (((ge_intake/no_days)*(1-de))+(Urinary_energy_frac*(ge_intake/no_days)))*((1-ash_content)/18.45)) #equation 10.24

  #Extracting Bo
  table_10.16 <- ghg_ipcc_data[["Table 10.16"]]
  table_10.16$Bo <- table_10.16[,4]

  region <- para[["region"]]

  productivity1 <-  ym1%>%
    mutate(productivity=ifelse(ipcc_meth_man_category == "Dairy cows"& annual_milk > 8500 & de >= 0.7,"High  productivity systems",
                        ifelse(ipcc_meth_man_category == "Dairy cows"& annual_milk %gle% c(5000,8500) & de %gel% c(0.63,0.7),"High  productivity systems",
                               ifelse(ipcc_meth_man_category == "Dairy cows"& annual_milk <= 5000 & de <= 0.62,"Low  productivity systems",
                                      ifelse(ipcc_meth_man_category == "Other cattle" & de <= 0.62,"Low  productivity systems",
                                             ifelse(ipcc_meth_man_category == "Other cattle" & de < 0.72 & de > 0.62,"Low  productivity systems",
                                                    ifelse(ipcc_meth_man_category == "Other cattle" & de >= 0.72 & de < 0.75,"High  productivity systems",
                                                           ifelse(ipcc_meth_man_category == "Other cattle" & de >= 0.75,"High  productivity systems",
                                                                  ifelse(ipcc_meth_man_category == "Sheep"& de < 0.72,"Low  productivity systems",
                                                                         ifelse(ipcc_meth_man_category == "Sheep"& de >= 0.72,"High  productivity systems",
                                                                                ifelse(ipcc_meth_man_category == "Goats"& de < 0.72,"Low  productivity systems",
                                                                                       ifelse(ipcc_meth_man_category == "Goats"& de >= 0.72,"High  productivity systems",
                                                                                              ifelse(ipcc_meth_man_category == "Swine"& de < 0.72,"Low  productivity systems",
                                                                                                     ifelse(ipcc_meth_man_category == "Swine"& de >= 0.72,"High  productivity systems",NA))))))))))))))


  dairy_cattle <- c("Cattle - Cows (local)","Cattle - Cows (improved)","Cattle - Cows (high productive)")
  non_dairy_cattle <- c("Cattle - Adult male","Cattle - Steers/heifers","Cattle - Steers/heifers (improved)","Cattle - Calves","Cattle - Calves (improved)")

  max_meth_bo <- productivity1%>%
    mutate(ipcc_meth_man_t2 = ifelse(livetype_desc %in% dairy_cattle,"Dairy cattle",
                                     ifelse(livetype_desc %in% non_dairy_cattle,"Other cattle",
                                            ifelse(grepl("Buffalo",livetype_desc),"Buffalo",
                                                   ifelse(grepl("Sheep",livetype_desc),"Sheep",
                                                          ifelse(grepl("Goats",livetype_desc),"Goats",
                                                                 ifelse(grepl("Pigs",livetype_desc),"Swine",NA)))))))%>%
    left_join(filter(table_10.16,Region == region),
              by=c("ipcc_meth_man_t2" = "Category_of_animal","productivity"="Productivity_systems"))

  #mcf
  climate_zone <- para[["climate_zone_2"]]

  table_10.17 <- ghg_ipcc_data[["Table 10.17"]][ghg_ipcc_data[["Table 10.17"]]$climate_zone_2==climate_zone,]

  mcf <- max_meth_bo%>%
    mutate(mfc_non_roofed_enclosure = table_10.17[table_10.17$Manure_management_systems == manureman_non_roofed_enclosure,"MCFs"],
           mfc_offfarm_grazing = table_10.17[table_10.17$Manure_management_systems == manureman_offfarm_grazing,"MCFs"],
           mfc_onfarm_grazing = table_10.17[table_10.17$Manure_management_systems == manureman_onfarm_grazing,"MCFs"],
           mfc_stable = table_10.17[table_10.17$Manure_management_systems == manureman_stable,"MCFs"])%>%
    select(-de_intake,-ge_intake,-de,-er_growth)

  #Emission factor for methane from manure management calculation
  eft <- left_join(vs,mcf,by=c("livestock_category_code" = "livetype_code"))%>%
    mutate(emission_factor = (volatile_solid_excretion*no_days)*(Bo*0.67*((time_in_stable*mfc_stable)+(time_in_non_roofed_enclosure*mfc_non_roofed_enclosure)+(time_in_onfarm_grazing*mfc_onfarm_grazing)+(time_in_offfarm_grazing*mfc_offfarm_grazing)))) #equation 10.23

  ################################################################################################################################
  #Annual average nitrogen excretion rates T2
  #Nitrogen intake
  n_intake1 <- left_join(eft,cp1,by="livestock_category_code")%>%
    mutate(n_intake = ifelse(grepl("Cattle",livetype_desc),((ge_intake/no_days)/18.45)*((cp/100)/6.25), #equation 10.32
                             ifelse(grepl("Buffalo",livetype_desc),((ge_intake/no_days)/18.45)*((cp/100)/6.25), #equation 10.32
                                    ifelse(grepl("Sheep",livetype_desc),((ge_intake/no_days)/18.45)*((cp/100)/6.25), #equation 10.32
                                           ifelse(grepl("Goats",livetype_desc),((ge_intake/no_days)/18.45)*((cp/100)/6.25), #equation 10.32
                                                  ifelse(grepl("Pigs",livetype_desc),(dmi_tot/no_days)*((cp/100)/6.25),NA)))))) #equation 10.32A

  #Nitrogen retention
  n_retention1 <- n_intake1%>%
    mutate(wkg = ifelse(ipcc_meth_man_category=="Dairy cows" & productivity == "High  productivity systems",7,
                        ifelse(ipcc_meth_man_category=="Dairy cows" & productivity == "Low  productivity systems",6.5,NA)),
           ckg = ifelse(ipcc_meth_man_category=="Swine" & productivity == "High  productivity systems",1.2,
                        ifelse(ipcc_meth_man_category=="Swine" & productivity == "Low  productivity systems",0.8,NA)),
           n_weaned = ifelse(livetype_desc=="Pigs - lactating/pregnant sows", (0.0025*litter_size*birth_interval*body_weight*((wkg-ckg)/0.98))/lactation_length,0),#equation 10.33B
           n_gain = ifelse(livetype_desc=="Pigs - lactating/pregnant sows",0.025*birth_interval*12.25,
                            ifelse(livetype_desc== "Pigs - growers" & body_weight %gel% c(4,7),0.031,
                                   ifelse(livetype_desc== "Pigs - growers" & body_weight %gel% c(7,20),0.028,
                                          ifelse(livetype_desc== "Pigs - growers" & body_weight %gel% c(20,40),0.025,
                                                 ifelse(livetype_desc== "Pigs - growers" & body_weight %gel% c(40,80),0.024,
                                                        ifelse(livetype_desc== "Pigs - growers" & body_weight %gel% c(80,120),0.021,
                                                               ifelse(livetype_desc== "Pigs - dry sows/boars",0.021,0))))))),
           n_retained = ifelse(!grepl("Pigs",livetype_desc),ifelse(annual_growth == 0,(annual_milk*(protein_milkcontent/100))/6.38/no_days,((annual_milk*(protein_milkcontent/100))/6.38)+(((annual_growth*(268-(7.03*er_growth/annual_growth)))/1000)/6.25))/no_days,#equation 10.33
                               ifelse(livetype_desc=="Pigs - lactating/pregnant sows",n_gain+n_weaned, #equation 10.33A
                                      ifelse(livetype_desc== "Pigs - growers" | livetype_desc== "Pigs - dry sows/boars",(annual_growth*n_gain),0)))) #equation 10.33C


  #Nitrogen excretion rates
  n_excretion <- n_retention1%>%
    mutate(n_excretion_rate = n_intake*(1-n_retained)*no_days) #equation 10.31

  #################################################################################################################################
  #Direct N2O emissions
  table_10.21 <- ghg_ipcc_data[["Table 10.21"]]
  direct_N2O <- n_excretion%>%
    mutate(ef3_stable = ifelse(is.na(table_10.21[table_10.21$system==manureman_stable,3]),0,table_10.21[table_10.21$system==manureman_stable,3]),
           ef3_non_roofed_enclosure = ifelse(is.na(table_10.21[table_10.21$system==manureman_non_roofed_enclosure,3]),0,table_10.21[table_10.21$system==manureman_non_roofed_enclosure,3]),
           ef3_offfarm_grazing = ifelse(is.na(table_10.21[table_10.21$system==manureman_offfarm_grazing,3]),0,table_10.21[table_10.21$system==manureman_offfarm_grazing,3]),
           ef3_onfarm_grazing = ifelse(is.na(table_10.21[table_10.21$system==manureman_onfarm_grazing,3]),0,table_10.21[table_10.21$system==manureman_onfarm_grazing,3]),
           direct_N2O_emission = ((n_excretion_rate*time_in_stable*ef3_stable)+(n_excretion_rate*time_in_non_roofed_enclosure*ef3_non_roofed_enclosure)+(n_excretion_rate*time_in_offfarm_grazing*ef3_offfarm_grazing)+(n_excretion_rate*time_in_onfarm_grazing*ef3_onfarm_grazing))*(44/28))#Equation 10.25

  #################################################################################################################################
  #Indirect N2O emissions
  #Selecting FracGasMS and n2o emissions from managed soils EF4
  table_10.22 <- ghg_ipcc_data[["Table 10.22"]]

  FracGasMS <- direct_N2O%>%
    mutate(indirect_n20_animal = ifelse(livetype_desc %in% dairy_cattle,"Dairy Cow",
                                        ifelse(livetype_desc %in% non_dairy_cattle,"Other Cattle",
                                               ifelse(grepl("Pigs",livetype_desc),"Swine","Other animals"))),
           ef4 = ghg_ipcc_data[["table_11.1_&_table_11.3"]][ghg_ipcc_data[["table_11.1_&_table_11.3"]]$emission_factors=="EF4",4])%>%
    left_join(table_10.22[,1:5],by = c("indirect_n20_animal"="livestock_category","manureman_stable"="system"))%>%
    rename(FracGasMS_stable = FracGas_MS,
           Frac_leach_MS_stable = Frac_leach_MS,
           FracGas_MS_range_stable = FracGas_MS_range)%>%
    left_join(table_10.22[,1:5],by = c("indirect_n20_animal"="livestock_category","manureman_non_roofed_enclosure"="system"))%>%
    rename(FracGasMS_non_roofed_enclosure = FracGas_MS,
           Frac_leach_MS_non_roofed_enclosure = Frac_leach_MS,
           FracGas_MS_range_non_roofed_enclosure = FracGas_MS_range)%>%
    left_join(table_10.22[,1:5],by = c("indirect_n20_animal"="livestock_category","manureman_offfarm_grazing"="system"))%>%
    rename(FracGasMS_offfarm_grazing = FracGas_MS,
           Frac_leach_MS_offfarm_grazing = Frac_leach_MS,
           FracGas_MS_range_offfarm_grazing = FracGas_MS_range)%>%
    left_join(table_10.22[,1:5],by = c("indirect_n20_animal"="livestock_category","manureman_onfarm_grazing"="system"))%>%
    rename(FracGasMS_onfarm_grazing = FracGas_MS,
           Frac_leach_MS_onfarm_grazing = Frac_leach_MS,
           FracGas_MS_range_onfarm_grazing = FracGas_MS_range)%>%
    mutate(FracGasMS_stable = ifelse(is.na(FracGasMS_stable),0,FracGasMS_stable),
           Frac_leach_MS_stable = ifelse(is.na(Frac_leach_MS_stable),0,Frac_leach_MS_stable),
           FracGasMS_non_roofed_enclosure = ifelse(is.na(FracGasMS_non_roofed_enclosure),0,FracGasMS_non_roofed_enclosure),
           Frac_leach_MS_non_roofed_enclosure = ifelse(is.na(Frac_leach_MS_non_roofed_enclosure),0,Frac_leach_MS_non_roofed_enclosure),
           FracGasMS_offfarm_grazing = ifelse(is.na(FracGasMS_offfarm_grazing),0,FracGasMS_offfarm_grazing),
           Frac_leach_MS_offfarm_grazing = ifelse(is.na(Frac_leach_MS_offfarm_grazing),0,Frac_leach_MS_offfarm_grazing),
           FracGasMS_onfarm_grazing = ifelse(is.na(FracGasMS_onfarm_grazing),0,FracGasMS_onfarm_grazing),
           Frac_leach_MS_onfarm_grazing = ifelse(is.na(Frac_leach_MS_onfarm_grazing),0,Frac_leach_MS_onfarm_grazing))

  #computing indirect N2O emissions
  indirect_N2O <- FracGasMS%>%
    mutate(n_volatilisation = ((n_excretion_rate*time_in_stable*FracGasMS_stable)+(n_excretion_rate*time_in_non_roofed_enclosure*FracGasMS_non_roofed_enclosure)+(n_excretion_rate*time_in_offfarm_grazing*FracGasMS_offfarm_grazing)+(n_excretion_rate*time_in_onfarm_grazing*FracGasMS_onfarm_grazing)), #equation 10.26
           indirect_N2O_emission = n_volatilisation*ef4*44/28)#equation 10.28

  #################################################################################################################################
  #N flow calculations of total available N from manure for application to  fields
  #Manure used as fertilizer
  total_n_from_manure_mgmt <- indirect_N2O%>%
    mutate(FracN2MS_stable = 3*ef3_stable, #equation 10.34B
           FracN2MS_non_roofed_enclosure = 3*ef3_non_roofed_enclosure, #equation 10.34B
           FracN2MS_offfarm_grazing = 3*ef3_offfarm_grazing, #equation 10.34B
           FracN2MS_onfarm_grazing = 3*ef3_onfarm_grazing, #equation 10.34B
           Frac_LOSS_MS_stable = FracGasMS_stable+Frac_leach_MS_stable+FracN2MS_stable+ef3_stable,  #equation 10.34A
           Frac_LOSS_MS_non_roofed_enclosure = FracGasMS_non_roofed_enclosure+Frac_leach_MS_non_roofed_enclosure+FracN2MS_non_roofed_enclosure+ef3_non_roofed_enclosure, #equation 10.34A
           Frac_LOSS_MS_offfarm_grazing = FracGasMS_offfarm_grazing+Frac_leach_MS_offfarm_grazing+FracN2MS_offfarm_grazing+ef3_offfarm_grazing, #equation 10.34A
           Frac_LOSS_MS_onfarm_grazing = FracGasMS_onfarm_grazing+Frac_leach_MS_onfarm_grazing+FracN2MS_onfarm_grazing+ef3_onfarm_grazing, #equation 10.34A
           total_n_from_manure_mgmt = ((n_excretion_rate*time_in_stable)*(1-Frac_LOSS_MS_stable))+((n_excretion_rate*time_in_non_roofed_enclosure)*(1-Frac_LOSS_MS_non_roofed_enclosure))+((n_excretion_rate*time_in_offfarm_grazing)*(1-Frac_LOSS_MS_offfarm_grazing))+((n_excretion_rate*time_in_onfarm_grazing)*(1-Frac_LOSS_MS_onfarm_grazing)))  #equation 10.34

  ###############################################################################################################################
  #last calc in manure sheet
  cattle_pig_poultry <- c("Cattle - Cows (local)","Cattle - Cows (improved)","Cattle - Cows (high productive)",
                          "Cattle - Adult male","Cattle - Steers/heifers","Cattle - Steers/heifers (improved)",
                          "Cattle - Calves","Cattle - Calves (improved)", "Buffalo - Cows","Buffalo - Steers/heifers",
                          "Buffalo - Steers/heifers","Buffalo - Calves","Pigs - lactating/pregnant sows",
                          "Pigs - dry sows/boars","Pigs - growers")
  sheep_and_other <- c("Sheep - Ewes","Goats - Does","Sheep - Breeding Rams","Goats - Breeding Bucks","Sheep - Fattening Rams",
                       "Goats - Fattening Bucks","Sheep - Lambs","Goats - Kids")

  cattle_pig_poultry_n_pasture <- dplyr::filter(total_n_from_manure_mgmt,livetype_desc%in%cattle_pig_poultry)%>%
    mutate(onfarm = n_excretion_rate*time_in_onfarm_grazing,
           off_farm = n_excretion_rate*time_in_offfarm_grazing)%>%
    select(livestock_category_code,livetype_desc,onfarm,off_farm)

  sheep_and_other_n_pasture <- dplyr::filter(total_n_from_manure_mgmt,livetype_desc%in%sheep_and_other)%>%
    mutate(onfarm = n_excretion_rate*time_in_onfarm_grazing,
           off_farm = n_excretion_rate*time_in_offfarm_grazing)%>%
    select(livestock_category_code,livetype_desc,onfarm,off_farm)

  #################################################################################################################################
  #Crop parameters
  #feed_items <- para[["feed_items"]]
  feed_items <- land_required[["feed_items_frac"]] %>%
    as.data.frame()

  land_used <- land_required[["land_requirements_all"]]%>%
    group_by(feed)%>%
    summarise(area_total = sum(area_total, na.rm = T))

  crop_parameters <- left_join(feed_items,land_used, by = c("feed_item_name"="feed"))

  #################################################################################################################################
  #GHG Soil
  purchased_manure <- as.numeric(para[["purchased_manure"]])
  purchased_compost <- as.numeric(para[["purchased_compost"]])
  purchased_organic_n <-  as.numeric(para[["purchased_organic_n"]])
  purchased_bedding <-  as.numeric(para[["purchased_bedding"]])
  manure_produced <-  as.numeric(para[["manure_produced"]])

  n_synthetic_fertilizer_managed_soil <- sum(nitrogen_balance$in1,na.rm = TRUE)

  sum_total_n_from_manure_mgmt <- sum(total_n_from_manure_mgmt$total_n_from_manure_mgmt,na.rm = TRUE)
  ###############################################################################################################################
  ## Direct N emission
  ### Anthropogenic N input types to estimate annual direct N2O-N emissions produced from managed soils

  n_organic_manure_managed_soil <- (purchased_manure+purchased_compost+purchased_organic_n+purchased_bedding+sum_total_n_from_manure_mgmt)-manure_produced

  n_from_crop_residues <- crop_parameters%>%
    mutate(fraction_crop_residue = 1-(residue_removal+residue_burnt),
           dm_per_ha = fresh_yield*dm_content,
           crop_residue_n_per_area = dm_per_ha*residue_n*1000,
           n_from_crop_residue = area_total*fraction_crop_residue*crop_residue_n_per_area)

  n_from_crop_residue_managed_soil <- sum(n_from_crop_residues$crop_residue_n_per_area,na.rm = TRUE)

  emission_factor_managed_soil <- "EF1"

  n_managed_soil <- data.frame(rbind(n_synthetic_fertilizer_managed_soil, n_organic_manure_managed_soil, n_from_crop_residue_managed_soil))%>%
    tibble::rownames_to_column()%>%
    mutate(emission_factor_managed_soil)

  names(n_managed_soil) <- c("anthropogenic_N_input","amount_of_N_applied","emission_factors")
  ###############################################################################################################################
  ### Anthropogenic N input types to estimate annual direct N2O-N emissions produced from flooded rice
  #N from rice
  rice <- dplyr::filter(crop_parameters,grepl('Rice', feed_item_name))

  if(nrow(rice)>0){

    if (rice$source_type!="Purchased"){
      rice_2 <- rice%>%
        mutate(n_synthetic_fertilizer = fertilizer_rate*area_total)

      n_synthetic_fertilizer_flooded_rice <- sum(rice_2$n_synthetic_fertilizer,na.rm = TRUE)
    }else{n_synthetic_fertilizer_flooded_rice <- 0}

  }else{n_synthetic_fertilizer_flooded_rice <- 0}

  n_organic_manure_flooded_rice <- NA

  n_from_crop_residue_flooded_rice <- NA

  emission_factor_flooded_rice <- "EF1R"

  n_flooded_rice <- data.frame(rbind(n_synthetic_fertilizer_flooded_rice, n_organic_manure_flooded_rice, n_from_crop_residue_flooded_rice))%>%
    tibble::rownames_to_column()%>%
    mutate(emission_factor_flooded_rice)

  names(n_flooded_rice) <- c("anthropogenic_N_input","amount_of_N_applied","emission_factors")

  ###############################################################################################################################
  ### Anthropogenic N input type from Urine and dung inputs to grazed soils
  cattle_pig_poultry_n_pasture_onfarm <-sum(cattle_pig_poultry_n_pasture$onfarm,na.rm = TRUE)
  sheep_and_other_n_pasture_onfarm <- sum(sheep_and_other_n_pasture$onfarm,na.rm = TRUE)

  emission_factor_grazed_soils <- c("EF3PRP-CPP","EF3PRP-SO")

  n_grazed_soils <- data.frame(rbind(cattle_pig_poultry_n_pasture_onfarm, sheep_and_other_n_pasture_onfarm))%>%
    tibble::rownames_to_column()%>%
    mutate(emission_factor_grazed_soils)

  names(n_grazed_soils) <- c("anthropogenic_N_input","amount_of_N_applied","emission_factors")

  annual_N20N_onfarm_direct_emission <- data.frame(rbind(n_managed_soil,n_flooded_rice,n_grazed_soils))%>%
    left_join(ghg_ipcc_data[["table_11.1_&_table_11.3"]],by="emission_factors")%>%
    mutate(annual_N20N_direct_emission_from_managed_soil = amount_of_N_applied*n2o_emissions_from_managed_soils*(44/28))

  ###############################################################################################################################
  ## Indirect N emission

  organic_n <- sum(n_organic_manure_managed_soil,n_from_crop_residue_managed_soil,n_organic_manure_flooded_rice,n_from_crop_residue_flooded_rice,na.rm = TRUE)

  n_pasture_onfarm <- sum(cattle_pig_poultry_n_pasture_onfarm,sheep_and_other_n_pasture_onfarm,na.rm = TRUE)

  N20_indirect_emission <- as.data.frame(cbind(n_synthetic_fertilizer_managed_soil,
                                               organic_n,
                                               n_pasture_onfarm))

  names(N20_indirect_emission) <- c("n_synthetic_fertilizer_managed_soil","n_organic","n_pasture_onfarm")

  FracGASF <- dplyr::filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "FracGASF")

  FracGASM <- dplyr::filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "FracGASM")

  EF4 <- dplyr::filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "EF4")

  EF3PRP_CPP <- dplyr::filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "EF3PRP-CPP")

  EF3PRP_SO <- dplyr::filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "EF3PRP-SO")

  N20_onfarm_indirect_emission <- N20_indirect_emission%>%
    mutate(FracGASF = FracGASF$n2o_emissions_from_managed_soils,
           FracGASM = FracGASM$n2o_emissions_from_managed_soils,
           EF4 = EF4$n2o_emissions_from_managed_soils,
           annual_N20N_from_atmospheric_deposition = ((n_synthetic_fertilizer_managed_soil*FracGASF)+((n_organic+n_pasture_onfarm)*FracGASM))*EF4*(44/28))
  ###############################################################################################################################
  ## Off-farm soil
  cattle_pig_poultry_n_pasture_off_farm <-sum(cattle_pig_poultry_n_pasture$off_farm,na.rm = TRUE)
  sheep_and_other_n_pasture_off_farm <- sum(sheep_and_other_n_pasture$off_farm,na.rm = TRUE)

  N20N_off_farm <- data.frame(rbind(cattle_pig_poultry_n_pasture_off_farm,sheep_and_other_n_pasture_off_farm))%>%
    tibble::rownames_to_column()%>%
    rename(category = rowname,n_off_farm_pasture = rbind.cattle_pig_poultry_n_pasture_off_farm..sheep_and_other_n_pasture_off_farm.)%>%
    mutate(EF3PRP = ifelse(grepl("cattle",category),EF3PRP_CPP$n2o_emissions_from_managed_soils,EF3PRP_SO$n2o_emissions_from_managed_soils),
           annual_N20N_off_farm_direct_emission =n_off_farm_pasture*EF3PRP,
           FracGASM = FracGASM$n2o_emissions_from_managed_soils,
           EF4 = EF4$n2o_emissions_from_managed_soils,
           annual_N20N_from_atmospheric_deposition =annual_N20N_off_farm_direct_emission*FracGASM*EF4*(44/28))

  ghg_soil <- list(annual_N20N_onfarm_direct_emission = annual_N20N_onfarm_direct_emission,
                   N20_onfarm_indirect_emission = N20_onfarm_indirect_emission,
                   N20N_off_farm = N20N_off_farm)

  ################################################################################################################################################################################################################################
  #GHG Burning

  residue_burn <- crop_parameters%>%
    mutate(amnt_crop_residue_burnt = residue_dry_yield*residue_burnt*area_total)

  mass_residue_burn <- sum(residue_burn$amnt_crop_residue_burnt,na.rm = TRUE)

  combusion_factor <- 0.80

  ghg_burn <- ghg_ipcc_data[["table_2.5"]]%>%
    mutate(mass_residue_burn,
           combusion_factor,
           amount_of_ghg_emission_from_fire = mass_residue_burn*combusion_factor*burnt_emission_factor)#equation 2.27

  ################################################################################################################################################################################################################################
  #GHG off-farm
  fertlizer_parameters <- para[["fertilizer"]]%>%
    left_join(ghg_ipcc_data[["fertilizer_table"]], by=c("fertilizer_desc"="fertilizer_type"))%>%
    mutate(fertlizer_ghg_emissions = quantity*emissions_factor_kg_CO2_eq_per_kg_fertilizer)

  fertlizer_ghg_emissions_per_ha <- sum(fertlizer_parameters$fertlizer_ghg_emissions,na.rm = TRUE)/sum(land_used$area_total,na.rm = TRUE)

  fetilizer_ghg <- list(fertlizer_parameters=fertlizer_parameters,
                        fertlizer_ghg_emissions_per_ha = fertlizer_ghg_emissions_per_ha)

  ################################################################################################################################################################################################################################
  #GHG Rice
  #filter rice feed
  rice <- dplyr::filter(crop_parameters,grepl('Rice', feed_type_name))

  if(nrow(rice)>0) {

    if (rice$source_type!="Purchased") {

      baseline_emission_factor <- ghg_ipcc_data[["table_5.11"]]$baseline_emission_factor
      soil_type_scaling_factor <- ghg_ipcc_data[["table_5.11"]]$soil_type_scaling_factor


      ghg_rice <- left_join(rice,land_used, by=c("feed_type_name"="feed"))%>%
        left_join(ghg_ipcc_data[["table_5.12"]],by="ecosystem_type")%>%
        left_join(ghg_ipcc_data[["table_5.13"]],by="water_regime")%>%
        left_join(ghg_ipcc_data[["table_5.14"]],by="organic_amendment")%>%
        mutate(baseline_emission_factor,
               soil_type_scaling_factor,
               Scaling_factor_for_both_types = (1+(fertilizer_rate*conversion_factor))^0.59,#equation 5.3
               daily_emission = baseline_emission_factor*disaggregated_scaling_factor_w*disaggregated_scaling_factor_p*Scaling_factor_for_both_types*soil_type_scaling_factor,#equation 5.3
               annual_methane_emission = area_total*cultivation_period*daily_emission)%>%
        select(-residue_dry_yield,-residue_burnt)


    }else{ghg_rice <- data.frame(annual_methane_emission = 0,message=c("rice is purchased"))}

  }else{ghg_rice <- data.frame(annual_methane_emission = 0,message=c("no rice in feed basket"))}


  ################################################################################################################################################################################################################################
  #GHG data merge
  Methane	<- 28
  N2O	<- 265


  Enteric_fermentation_Methane <- sum(ef$enteric_methane_emissions,na.rm = T)/sum(land_used$area_total,na.rm = T)
  Manure_Methane <- sum(eft$emission_factor,na.rm = T)/sum(land_used$area_total,na.rm = T)
  Manure_Direct_N2O <- sum(direct_N2O$direct_N2O_emission,na.rm = T)/sum(land_used$area_total,na.rm = T)
  Manure_Indirect_N2O <- sum(indirect_N2O$indirect_N2O_emission,na.rm = T)/sum(land_used$area_total,na.rm = T)
  Soil_Direct_N2O <- sum(annual_N20N_onfarm_direct_emission$annual_N20N_direct_emission_from_managed_soil,na.rm = T)/sum(land_used$area_total,na.rm = T)
  Soil_Indirect_N2O <- sum(N20_onfarm_indirect_emission$annual_N20N_from_atmospheric_deposition,na.rm = T)/sum(land_used$area_total,na.rm = T)
  OFF_Farm_Soil_Direct_N2O <- sum(N20N_off_farm$annual_N20N_off_farm_direct_emission,na.rm = T)/sum(land_used$area_total,na.rm = T)
  OFF_Farm_Soil_Indirect_N2O <- sum(N20N_off_farm$annual_N20N_from_atmospheric_deposition,na.rm = T)/sum(land_used$area_total,na.rm = T)
  Burning <- ((ghg_burn[ghg_burn$ghg_gas=="CO2",5])+(ghg_burn[ghg_burn$ghg_gas=="CH4",5]*Methane)+(ghg_burn[ghg_burn$ghg_gas=="Nox",5]*N2O))/sum(land_used$area_total,na.rm = T)
  Rice_production_Methane <- sum(ghg_rice$annual_methane_emission,na.rm = T)
  Off_farm_emissions <- fertlizer_ghg_emissions_per_ha


  ghg_emissions <- data.frame(rbind(Enteric_fermentation_Methane,
                                    Manure_Methane,
                                    Manure_Direct_N2O,
                                    Manure_Indirect_N2O,
                                    Soil_Direct_N2O,
                                    Soil_Indirect_N2O,
                                    OFF_Farm_Soil_Direct_N2O,
                                    OFF_Farm_Soil_Indirect_N2O,
                                    Burning,
                                    Rice_production_Methane,
                                    Off_farm_emissions))%>%
    tibble::rownames_to_column()%>%
    rename(GHG_balance = rowname,kg_per_ha = rbind.Enteric_fermentation_Methane..Manure_Methane..Manure_Direct_N2O..)

  return(ghg_emissions)

} #end of ghg function
