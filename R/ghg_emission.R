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

  annual_energy <- select(energy_required[["annual_results"]],livestock_category_code,ge_intake,dmi_tot)

  ##########################################################################################################################
  #Computing methane emission from enteric fermentation
  #Computing feed digestibility (DE)
  de <- feed_basket_quality%>%
    gather(feed,value,-season_name,-livestock_category_code,-livestock_category_name,-feed_variables)%>%
    spread(feed_variables,value)%>%
    mutate(de_by_frac_fed = de_fraction*fraction_as_fed,
           cp_by_frac_fed = fraction_as_fed*cp_content_fresh,
           dm_by_frac_fed = fraction_as_fed*dm_content)%>%
    group_by(season_name,livestock_category_code,livestock_category_name)%>%
    summarise(de_by_frac_fed2 = sum(de_by_frac_fed,na.rm = TRUE),
              cp_by_frac_fed2 = sum(cp_by_frac_fed,na.rm = TRUE),
              dm_by_frac_fed2 = sum(dm_by_frac_fed,na.rm = TRUE),
              fraction_as_fed2 = sum(fraction_as_fed,na.rm = TRUE))%>%
    left_join(seasons, by = "season_name")%>%
    mutate(season_de = de_by_frac_fed2*season_length/no_days,
           season_cp = cp_by_frac_fed2*season_length/no_days,
           season_dm = dm_by_frac_fed2*season_length/no_days)%>%
    group_by(livestock_category_code,livestock_category_name)%>%
    summarise(de = sum(season_de,na.rm = TRUE),
              cp = sum(season_cp,na.rm = TRUE),
              dm = sum(season_dm,na.rm = TRUE))

  #Selecting methane conversion factor (Ym)
  table_10.12 <- ghg_ipcc_data[["Table 10.12"]]
  table_10.13 <- ghg_ipcc_data[["Table 10.13"]]

  ym <- left_join(livestock,de, by = c("livetype_code"="livestock_category_code"))%>%
    mutate(ym = ifelse(ipcc_ef_category_t2 == "Dairy cows"& annual_milk > 8500 & de >= 0.7,table_10.12$Ym[1],
                       ifelse(ipcc_ef_category_t2 == "Dairy cows"& annual_milk > 5000 & annual_milk <= 8500 & de >= 0.63 & de < 0.7,table_10.12$Ym[2],
                              ifelse(ipcc_ef_category_t2 == "Dairy cows"& annual_milk <= 5000 & de <= 0.62,table_10.12$Ym[3],
                                     ifelse(ipcc_ef_category_t2 == "Non-dairy" & de <= 0.62,table_10.12$Ym[4],
                                            ifelse(ipcc_ef_category_t2 == "Non-dairy" & de < 0.72 & de > 0.62,table_10.12$Ym[5],
                                                   ifelse(ipcc_ef_category_t2 == "Non-dairy" & de >= 0.72 & de < 0.75,table_10.12$Ym[6],
                                                          ifelse(ipcc_ef_category_t2 == "Non-dairy" & de >= 0.75,table_10.12$Ym[7],
                                                                 ifelse(ipcc_ef_category_t2 == "Sheep",table_10.13$Ym[1],
                                                                        ifelse(ipcc_ef_category_t2 == "Goats",table_10.13$Ym[2],0))))))))))

  #Computing methane enteric emission factor

  ef <- left_join(ym,annual_energy, by = c("livetype_code"="livestock_category_code"))%>%
    mutate(enteric_methane_emissions = (ge_intake/no_days)*(ym/55.65))

  ############################################################################################################################
  #Computing methane emission from manure management T2
  #Computing volatile solid excretion
  table_m <- ghg_ipcc_data[["Table_m"]]

  vs <- left_join(annual_energy,de, by = "livestock_category_code")%>%
    left_join(table_m,by = "livestock_category_name")%>%
    mutate(volatile_solid_excretion = (((ge_intake/no_days)*(1-de))+(Urinary_energy_frac*(ge_intake/no_days)))*((1-ash_content)/18.45))

  #Extracting Bo
  table_10.16 <- ghg_ipcc_data[["Table 10.16"]]
  table_10.16$Bo <- table_10.16[,4]

  region <- para[["region"]]

  productivity <- para[["productivity"]]

  dairy_cattle <- c("Cattle - Cows (local)","Cattle - Cows (improved)","Cattle - Cows (high productive)")
  non_dairy_cattle <- c("Cattle - Adult male","Cattle - Steers/heifers","Cattle - Steers/heifers (improved)","Cattle - Calves","Cattle - Calves (improved)")

  max_meth_bo <- livestock%>%
    mutate(ipcc_meth_man_t2 = ifelse(livetype_desc %in% dairy_cattle,"Dairy cattle",
                                     ifelse(livetype_desc %in% non_dairy_cattle,"Other cattle",
                                            ifelse(grepl("Buffalo",livetype_desc),"Buffalo",
                                                   ifelse(grepl("Sheep",livetype_desc),"Sheep",
                                                          ifelse(grepl("Goats",livetype_desc),"Goats",
                                                                 ifelse(grepl("Pigs",livetype_desc),"Swine",NA)))))))%>%
    left_join(filter(table_10.16,Region == region & Productivity_systems == productivity),
              by=c("ipcc_meth_man_t2" = "Category_of_animal"))

  #mcf
  table_10.17 <- ghg_ipcc_data[["Table 10.17"]]

  manureman_pasture <- para[["manureman_pasture"]]
  manureman_stable <- para[["manureman_stable"]]
  manureman_yard <- para[["manureman_yard"]]

  mcf <- max_meth_bo%>%
    mutate(manure_man_pasture = manureman_pasture,
           mfc_pasture = table_10.17[table_10.17$system==manure_man_pasture,2],
           manure_man_stable = manureman_stable,
           mfc_stable = table_10.17[table_10.17$system==manure_man_stable,2],
           manure_man_yard = manureman_yard,
           mfc_yard = table_10.17[table_10.17$system==manure_man_yard,2])

  #Emission factor for methane from manure management calculation
  eft <- left_join(vs,mcf,by=c("livestock_category_code" = "livetype_code"))%>%
    mutate(emission_factor = (volatile_solid_excretion*no_days)*(Bo*0.67*((time_in_stable*mfc_stable)+(time_in_non_roofed_enclosure*mfc_yard)+(time_in_onfarm_grazing*mfc_pasture)))) #equation 10.23

  ################################################################################################################################
  #Annual average nitrogen excretion rates T2
  #Nitrogen intake
  n_intake <- eft%>%
    mutate(n_intake = ifelse(grepl("Cattle",livetype_desc),((ge_intake/no_days)/18.45)*(cp/6.25), #equation 10.32
                             ifelse(grepl("Buffalo",livetype_desc),((ge_intake/no_days)/18.45)*(cp/6.25), #equation 10.32
                                    ifelse(grepl("Sheep",livetype_desc),((ge_intake/no_days)/18.45)*(cp/6.25), #equation 10.32
                                           ifelse(grepl("Goats",livetype_desc),((ge_intake/no_days)/18.45)*(cp/6.25), #equation 10.32
                                                  ifelse(grepl("Pigs",livetype_desc),(dmi_tot/no_days)*(cp/6.25),NA)))))) #equation 10.32A

  #Nitrogen retention
  n_retention <- n_intake%>%
    mutate(n_retained = (((annual_milk*(protein_milkcontent/100))/6.38)+((annual_growth*(268-((7.0*16))))/6250))*herd_composition/365/n_intake) #equation 10.33

  #Nitrogen excretion rates
  n_excretion <- n_retention%>%
    mutate(n_excretion_rate = n_intake*(1-n_retained)*no_days) #equation 10.31

  #################################################################################################################################
  #Direct N2O emissions
  table_10.21 <- ghg_ipcc_data[["Table 10.21"]]
  direct_N2O <- n_excretion%>%
    mutate(ef3_stable = table_10.21[table_10.21$system==manureman_stable,3],
           ef3_yard = table_10.21[table_10.21$system==manureman_yard,3],
           direct_N2O_emission = ((n_excretion_rate*time_in_stable*ef3_stable)+(n_excretion_rate*time_in_non_roofed_enclosure*ef3_yard))*(44/28))#Equation 10.25

  #################################################################################################################################
  #Indirect N2O emissions




  ###########################################################################################################
  #Preparation of ghg parameters
  #
  #
  ghg_parameters <- function(para,seasons,annual_energy,ghg_ipcc_data){
    #Computation of ghg parameters from the feedbasket
    for (i in 1:nrow(seasons)) {
      sl <- seasons$season_length[i]

      #Preparing the seasonal feed quality
      seasonal_feed_parameters <- dplyr::filter(feed_basket_quality, season_name == seasons$season_name[i])%>%
        gather(feed,value,-season_name,-livestock_category_code,-livestock_category_name,-feed_variables)%>%
        spread(feed_variables,value)%>%
        mutate(prod_me = fraction_as_fed*me_content_fresh,
               prod_dm = fraction_as_fed*dm_content,
               prod_cp = fraction_as_fed*cp_content_fresh,
               prod_de = fraction_as_fed*de_fraction)%>%
        select(livestock_category_code,prod_me,prod_dm,prod_cp,prod_de)%>%
        group_by(livestock_category_code)%>%
        summarise(average_me = sum(prod_me),
                  average_dm = sum(prod_dm),
                  average_cp = sum(prod_cp),
                  average_de = sum(prod_de))%>%
        mutate(season_de = average_de*sl/no_days,
               season_cp = (average_cp*sl/no_days)/(average_dm*0.01),
               season_name = seasons$season_name[i])
      #Binding seasonal results
      if (i==1) {feed_parameters <- seasonal_feed_parameters}
      else{feed_parameters <- rbind(feed_parameters,seasonal_feed_parameters)}
    }

    ghg_feed_parameters <- feed_parameters%>%
      group_by(livestock_category_code)%>%
      summarise(de = sum(season_de), # digestible energy
                cp = sum(season_cp)) # crude protein

    #Computation of ghg parameters from energy requirement
    ghg_energy_parameters <- annual_energy%>%
      select(livestock_category_code,livestock_category_name,ge_intake)%>%
      mutate(daily_ge_intake = ge_intake/no_days)

    #Computation of ghg parameters from ipcc data
    #manure conversion factor
    manureman_stable <- para[["manureman_stable"]]
    mfc_stable = dplyr::filter(ghg_ipcc_data[["table_10.17"]],system == manureman_stable);names(mfc_stable) <- c("stable_mgmt_syst","mfc_stable")
    manureman_yard <- para[["manureman_yard"]]
    mfc_yard = dplyr::filter(ghg_ipcc_data[["table_10.17"]],system == manureman_yard);names(mfc_yard) <- c("yard_mgmt_syst","mfc_yard")
    manureman_pasture <- para[["manureman_pasture"]]
    mfc_pasture = dplyr::filter(ghg_ipcc_data[["table_10.17"]],system == manureman_pasture);names(mfc_pasture) <- c("pasture_mgmt_syst","mfc_pasture")

    #direct nitrous oxide factor
    direct_n2o_stable <- dplyr::filter(ghg_ipcc_data[["table_10.21"]],system == manureman_stable)%>%
      select(direct_nitrous_oxide_factor)
    names(direct_n2o_stable) <- "direct_n2o_stable"
    direct_n2o_yard <- dplyr::filter(ghg_ipcc_data[["table_10.21"]],system == manureman_yard)%>%
      select(direct_nitrous_oxide_factor)
    names(direct_n2o_yard) <- "direct_n2o_yard"

    #fraction of N loss due to manure management system
    fraction_n_loss_mms_stable <- dplyr::filter(ghg_ipcc_data[["table_10.22"]],system == manureman_stable)%>%
      select(anaimal_category,fraction_n_loss_mms)
    names(fraction_n_loss_mms_stable) <- c("anaimal_category","fraction_n_loss_mms_stable")
    fraction_n_loss_mms_yard <- dplyr::filter(ghg_ipcc_data[["table_10.22"]],system == manureman_yard)%>%
      select(anaimal_category,fraction_n_loss_mms)
    names(fraction_n_loss_mms_yard) <- c("anaimal_category","fraction_n_loss_mms_yard")

    #n2o emissions from managed soils
    n2o_emissions_from_managed_soils <- dplyr::filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "EF4")%>%
      select(n2o_emissions_from_managed_soils)

    #Cnotinent
    region <- para[["region"]]

    ghg_ipcc_parameters <- ghg_ipcc_data[["livestock_parameters"]]%>%
      dplyr::filter(livestock_category_name %in% ghg_energy_parameters$livestock_category_name)%>%
      mutate(stable_mgmt_syst = mfc_stable$stable_mgmt_syst,
             mfc_stable = mfc_stable$mfc_stable,
             yard_mgmt_syst = mfc_yard$yard_mgmt_syst,
             mfc_yard = mfc_yard$mfc_yard,
             pasture_mgmt_syst = mfc_pasture$pasture_mgmt_syst,
             mfc_pasture = mfc_pasture$mfc_pasture,
             direct_n2o_stable = direct_n2o_stable$direct_n2o_stable,
             direct_n2o_yard = direct_n2o_yard$direct_n2o_yard,
             n2o_emissions_from_managed_soils = n2o_emissions_from_managed_soils$n2o_emissions_from_managed_soils)%>%
      left_join(dplyr::filter(ghg_ipcc_data[["table_10A_9"]],Continent == region), by = c("IPCC Category - methane emissions manure - Tier 1" = "anaimal_category"))%>%
      left_join(dplyr::filter(ghg_ipcc_data[["table_10.19"]],Continent  == region), by = c("IPCC-Category - Default N-excretion rates Tier 1" = "anaimal_category"))%>%
      left_join(fraction_n_loss_mms_stable,by = c("IPCC Category - methane emissions manure - Tier 1" = "anaimal_category"))%>%
      left_join(fraction_n_loss_mms_yard,by = c("IPCC Category - methane emissions manure - Tier 1" = "anaimal_category"))

    ghg_parameters <- ghg_energy_parameters%>%
      left_join(ghg_feed_parameters,by = "livestock_category_code")%>%
      left_join(ghg_ipcc_parameters,by = "livestock_category_name")

    return(ghg_parameters)

  }
  ghg_parameters <- ghg_parameters(para,seasons,annual_energy,ghg_ipcc_data)


  ################################################################################################################################################################################################################################
  #Computation of methane emissions from enteric fermentation
  ghg_enteric_manure <- livestock%>%
    left_join(select(ghg_parameters,-livestock_category_name),by = "livestock_category_code")%>%
    mutate(tier_1_enteric_methane_emissions = methferm_tier1*herd_composition, #equation 10.19 and 10.20
           tier_2_enteric_methane_emissions = ((daily_ge_intake)*(methferm_tier2/100)*(no_days))/55.65*(herd_composition), #equation 10.21
           tier_1_manure_mgmt_methane_emissions = herd_composition*methmanure, #equation 10.22
           tier_2_volatile_solid_excretion = (daily_ge_intake*(1-de)+(Urinary_energy_frac*daily_ge_intake))*(1-ash_content)/18.45, #equation 10.24
           tier_2_manure_mgmt_methane_emissions = (tier_2_volatile_solid_excretion*no_days)*(Bo*0.67*((time_in_stable*mfc_stable)+(time_in_non_roofed_enclosure*mfc_yard)+(time_in_onfarm_grazing*mfc_pasture))), #equation 10.23
           tier_1_annual_N_excretion = n_rate*(body_weight/1000)*no_days, #equation 10.30
           n_intake = (daily_ge_intake/18.45)*(cp/100/6.25), #equation 10.32
           n_retention = ((annual_milk*0.034/6.38)+(annual_growth*(268-(7*16))/6250))*herd_composition/no_days/n_intake, #equation 10.33
           tier_2_annual_N_excretion = n_intake*(1-n_retention)*no_days,#equation 10.31
           tier_1_direct_n2o_emission = ((tier_1_annual_N_excretion*time_in_stable*direct_n2o_stable)+(tier_1_annual_N_excretion*time_in_non_roofed_enclosure*direct_n2o_yard))*(44/28)*herd_composition, #Equation 10.25- T1
           tier_2_direct_n2o_emission = ((tier_2_annual_N_excretion*time_in_stable*direct_n2o_stable)+(tier_2_annual_N_excretion*time_in_non_roofed_enclosure*direct_n2o_yard))*(44/28)*herd_composition,#Equation 10.25
           tier_1_n_volatization = ((tier_1_annual_N_excretion*time_in_stable*fraction_n_loss_mms_stable)+(tier_1_annual_N_excretion*time_in_non_roofed_enclosure*fraction_n_loss_mms_yard))*herd_composition, #equation 10.26
           tier_2_n_volatization = ((tier_2_annual_N_excretion*time_in_stable*fraction_n_loss_mms_stable)+(tier_2_annual_N_excretion*time_in_non_roofed_enclosure*fraction_n_loss_mms_yard))*herd_composition, #equation 10.26
           tier_1_indirect_n2o_emission = tier_1_n_volatization*n2o_emissions_from_managed_soils*44/28,#equation 10.27
           tier_2_indirect_n2o_emission = tier_2_n_volatization*n2o_emissions_from_managed_soils*44/28,#equation 10.27
           tier_1_total_n_from_manure_mgmt = (((tier_1_annual_N_excretion*herd_composition*time_in_stable)*(1-fraction_n_loss_mms_stable)*manure_in_stable*manure_as_fertilizer)+
                                                ((tier_1_annual_N_excretion*herd_composition*time_in_non_roofed_enclosure)*(1-fraction_n_loss_mms_yard)*manure_in_field*manure_as_fertilizer))-(tier_1_direct_n2o_emission*28/44),
           tier_2_total_n_from_manure_mgmt = (((tier_2_annual_N_excretion*herd_composition*time_in_stable)*(1-fraction_n_loss_mms_stable)*manure_in_stable*manure_as_fertilizer)+
                                                ((tier_2_annual_N_excretion*herd_composition*time_in_non_roofed_enclosure)*(1-fraction_n_loss_mms_yard)*manure_in_field*manure_as_fertilizer))-(tier_2_direct_n2o_emission*28/44))%>%
    select("livestock_category_code","livestock_category_name","herd_composition","time_in_stable","time_in_non_roofed_enclosure",
           "time_in_onfarm_grazing","time_in_offfarm_grazing","manure_in_stable","manure_in_non_roofed_enclosure","manure_in_field",
           "manure_as_fertilizer","n_content","methferm_tier1","methferm_tier2","methmanure","nexcretion","ge_intake","daily_ge_intake",
           "de","cp","Urinary_energy_frac","ash_content","IPCC Category - methane emissions enteric fermentation - Tier 1",
           "IPCC Category - methane emissions enteric fermentation - Tier 2","IPCC Category - methane emissions manure - Tier 1",
           "IPCC-Category - Default N-excretion rates Tier 1","stable_mgmt_syst","mfc_stable","yard_mgmt_syst","mfc_yard",
           "pasture_mgmt_syst","mfc_pasture","direct_n2o_stable","direct_n2o_yard","n2o_emissions_from_managed_soils",
           "Bo","n_rate","fraction_n_loss_mms_stable","fraction_n_loss_mms_yard","tier_1_enteric_methane_emissions",
           "tier_2_enteric_methane_emissions","tier_1_manure_mgmt_methane_emissions","tier_2_volatile_solid_excretion","tier_2_manure_mgmt_methane_emissions",
           "tier_1_annual_N_excretion","n_intake","n_retention","tier_2_annual_N_excretion","tier_1_direct_n2o_emission","tier_2_direct_n2o_emission",
           "tier_1_n_volatization","tier_2_n_volatization","tier_1_indirect_n2o_emission","tier_2_indirect_n2o_emission","tier_1_total_n_from_manure_mgmt",
           "tier_2_total_n_from_manure_mgmt")

  #N on the pasture
  cattle_pig_poultry_n_pasture <- dplyr::filter(ghg_enteric_manure,livestock_category_name%in%c("Cows (local)","Cows (improved)","Cows (high productive)",
                                                                                        "Adult cattle - male","Steers/heifers","Steers/heifers (improved)",
                                                                                        "Calves","Calves (improved)","Buffalo (dairy)","Buffalo steers/heifers",
                                                                                        "Buffalo calves",  "Pigs - lactating/pregnant sows","Pigs - dry sows/boars","Pigs - growers"))%>%
    mutate(tier_1_onfarm = tier_1_annual_N_excretion*time_in_onfarm_grazing,
           tier_1_offfarm = tier_1_annual_N_excretion*time_in_offfarm_grazing,
           tier_2_onfarm = tier_2_annual_N_excretion*time_in_onfarm_grazing,
           tier_2_offfarm = tier_2_annual_N_excretion*time_in_offfarm_grazing)%>%
    select(livestock_category_code,livestock_category_name,tier_1_onfarm,tier_1_offfarm,tier_2_onfarm,tier_2_offfarm)

  sheep_and_other_n_pasture <- dplyr::filter(ghg_enteric_manure,livestock_category_name%in%c("Sheep/Goats - Ewes/Does","Sheep/Goats - Breeding Rams/Bucks","Sheep/Goats - Fattening Rams/Bucks","Sheep/Goats - Lambs/Kids"))%>%
    mutate(tier_1_onfarm = tier_1_annual_N_excretion*time_in_onfarm_grazing,
           tier_1_offfarm = tier_1_annual_N_excretion*time_in_offfarm_grazing,
           tier_2_onfarm = tier_2_annual_N_excretion*time_in_onfarm_grazing,
           tier_2_offfarm = tier_2_annual_N_excretion*time_in_offfarm_grazing)%>%
    select(livestock_category_code,livestock_category_name,tier_1_onfarm,tier_1_offfarm,tier_2_onfarm,tier_2_offfarm)

  ################################################################################################################################################################################################################################
  #GHG Burning
  crop <- unnest(para[["feed_production"]], cols = c(feed_type_name))


  for (i in 1:length(crop$feed_type_name)) {

    feed_selected <- crop %>% dplyr::filter(feed_type_name %in% crop$feed_type_name[i])

    feed_item <- as.data.frame(feed_selected[["feed_items"]])%>%
      select(feed_item_name,source_type,residue_removal,residue_burnt,fertilizer_rate,ecosystem_type,cultivation_period,water_regime,organic_amendment)

    temp <- feed_selected%>%
      select(feed_type_code,feed_type_name,fresh_yield,dm_content,residue_dry_yield,residue_n)%>%
      left_join(feed_item, by = c("feed_type_name"="feed_item_name"))
    if (i==1) {crop_ghg_parameters = temp}
    else{crop_ghg_parameters = rbind(crop_ghg_parameters,temp)}
  }

  crop_ghg_parameters[c("fresh_yield","dm_content","residue_dry_yield","residue_n","residue_removal","residue_burnt","cultivation_period","fertilizer_rate")] <- sapply(
    crop_ghg_parameters[c("fresh_yield","dm_content","residue_dry_yield","residue_n","residue_removal","residue_burnt","cultivation_period","fertilizer_rate")],as.numeric)#convert columns to numeric

  land_used <- land_required%>%
    group_by(feed)%>%
    summarise(area_total = sum(area_total, na.rm = T))

  residue_burn <- crop_ghg_parameters%>%
    left_join(land_used, by=c("feed_type_name"="feed"))%>%
    mutate(amnt_crop_residue_burnt = residue_dry_yield*residue_burnt*area_total)

  mass_residue_burn <- sum(residue_burn$amnt_crop_residue_burnt,na.rm = TRUE)

  combusion_factor <- 0.80

  ghg_burn <- ghg_ipcc_data[["table_2.5"]]%>%
    mutate(mass_residue_burn,
           combusion_factor,
           amount_of_ghg_emission_from_fire = mass_residue_burn*combusion_factor*burnt_emission_factor)#equation 2.27

  ################################################################################################################################################################################################################################
  #GHG off-farm
  fertlizer_parameters <- para[["purchased_inorganic_fertilizers"]]%>%
    left_join(ghg_ipcc_data[["fertilizer_table"]], by=c("fertilizer_mame"="fertilizer_type"))%>%
    mutate(fertlizer_ghg_emissions = quantity*emissions_factor_kg_CO2_eq_per_kg_fertilizer)

  fertlizer_ghg_emissions_per_ha <- sum(fertlizer_parameters$fertlizer_ghg_emissions,na.rm = TRUE)/sum(land_used$area_total,na.rm = TRUE)

  fetilizer_ghg <- list(fertlizer_parameters=fertlizer_parameters,
                        fertlizer_ghg_emissions_per_ha = fertlizer_ghg_emissions_per_ha)

  ################################################################################################################################################################################################################################
  #GHG Rice
  #filter rice feed
  rice <- dplyr::filter(crop_ghg_parameters,grepl('Rice', feed_type_name))

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


    }else{ghg_rice <- "rice is purchased"}

  }else{ghg_rice <- "no rice in feed basket"}


  ################################################################################################################################################################################################################################
  #GHG Soil

  # T1 N excretion
  ## Direct N emission
  ### Anthropogenic N input types to estimate annual direct N2O-N emissions produced from managed soils
  tier_1_n_synthetic_fertilizer_managed_soil <- sum(nitrogen_balance$in1,na.rm = TRUE)

  sum_tier_1_total_n_from_manure_mgmt <- sum(ghg_enteric_manure$tier_1_total_n_from_manure_mgmt,na.rm = TRUE)

  purchased_manure <- as.numeric(para[["purchased_manure"]])
  purchased_compost <- as.numeric(para[["purchased_compost"]])
  purchased_organic_n <-  as.numeric(para[["purchased_organic_n"]])
  purchased_bedding <-  as.numeric(para[["purchased_bedding"]])
  manure_produced <-  as.numeric(para[["manure_produced"]])
  tier_1_n_organic_manure_managed_soil <- (purchased_manure+purchased_compost+purchased_organic_n+purchased_bedding+sum_tier_1_total_n_from_manure_mgmt)-manure_produced

  n_from_crop_residues <- residue_burn%>% #Nitrogen input from crop residue
    mutate(fraction_crop_residue = 1-(residue_removal+residue_burnt),
           dm_per_ha = fresh_yield*dm_content,
           crop_residue_n_per_area = dm_per_ha*residue_n*1000,
           n_from_crop_residue = area_total*fraction_crop_residue*crop_residue_n_per_area)
  tier_1_n_from_crop_residue_managed_soil <- sum(n_from_crop_residues$crop_residue_n_per_area,na.rm = TRUE)

  emission_factor_managed_soil <- "EF1"

  tier_1_n_managed_soil <- data.frame(rbind(tier_1_n_synthetic_fertilizer_managed_soil, tier_1_n_organic_manure_managed_soil, tier_1_n_from_crop_residue_managed_soil))%>%
    tibble::rownames_to_column()%>%
    mutate(emission_factor_managed_soil)

  names(tier_1_n_managed_soil) <- c("anthropogenic_N_input","amount_of_N_applied","emission_factors")

  ### Anthropogenic N input types to estimate annual direct N2O-N emissions produced from flooded rice
  if(nrow(rice)>0){

    if (rice$source_type!="Purchased"){
      rice_2 <- left_join(rice,land_used,by=c("feed_type_name"="feed"))%>%
        mutate(n_synthetic_fertilizer = fertilizer_rate*area_total)

      tier_1_n_synthetic_fertilizer_flooded_rice <- sum(rice_2$n_synthetic_fertilizer,na.rm = TRUE)
    }else{tier_1_n_synthetic_fertilizer_flooded_rice <- 0}

  }else{tier_1_n_synthetic_fertilizer_flooded_rice <- 0}

  tier_1_n_organic_manure_flooded_rice <- NA

  tier_1_n_from_crop_residue_flooded_rice <- NA

  emission_factor_flooded_rice <- "EF1R"

  tier_1_n_flooded_rice <- data.frame(rbind(tier_1_n_synthetic_fertilizer_flooded_rice, tier_1_n_organic_manure_flooded_rice, tier_1_n_from_crop_residue_flooded_rice))%>%
    tibble::rownames_to_column()%>%
    mutate(emission_factor_flooded_rice)

  names(tier_1_n_flooded_rice) <- c("anthropogenic_N_input","amount_of_N_applied","emission_factors")

  ### Anthropogenic N input type from Urine and dung inputs to grazed soils
  tier_1_cattle_pig_poultry_n_pasture_onfarm <-sum(cattle_pig_poultry_n_pasture$tier_1_onfarm,na.rm = TRUE)
  tier_1_sheep_and_other_n_pasture_onfarm <- sum(sheep_and_other_n_pasture$tier_1_onfarm,na.rm = TRUE)

  emission_factor_grazed_soils <- c("EF3PRP-CPP","EF3PRP-SO")

  tier_1_n_grazed_soils <- data.frame(rbind(tier_1_cattle_pig_poultry_n_pasture_onfarm, tier_1_sheep_and_other_n_pasture_onfarm))%>%
    tibble::rownames_to_column()%>%
    mutate(emission_factor_grazed_soils)

  names(tier_1_n_grazed_soils) <- c("anthropogenic_N_input","amount_of_N_applied","emission_factors")

  tier_1_N20N_direct_emission <- data.frame(rbind(tier_1_n_managed_soil,tier_1_n_flooded_rice,tier_1_n_grazed_soils))%>%
    left_join(ghg_ipcc_data[["table_11.1_&_table_11.3"]],by="emission_factors")%>%
    mutate(annual_N20N_direct_emission_from_managed_soil = amount_of_N_applied*n2o_emissions_from_managed_soils*(44/28))



  # T2 N excretion
  ## Direct N emission
  ### Anthropogenic N input types to estimate annual direct N2O-N emissions produced from managed soils

  tier_2_n_synthetic_fertilizer_managed_soil <- tier_1_n_synthetic_fertilizer_managed_soil

  sum_tier_2_total_n_from_manure_mgmt <- sum(ghg_enteric_manure$tier_2_total_n_from_manure_mgmt,na.rm = TRUE)
  tier_2_n_organic_manure_managed_soil <- (purchased_manure+purchased_compost+purchased_organic_n+purchased_bedding+sum_tier_2_total_n_from_manure_mgmt)-manure_produced

  tier_2_n_from_crop_residue_managed_soil <- tier_1_n_from_crop_residue_managed_soil

  tier_2_n_managed_soil <- data.frame(rbind(tier_2_n_synthetic_fertilizer_managed_soil, tier_2_n_organic_manure_managed_soil, tier_2_n_from_crop_residue_managed_soil))%>%
    tibble::rownames_to_column()%>%
    mutate(emission_factor_managed_soil)

  names(tier_2_n_managed_soil) <- c("anthropogenic_N_input","amount_of_N_applied","emission_factors")

  ### Anthropogenic N input types to estimate annual direct N2O-N emissions produced from flooded rice

  tier_2_n_synthetic_fertilizer_flooded_rice <- tier_1_n_synthetic_fertilizer_flooded_rice

  tier_2_n_organic_manure_flooded_rice <- tier_1_n_organic_manure_flooded_rice

  tier_2_n_from_crop_residue_flooded_rice <- tier_1_n_from_crop_residue_flooded_rice

  tier_2_n_flooded_rice <- data.frame(rbind(tier_2_n_synthetic_fertilizer_flooded_rice, tier_2_n_organic_manure_flooded_rice, tier_2_n_from_crop_residue_flooded_rice))%>%
    tibble::rownames_to_column()%>%
    mutate(emission_factor_flooded_rice)

  names(tier_2_n_flooded_rice) <- c("anthropogenic_N_input","amount_of_N_applied","emission_factors")

  ### Anthropogenic N input type from Urine and dung inputs to grazed soils
  tier_2_cattle_pig_poultry_n_pasture_onfarm <-sum(cattle_pig_poultry_n_pasture$tier_2_onfarm,na.rm = TRUE)
  tier_2_sheep_and_other_n_pasture_onfarm <- sum(sheep_and_other_n_pasture$tier_2_onfarm,na.rm = TRUE)

  tier_2_n_grazed_soils <- data.frame(rbind(tier_2_cattle_pig_poultry_n_pasture_onfarm, tier_2_sheep_and_other_n_pasture_onfarm))%>%
    tibble::rownames_to_column()%>%
    mutate(emission_factor_grazed_soils)

  names(tier_2_n_grazed_soils) <- c("anthropogenic_N_input","amount_of_N_applied","emission_factors")

  tier_2_N20N_direct_emission <- data.frame(rbind(tier_2_n_managed_soil,tier_2_n_flooded_rice,tier_2_n_grazed_soils))%>%
    left_join(ghg_ipcc_data[["table_11.1_&_table_11.3"]],by="emission_factors")%>%
    mutate(annual_N20N_direct_emission_from_managed_soil = amount_of_N_applied*n2o_emissions_from_managed_soils*(44/28))


  annual_N20N_onfarm_direct_emission <- rbind.data.frame(tier_1_N20N_direct_emission,tier_2_N20N_direct_emission)


  ## Indirect N emission
  ###tier 1
  tier_1 <- 1

  tier_1_organic_n <- sum(tier_1_n_organic_manure_managed_soil,tier_1_n_from_crop_residue_managed_soil,tier_1_n_organic_manure_flooded_rice,tier_1_n_from_crop_residue_flooded_rice,na.rm = TRUE)

  tier_1_n_pasture_onfarm <- sum(tier_1_cattle_pig_poultry_n_pasture_onfarm,tier_1_sheep_and_other_n_pasture_onfarm,na.rm = TRUE)

  tier_1_N20_indirect_emission <- as.data.frame(cbind(tier_1,
                                                      tier_1_n_synthetic_fertilizer_managed_soil,
                                                      tier_1_organic_n,
                                                      tier_1_n_pasture_onfarm))

  names(tier_1_N20_indirect_emission) <- c("tier","n_synthetic_fertilizer_managed_soil","n_organic","n_pasture_onfarm")

  ###tier 2
  tier_2 <- 2

  tier_2_organic_n <- sum(tier_2_n_organic_manure_managed_soil,tier_2_n_from_crop_residue_managed_soil,tier_2_n_organic_manure_flooded_rice,tier_2_n_from_crop_residue_flooded_rice,na.rm = TRUE)

  tier_2_n_pasture_onfarm <- sum(tier_2_cattle_pig_poultry_n_pasture_onfarm,tier_2_sheep_and_other_n_pasture_onfarm,na.rm = TRUE)

  tier_2_N20_indirect_emission <- as.data.frame(cbind(tier_2,
                                                      tier_2_n_synthetic_fertilizer_managed_soil,
                                                      tier_2_organic_n,
                                                      tier_2_n_pasture_onfarm))

  names(tier_2_N20_indirect_emission) <- c("tier","n_synthetic_fertilizer_managed_soil","n_organic","n_pasture_onfarm")

  FracGASF <- dplyr::filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "FracGASF")

  FracGASM <- dplyr::filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "FracGASM")

  EF4 <- dplyr::filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "EF4")

  EF3PRP_CPP <- dplyr::filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "EF3PRP-CPP")

  EF3PRP_SO <- dplyr::filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "EF3PRP-SO")

  N20_onfarm_indirect_emission <- rbind.data.frame(tier_1_N20_indirect_emission,tier_2_N20_indirect_emission)%>%
    mutate(FracGASF = FracGASF$n2o_emissions_from_managed_soils,
           FracGASM = FracGASM$n2o_emissions_from_managed_soils,
           EF4 = EF4$n2o_emissions_from_managed_soils,
           annual_N20N_from_atmospheric_deposition = ((n_synthetic_fertilizer_managed_soil*FracGASF)+((n_organic+n_pasture_onfarm)*FracGASM))*EF4*(44/28))

  ## Off-farm

  tier_1_cattle_pig_poultry_n_pasture_offfarm <-sum(cattle_pig_poultry_n_pasture$tier_1_offfarm,na.rm = TRUE)
  tier_1_sheep_and_other_n_pasture_offfarm <- sum(sheep_and_other_n_pasture$tier_1_offfarm,na.rm = TRUE)

  tier_2_cattle_pig_poultry_n_pasture_offfarm <-sum(cattle_pig_poultry_n_pasture$tier_2_offfarm,na.rm = TRUE)
  tier_2_sheep_and_other_n_pasture_offfarm <- sum(sheep_and_other_n_pasture$tier_2_offfarm,na.rm = TRUE)

  N20N_offfarm <- data.frame(rbind(tier_1_cattle_pig_poultry_n_pasture_offfarm,tier_1_sheep_and_other_n_pasture_offfarm,tier_2_cattle_pig_poultry_n_pasture_offfarm,tier_2_sheep_and_other_n_pasture_offfarm))%>%
    tibble::rownames_to_column()%>%
    rename(category = rowname,n_offfarm_pasture = rbind.tier_1_cattle_pig_poultry_n_pasture_offfarm..tier_1_sheep_and_other_n_pasture_offfarm..)%>%
    mutate(EF3PRP = ifelse(grepl("cattle",category),EF3PRP_CPP$n2o_emissions_from_managed_soils,EF3PRP_SO$n2o_emissions_from_managed_soils),
           annual_N20N_offfarm_direct_emission =n_offfarm_pasture*EF3PRP,
           FracGASM = FracGASM$n2o_emissions_from_managed_soils,
           EF4 = EF4$n2o_emissions_from_managed_soils,
           annual_N20N_from_atmospheric_deposition =annual_N20N_offfarm_direct_emission*FracGASM*EF4*(44/28))

  ghg_soil <- list(annual_N20N_onfarm_direct_emission = annual_N20N_onfarm_direct_emission,
                   N20_onfarm_indirect_emission = N20_onfarm_indirect_emission,
                   N20N_offfarm = N20N_offfarm)

  ################################################################################################################################################################################################################################
  #GHG data merge
  ghg_emissions <- list(ghg_enteric_manure = ghg_enteric_manure,
                        cattle_pig_poultry_n_pasture = cattle_pig_poultry_n_pasture,
                        sheep_and_other_n_pasture = sheep_and_other_n_pasture,
                        ghg_off_farm = fetilizer_ghg,
                        ghg_burning = ghg_burn,
                        ghg_soil = ghg_soil,
                        ghg_rice = ghg_rice)

  return(ghg_emissions)

} #end of ghg function
