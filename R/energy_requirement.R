#' @title Energy requirement
#'
#' @description It calculates energy requirement. Compute this after
#' the `feed_quality` function
#'
#' @param para A JSON file containing user inputs
#'
#' @param feed_basket_quality A dataframe computed using the `feed_quality` function
#'
#' @param energy_parameters A JSON file containing energy coefficients
#'
#' @importFrom dplyr summarise mutate filter left_join %>%
#'
#' @importFrom tidyr gather spread
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' feed_basket_quality <- feed_quality(mufindi)
#' energy_requirement(mufindi,feed_basket_quality,energy_parameters)
#' }
#'
#' @export

energy_requirement <- function(para, feed_basket_quality,energy_parameters){
  #get the seasons
  seasons <- para[["seasons"]]
  livestock <- para[["livestock"]]%>%
    rename(livestock_category_code=livetype_code,
                  livestock_category_name=livetype_desc)
  no_days <- 365


  #Compute annual energy and protein required

  #Maintenance energy
  table_10.4 <- energy_parameters[["Table 10.4"]]

  Cattle_n_Buffalo <- c("Cattle - Steers/heifers",
                      "Cattle - Steers/heifers (improved)",
                      "Cattle - Calves",
                      "Cattle - Calves (improved)",
                      "Buffalo - Steers/heifers",
                      "Buffalo - Calves")

  Cattle_n_Buffalo_lactating_cows <- c("Cattle - Cows (local)",
                                     "Cattle - Cows (improved)",
                                     "Cattle - Cows (high productive)",
                                     "Buffalo - Cows")

  Cattle_n_Buffalo_bulls <- c("Cattle - Adult male",
                            "Buffalo - Adult male")

  Sheep_lamb_to_one_year <- c("Sheep - Lambs")

  Sheep_older_than_one_year <- c("Sheep - Ewes",
                               "Sheep - Breeding Rams",
                               "Sheep - Fattening Rams")
  Goats <- c("Goats - Does",
             "Goats - Breeding Bucks",
             "Goats - Fattening Bucks",
             "Goats - Kids")

  Pigs <- c("Pigs - lactating/pregnant sows",
            "Pigs - dry sows/boars",
            "Pigs - growers")

  maintenance_er <- livestock%>%
    mutate(maintenance_cat = ifelse(livestock_category_name%in%Cattle_n_Buffalo,"Cattle_Buffalo",
                                    ifelse(livestock_category_name%in%Cattle_n_Buffalo_lactating_cows,"Cattle_Buffalo_lactating_cows",
                                           ifelse(livestock_category_name%in%Cattle_n_Buffalo_bulls,"Cattle_Buffalo_bulls",
                                                  ifelse(livestock_category_name%in%Sheep_lamb_to_one_year,"Sheep_lamb_to_1_year",
                                                         ifelse(livestock_category_name%in%Sheep_older_than_one_year,"Sheep_older_than_1_year",
                                                                ifelse(livestock_category_name%in%Goats,"Goats",
                                                                       ifelse(livestock_category_name%in%Pigs,"Pigs",NA))))))))%>%
    left_join(table_10.4,by = c("maintenance_cat" = "animal_category"))%>%
    mutate(er_maintenance = maintenance_cfi*(body_weight**0.75)) #equation 10.3

  #Activity energy
  activity_er <- maintenance_er%>%
    mutate(er_activity = ifelse(livestock_category_name%in%c(Cattle_n_Buffalo,Cattle_n_Buffalo_lactating_cows,Cattle_n_Buffalo_bulls),er_maintenance*((0*time_in_stable)+(0.17*time_in_onfarm_grazing)+(0.17*time_in_non_roofed_enclosure)+(0.36*time_in_offfarm_grazing)), #equation 10.4
                                ifelse(livestock_category_name == "Sheep - Ewes",body_weight*((0.0096*time_in_stable)+(0.0096*time_in_non_roofed_enclosure)+(0.0107*time_in_onfarm_grazing)+(0.024*time_in_offfarm_grazing)), #equation 10.5
                                       ifelse(livestock_category_name%in%c("Sheep - Breeding Rams","Sheep - Fattening Rams","Sheep - Lambs"),body_weight*((0.0067*time_in_stable)+(0.0067*time_in_non_roofed_enclosure)+(0.0107*time_in_onfarm_grazing)+(0.024*time_in_offfarm_grazing)),  #equation 10.5
                                              ifelse(livestock_category_name == "Goats - Does", body_weight*((0.0096*time_in_stable)+(0.0096*time_in_non_roofed_enclosure)+(0.019*time_in_onfarm_grazing)+(0.024*time_in_offfarm_grazing)),  #equation 10.5
                                                     ifelse(livestock_category_name%in%c("Goats - Breeding Bucks","Goats - Fattening Bucks","Goats - Kids"),body_weight*((0.0067*time_in_stable)+(0.0067*time_in_non_roofed_enclosure)+(0.019*time_in_onfarm_grazing)+(0.024*time_in_offfarm_grazing)),  #equation 10.5
                                                            0))))))

  #Growth energy
  growth_er <- activity_er%>%
    mutate(er_growth = ifelse(livestock_category_name%in%Pigs,annual_growth*45/no_days, #this equation is not explained
                              ifelse(livestock_category_name%in%Cattle_n_Buffalo_lactating_cows,22.02*((body_weight/(0.8*adult_weight))**0.75)*((annual_growth/no_days)**1.097), #equation 10.6 cows
                                     ifelse(livestock_category_name%in%Cattle_n_Buffalo,22.02*((body_weight/(1*adult_weight))**0.75)*((annual_growth/no_days)**1.097), #equation 10.6 castrates
                                            ifelse(livestock_category_name%in%Cattle_n_Buffalo_bulls,22.02*((body_weight/(1.2*adult_weight))**0.75)*((annual_growth/no_days)**1.097), #equation 10.6 bulls
                                                   ifelse(livestock_category_name%in%Goats,(annual_growth*(5+(0.5*0.33*(body_weight_weaning+body_weight_year_one))))/no_days, #equation 10.7 goats
                                                          ifelse(livestock_category_name == "Sheep - Ewes",(annual_growth*(2.1+(0.5*0.45*(body_weight_weaning+body_weight_year_one))))/no_days,  #equation 10.7 sheep - ewes
                                                                 ifelse(livestock_category_name == "Sheep - Breeding Rams",(annual_growth*(2.5+(0.5*0.35*(body_weight_weaning+body_weight_year_one))))/no_days, #equation 10.7 sheep - intact males
                                                                        ifelse(livestock_category_name == "Sheep - Fattening Rams",(annual_growth*(4.4+(0.5*0.32*(body_weight_weaning+body_weight_year_one))))/no_days, #equation 10.7 sheep - castrates
                                                                               ifelse(livestock_category_name == "Sheep - Lambs",(annual_growth*(2.3+(0.5*0.4*(body_weight_weaning+body_weight_year_one))))/no_days, #equation 10.7 sheep - lambs
                                                                                      0))))))))),
           er_growth = ifelse(!is.finite(er_growth),0,er_growth))

  #Lactation energy
  lactation_er <- growth_er%>%
    mutate(ev = ifelse(livestock_category_name %in% Cattle_n_Buffalo_lactating_cows, 1.47+(0.40*fat_content),
                       ifelse(livestock_category_name == "Sheep - Ewes",4.6,
                              ifelse(livestock_category_name == "Goats - Does",3,0))),
           er_lact = ifelse(livestock_category_name%in%c("Sheep - Ewes","Goats - Does") & annual_milk == 0,5*(body_weight_weaning/no_days)*ev,  #equation 10.10
                                ifelse(livestock_category_name=="Pigs - lactating/pregnant sows",(((piglets_relying_on_milk/100)*lactation_length*((6.83*litter_size*lw_gain)-(0.125*litter_size)))*4.2)/no_days/birth_interval, #not from the ipcc
                                       (annual_milk*ev)/no_days)), #equation 10.8 & equation 10.9
           er_lactation = ifelse(!is.finite(er_lact),0,er_lact))

  #Pregnancy energy
  pregnancy_er <- lactation_er%>%
    mutate(er_preg = ifelse(livestock_category_name=="Pigs - lactating/pregnant sows",(171/no_days)/birth_interval,
                                 er_maintenance*(0.1/birth_interval)),  #equation 10.13
           er_pregnancy = ifelse(!is.finite(er_preg),0,er_preg))

  #Work energy
  work_er <- pregnancy_er%>%
    mutate(er_work = er_maintenance*0.10*work_hour,#equation 10.11
           er_work = ifelse(!is.finite(er_work),0,er_work))

  #wool energy
  wool_er <- work_er%>%
    mutate(er_wool = (24*annual_wool)/no_days,#equation 10.12
           er_wool = ifelse(!is.finite(er_wool),0,er_wool))

  #Computing Gross Energy and DMI

  #Compute annual energy and protein required
  annual_requirement <- wool_er%>%
    mutate(energy_required_annually=(er_maintenance+er_activity+er_growth+er_lactation+er_pregnancy+er_work+er_wool)*no_days*herd_composition,
           protein_required_annually =((cp_maintenance*no_days)+(cp_grazing*grazing_displacement*no_days)+
                                         ifelse(!is.finite(((cp_pregnancy/(no_days*birth_interval))*no_days)),0,((cp_pregnancy/(no_days*birth_interval))*no_days))+
                                         ifelse(!is.finite(((cp_lactation/(no_days*birth_interval))*no_days)),0,((cp_lactation/(no_days*birth_interval))*no_days))+
                                         (annual_milk*cp_lactmilk)+(annual_growth*cp_growth))*herd_composition)

  #get livestock energy and protein requirement per season
  for (i in 1:nrow(seasons)) {
    sl <- seasons$season_length[i]

    #Preparing the seasonal feed quality
    s_feed_basket_quality <- filter(feed_basket_quality, season_name == seasons$season_name[i])%>%
      gather(feed,value,-season_name,-livestock_category_code,-livestock_category_name,-feed_variables)%>%
      spread(feed_variables,value)%>%
      mutate(prod_me = fraction_as_fed*me_content_fresh,
             prod_dm = fraction_as_fed*dm_content,
             prod_cp = fraction_as_fed*cp_content_fresh)%>%
      select(livestock_category_code,prod_me,prod_dm,prod_cp)%>%
      group_by(livestock_category_code)%>%
      summarise(average_me = sum(prod_me),
                average_dm = sum(prod_dm),
                average_cp=sum(prod_cp))


    #Computation of seasonal requirements
    temp <- annual_requirement%>%
      mutate(season_name = seasons$season_name[i],
             energy_required_by_season = energy_required_annually*(sl/no_days),#compute energy require by season
             protein_required_by_season = protein_required_annually*(sl/no_days))%>% #compute protein require by season
      left_join(s_feed_basket_quality, by = "livestock_category_code")%>%
      mutate(fresh_intake_required_e = energy_required_by_season/average_me,
             dmi_required_e = fresh_intake_required_e*average_dm/100,
             fresh_intake_required_cp = protein_required_by_season/(average_cp/100),
             dmi_required_cp = fresh_intake_required_cp*average_dm/100,
             dmi_s = ifelse(dmi_required_cp>dmi_required_e,dmi_required_cp,dmi_required_e),
             limiting = ifelse(dmi_required_e == 0 |dmi_required_cp == 0, NA,
                               ifelse(dmi_required_cp>dmi_required_e,"CP","ENERGY")),
             surplus_season = ifelse(is.na(limiting),NA,
                                     ifelse(limiting=="CP",(dmi_required_cp-dmi_required_e)/dmi_required_e,
                                            (dmi_required_e-dmi_required_cp)/dmi_required_cp)),
             me_intake_s = dmi_s*average_me*100/average_dm)

    #Binding seasonal results
    if (i==1) {df <- temp}
    else{df <- rbind(df,temp)}
  }

  # Annual limiting factor
  limiting_factor <- df%>%
    group_by(livestock_category_code,livestock_category_name)%>%
    summarise(dmi_energy_total = sum(dmi_required_e,na.omit=TRUE),
              dmi_cp_total = sum(dmi_required_cp,na.omit=TRUE))%>%
    mutate(limiting_total = ifelse(dmi_cp_total>dmi_energy_total,"CP","ENERGY"),
           surplus_total = ifelse(limiting_total == "CP", (dmi_cp_total-dmi_energy_total)/dmi_energy_total,
                                  (dmi_energy_total-dmi_cp_total)/dmi_cp_total))


  # Manure computation
  manure_comp <- df%>%group_by(livestock_category_code)%>%
    summarise(me_intake = sum(me_intake_s),
              dmi_tot = sum(dmi_s))%>%
    mutate(de_intake = me_intake/0.81,
           ge_intake = dmi_tot*18.45,
           annual_manure_produced = (dmi_tot*0.365),
           daily_manure_produced = annual_manure_produced/365)%>%
    left_join(livestock, by = "livestock_category_code")%>%
    mutate(manure_onfarm_grazing = (annual_manure_produced*time_in_onfarm_grazing)-(annual_manure_produced*time_in_onfarm_grazing*manure_in_field),
           n_content_manure_grazing = manure_onfarm_grazing*n_content,
           manure_collected = annual_manure_produced*((time_in_stable*manure_in_stable)+
                                                        (time_in_non_roofed_enclosure*manure_in_non_roofed_enclosure)+
                                                        (time_in_onfarm_grazing*manure_in_field))*manure_onfarm_fraction,
           n_content_manure_collected = manure_collected*n_content,
           n_content_manure_total = n_content_manure_grazing+n_content_manure_collected,
           manure_exported = manure_collected*manure_sales_fraction)%>%
    select(livestock_category_code,me_intake,dmi_tot,de_intake,ge_intake,annual_manure_produced,daily_manure_produced,manure_onfarm_grazing,
           n_content_manure_grazing,manure_collected,n_content_manure_collected,n_content_manure_total,manure_exported)

  annual_results <- left_join(annual_requirement, limiting_factor, by = c("livestock_category_code","livestock_category_name"))%>%
    left_join(manure_comp, by = "livestock_category_code")

  seasonal_results <- select(df,
                             season_name,
                             livestock_category_code,
                             livestock_category_name,
                             energy_required_by_season,
                             protein_required_by_season,
                             fresh_intake_required_e,
                             dmi_required_e,
                             fresh_intake_required_cp,
                             dmi_required_cp,
                             dmi_s,
                             limiting,
                             surplus_season,
                             me_intake_s)

  #return results
  results <- list(annual_results = annual_results,
                  seasonal_results = seasonal_results)
  return(results)
}
