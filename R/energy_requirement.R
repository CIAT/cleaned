#' @title Energy requirement
#'
#' @description It calculates energy requirement. Compute this after
#' the `feed_quality` function
#'
#' @param para A JSON file
#'
#' @param feed_basket_quality A dataframe computed using the `feed_quality` function
#'
#' @importFrom dplyr summarise mutate filter left_join %>%
#'
#' @importFrom tidyr gather spread
#'
#' @return dataframe
#'
#' @examples
#' \dontrun{
#' data(mufindi)
#' feed_basket_quality <- feed_quality(mufindi)
#' energy_requirement(mufindi,feed_basket_quality)
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
    mutate(er_maintenance = body_weight*maintenance_cfi**0.75) #equation 10.3

  #Activity energy

  #Growth energy
  growth_er <- livestock%>%
    mutate(er_growth = ifelse(livestock_category_name%in%Pigs,annual_growth*45/no_days, #this equation is not explained
                              ifelse(livestock_category_name%in%Cattle_n_Buffalo_lactating_cows,22.02*((body_weight/(0.8*adult_weight))**0.75)*((annual_growth/no_days)**1.097), #equation 10.6 cows
                                     ifelse(livestock_category_name%in%Cattle_n_Buffalo,22.02*((body_weight/(1*adult_weight))**0.75)*((annual_growth/no_days)**1.097), #equation 10.6 castrates
                                            ifelse(livestock_category_name%in%Cattle_n_Buffalo_bulls,22.02*((body_weight/(1.2*adult_weight))**0.75)*((annual_growth/no_days)**1.097), #equation 10.6 bulls
                                                   ifelse(livestock_category_name%in%Goats,(annual_growth*(5+(0.5*0.33*(body_weight_weaning+body_weight_year_one))))/no_days, #equation 10.7 goats
                                                          ifelse(livestock_category_name == "Sheep - Ewes",(annual_growth*(2.1+(0.5*0.45*(body_weight_weaning+body_weight_year_one))))/no_days,  #equation 10.7 sheep - ewes
                                                                 ifelse(livestock_category_name == "Sheep - Breeding Rams",(annual_growth*(2.5+(0.5*0.35*(body_weight_weaning+body_weight_year_one))))/no_days, #equation 10.7 sheep - intact males
                                                                        ifelse(livestock_category_name == "Sheep - Fattening Rams",(annual_growth*(4.4+(0.5*0.32*(body_weight_weaning+body_weight_year_one))))/no_days, #equation 10.7 sheep - castrates
                                                                               ifelse(livestock_category_name == "Sheep - Lambs",(annual_growth*(2.3+(0.5*0.4*(body_weight_weaning+body_weight_year_one))))/no_days, #equation 10.7 sheep - lambs
                                                                                      NA))))))))))

  #Lactation energy
  lactation_er <- livestock%>%
    mutate(ev = ifelse(livestock_category_name %in% Cattle_n_Buffalo_lactating_cows, 1.47+(0.40*fat_content),
                       ifelse(livestock_category_name == "Sheep - Ewes",4.6,
                              ifelse(livestock_category_name == "Goats - Does",3,0))),
           er_lactation = ifelse(livestock_category_name%in%c("Sheep - Ewes","Goats - Does") & annual_milk == 0,5*(body_weight_weaning/no_days)*ev,  #equation 10.10
                                ifelse(livestock_category_name=="Pigs - lactating/pregnant sows",(((piglets_relying_on_milk/100)*lactation_length*((6.83*litter_size*lw_gain)-(0.125*litter_size)))*4.2)/no_days/birth_interval, #not from the ipcc
                                       (annual_milk*ev)/no_days)))  #equation 10.8 & equation 10.9
  #Pregnancy energy
  pregnancy_er <- maintenance_er%>%
    mutate(er_pregnancy = ifelse(livestock_category_name=="Pigs - lactating/pregnant sows",(171/no_days)/birth_interval,
                                 er_maintenance*(0.1/birth_interval)))




  annual_requirement <- livestock%>%select(livestock_category_code,livestock_category_name)%>%
    mutate(energy_required_annually=((livestock$er_maintenance*no_days)+(livestock$er_grazing*livestock$grazing_displacement*no_days)+
                                       ifelse(is.nan(((livestock$er_pregnancy/(no_days*livestock$birth_interval))*no_days)),0,((livestock$er_pregnancy/(no_days*livestock$birth_interval))*no_days))+
                                       ifelse(is.nan(((livestock$er_lactation/(no_days*livestock$birth_interval))*no_days)),0,((livestock$er_lactation/(no_days*livestock$birth_interval))*no_days))+
                                       (livestock$annual_milk*livestock$er_lactmilk)+(livestock$annual_growth*livestock$er_growth))*livestock$herd_composition,
           protein_required_annually =((livestock$cp_maintenance*no_days)+(livestock$cp_grazing*livestock$grazing_displacement*no_days)+
                                         ifelse(is.nan(((livestock$cp_pregnancy/(no_days*livestock$birth_interval))*no_days)),0,((livestock$cp_pregnancy/(no_days*livestock$birth_interval))*no_days))+
                                         ifelse(is.nan(((livestock$cp_lactation/(no_days*livestock$birth_interval))*no_days)),0,((livestock$er_lactation/(no_days*livestock$birth_interval))*no_days))+
                                         (livestock$annual_milk*livestock$cp_lactmilk)+(livestock$annual_growth*livestock$cp_growth))*livestock$herd_composition)
  #get livestock energy and protein requirement per season
  for (i in 1:nrow(seasons)) {
    sl <- seasons$season_length[i]

    #Preparing the seasonal feed quality
    s_feed_basket_quality <- dplyr::filter(feed_basket_quality, season_name == seasons$season_name[i])%>%
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
             energy_required_by_season = annual_requirement$energy_required_annually*(sl/no_days),#compute energy require by season
             protein_required_by_season = annual_requirement$protein_required_annually*(sl/no_days))%>% #compute protein require by season
      left_join(s_feed_basket_quality, by = "livestock_category_code")%>%
      mutate(fresh_intake_required_e = energy_required_by_season/average_me,
             dmi_required_e = fresh_intake_required_e*average_dm/100,
             fresh_intake_required_cp = protein_required_by_season/(average_cp/100),
             dmi_required_cp = fresh_intake_required_cp*average_dm/100,
             dmi_s = ifelse(dmi_required_cp>dmi_required_e,dmi_required_cp,dmi_required_e),
             limiting = ifelse(dmi_required_e == 0 | dmi_required_cp == 0, NA,
                               ifelse(dmi_required_cp>dmi_required_e,"CP","ENERGY")),
             me_intake_s = dmi_s*average_me*100/average_dm)

    #Binding seasonal results
    if (i==1) {df <- temp}
    else{df <- rbind(df,temp)}
  }

  #Manure computation
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
           n_content_manure_total = n_content_manure_grazing+n_content_manure_collected)%>%
    select(livestock_category_code,me_intake,dmi_tot,de_intake,ge_intake,annual_manure_produced,daily_manure_produced,manure_onfarm_grazing,
           n_content_manure_grazing,manure_collected,n_content_manure_collected,n_content_manure_total)

  annual_results <- left_join(annual_requirement,manure_comp, by = "livestock_category_code")
  seasonal_results <- select(df,season_name,livestock_category_code,livestock_category_name,energy_required_by_season,protein_required_by_season,
                             fresh_intake_required_e,dmi_required_e,fresh_intake_required_cp,dmi_required_cp,dmi_s,limiting,me_intake_s)
  #return results
  results <- list(annual_results = annual_results,
                  seasonal_results = seasonal_results)
  return(results)
}
