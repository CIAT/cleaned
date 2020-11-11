#' @title Energy requirement
#'
#' @description It calculates energy requirement. Compute this after
#' the `feed_quality` function
#'
#' @param para A json file
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

energy_requirement <- function(para,feed_basket_quality){
  #get the seasons
  seasons <- para[["seasons"]]
  livestock <- para[["livestock"]]
  no_days <- 365

  #convert columns to numeric
  cols_nam <- colnames(livestock%>%select(-livestock_category_code,-livestock_category_name))
  livestock[cols_nam] <- sapply(livestock[cols_nam],as.numeric)
  #change NAs to 0
  livestock[is.na(livestock)] <- 0

  #Compute annual energy and protein required
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
             limiting = ifelse(dmi_required_e == 0 |dmi_required_cp == 0, NA,
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
                                                        (time_in_onfarm_grazing*manure_in_field))*manure_as_fertilizer,
           n_content_manure_collected = manure_collected*n_content,
           n_content_manure_total = n_content_manure_grazing+n_content_manure_collected)%>%
    select(livestock_category_code,me_intake,dmi_tot,de_intake,ge_intake,annual_manure_produced,daily_manure_produced,manure_onfarm_grazing,
           n_content_manure_grazing,manure_collected,n_content_manure_collected,n_content_manure_total)

  annual_results <- left_join(annual_requirement,manure_comp)
  seasonal_results <- select(df,season_name,livestock_category_code,livestock_category_name,energy_required_by_season,protein_required_by_season,
                             fresh_intake_required_e,dmi_required_e,fresh_intake_required_cp,dmi_required_cp,dmi_s,limiting,me_intake_s)
  #return results
  results <- list(annual_results,seasonal_results)
  return(results)
}
