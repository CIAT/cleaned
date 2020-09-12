# CLEANED FUNCTIONS

###### Calculate length of seasons and feeding rations ######
season_length <- function(start, end){
  
  # Start of seasons
  start_wet_season <- as.Date(as.character(start), format="%Y/%m/%d")
  end_wet_season <- as.Date(as.character(end), format="%Y/%m/%d")
  
  # Check for leap years
  if (leap_year(start_wet_season)==TRUE){
    
    no_days <- 366
    
  }else{
    
    no_days <- 365
    
  }
  
  # Length of seasons
  `Wet season` <- as.numeric(end_wet_season-start_wet_season)
  `Dry season` <- as.numeric(no_days-`Wet season`)
  
  wet_feeding_season_ratio <- `Wet season`/no_days
  dry_feeding_season_ratio <- `Dry season`/no_days
  
  season_length <- data.frame("season_name"=c("Wet season", "Dry season"),
                              "season_length"=c(`Wet season`, `Dry season`),
                              "feeding_ration"=c(wet_feeding_season_ratio, dry_feeding_season_ratio))
  
  return(season_length)
  
}

# Compute energy requirement
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


# Compute feed quality
feed_quality <- function(para) {
  
  livestock <- para[["livestock"]]
  
  livestock_category_names <- c(livestock$livestock_category_code)
  
  livestock_allocation <- list()
  
  for (livestock in livestock_category_names) {
    
    seasons <- para[["seasons"]]
    
    # Compute feeding season ratio
    seasons <- seasons %>% mutate(feeding_ratio = season_length/sum(seasons$season_length))
    
    season_allocation <- list()
    
    for (season in 1:nrow(seasons)) {
      
      feed_production <- unnest(para[["feed_production"]], cols = c(feed_type_name))
      
      feed_types <- unique(feed_production$feed_type_name)
      
      feed_allocation <- list()
      
      for (i in 1:length(feed_types)) {
        
        feed_selected <- feed_production %>% filter(feed_type_name %in% 
                                                      feed_production$feed_type_name[i])
        
        feed_item <- as.data.frame(feed_selected[["feed_items"]])
        
        # calculate me and dm fresh
        feed_item <- feed_item %>% 
          mutate_at(c("cp_content","me_content", "dm_content"), as.numeric) %>% 
          select(feed_item_code, feed_item_name, cp_content, me_content, dm_content) %>% 
          mutate(me_content_fresh = dm_content * me_content/100,
                 cp_content_fresh = dm_content * cp_content/100, 
                 de_fraction = me_content * 0.066) %>% 
          select(feed_item_code, feed_item_name, cp_content_fresh, de_fraction, dm_content, me_content_fresh)
        

        # Extracting allocation
        feeding_seasons <- unnest(para[["livestock_feeding_seasons"]], 
                                  cols = c(livestock_categories)) %>% filter(season_name %in% 
                                                                               seasons$season_name[season])
        
        
        feeding_seasons <- feeding_seasons %>% mutate(livestock_category_name = ifelse(livestock_category_code %in% 
                                                                                         "01", "Cows (local)", ifelse(livestock_category_code %in% 
                                                                                                                        "02", "Cows (improved)", ifelse(livestock_category_code %in% 
                                                                                                                                                          "03", "Adult cattle - male", ifelse(livestock_category_code %in% 
                                                                                                                                                                                                "04", "Calves", ifelse(livestock_category_code %in% "05", 
                                                                                                                                                                                                                       "Buffalo (dairy)", ifelse(livestock_category_code %in% 
                                                                                                                                                                                                                                                   "06", "Sheep/Goats - Ewes/Does", ifelse(livestock_category_code %in% 
                                                                                                                                                                                                                                                                                             "07", "Pigs - lactating/pregnant sows", ifelse(livestock_category_code %in% 
                                                                                                                                                                                                                                                                                                                                              "08", "Calves", ifelse(livestock_category_code %in% 
                                                                                                                                                                                                                                                                                                                                                                       "09", "Cows (high productive)", "Error"))))))))))
        
        
        
        
        
        livestock_selected <- feeding_seasons[feeding_seasons$livestock_category_code == 
                                                livestock, ]
        
        feed_item_select <- as.data.frame(livestock_selected[["allocation:"]])
        
        # select feed item
        feed_item_selected <- feed_item_select[feed_item_select$feed_item_code == 
                                                 feed_item$feed_item_code, ]
        
        feed_allocation[[i]] <- feed_item %>% mutate(fraction_as_fed = as.numeric(feed_item_selected$allocation)/100)
      }
      
      # Bind by rows
      feed_allocation_all <- feed_allocation %>% bind_rows() %>% 
        select(-feed_item_code)
      
      # Gather
      feed_allocation_all <- feed_allocation_all %>% 
        gather(feed_variables, value, cp_content_fresh:fraction_as_fed) %>% 
        spread(feed_item_name, value) %>% 
        mutate_at(c(2,8), as.numeric)
      
      # calculate fraction of dry matter
      feed_allocation_all <- rbind(feed_allocation_all, c(feed_variables = "fraction_dry_matter", 
                                                          feed_allocation_all[feed_allocation_all$feed_variables == "fraction_as_fed", -1] * feed_allocation_all[feed_allocation_all$feed_variables == "dm_content", -1]/sum(unlist(feed_allocation_all[feed_allocation_all$feed_variables == "fraction_as_fed", -1] * feed_allocation_all[feed_allocation_all$feed_variables == "dm_content", -1]))))
      


      # Bind and add into the season list
      season_allocation[[season]] <- cbind(season_name = rep(livestock_selected$season_name, 
                                                             times = nrow(feed_allocation_all)), 
                                           livestock_category_code = rep(livestock_selected$livestock_category_code, times = nrow(feed_allocation_all)), 
                                           livestock_category_name = rep(livestock_selected$livestock_category_name, times = nrow(feed_allocation_all)), 
                                           feed_allocation_all)
      
      
    }
    
    # Bind by rows
    season_feed_allocation <- season_allocation %>% bind_rows()
    
    livestock_allocation[[livestock]] <- season_feed_allocation
    
  }
  
  # Bind by rows
  livestock_feed_allocation <- livestock_allocation %>% bind_rows()
  
}

land_requirement <- function(feed_basket_quality, energy_required, para){
  
  livestock_category_code <- unique(feed_basket_quality$livestock_category_code)
  
  livestock_requirements <- list()
  
  for (livestock in livestock_category_code){
    
    livestock_selected <- feed_basket_quality %>% 
      filter(livestock == livestock_category_code)
    
    seasons <- unique(feed_basket_quality$season_name)
    
    seasonal_requirements <- list()
    
    for (season in seasons){
      
      # select feed and transpose the data
      season_feeds <- livestock_selected %>% 
        filter(season == season_name) %>% 
        gather(feed,value,-season_name,-livestock_category_code,-livestock_category_name,-feed_variables)%>%
        spread(feed_variables,value)
      
      # select form energy requirment sheet
      season_selected_energy <- energy_required[2] %>% 
        as.data.frame() %>% 
        filter(livestock == livestock_category_code, season == season_name)
      
      feed_items <- unique(season_feeds$feed)
      
      land_requirements <- list()
      
      for (i in feed_items){
        
        # get crop yield
        feed_production <- unnest(para[["feed_production"]], cols = c(feed_type_name))
        feed_selected <- feed_production[feed_production$feed_type_name == i,]
        
        # get main product removal
        feed_item_selected <- as.data.frame(feed_selected[["feed_items"]])
        
        # selected feed from season feeds above
        selected_feed <- season_feeds[season_feeds$feed == i,]
        
        land_requirements[[i]] <- selected_feed %>% 
          select(feed) %>% 
          mutate(feed_item_dm = selected_feed$fraction_dry_matter*season_selected_energy$dmi_s,
                 crop_yield = as.numeric(feed_selected$dry_yield)*1000,
                 crop_removal = as.numeric(feed_item_selected$main_product_removal),
                 cr_yield = as.numeric(feed_selected$residue_dry_yield)*1000,
                 crop_residue_removal = ifelse(feed_item_selected$source_type == "Residue",
                                               crop_residue_removal <- as.numeric(feed_item_selected$residue_removal),
                                               crop_residue_removal <- 0),
                 area_total = ifelse(feed_item_selected$source_type == "Main",
                                     area_total <- feed_item_dm/(as.numeric(crop_yield)*as.numeric(crop_removal)),
                                     ifelse(feed_item_selected$source_type != "Main",
                                            area_total <- feed_item_dm/(as.numeric(cr_yield)*as.numeric(crop_residue_removal)),
                                            area_total <- 0)),
                 area_non_feed = ifelse(crop_residue_removal > 0,
                                        area_non_feed <- area_total*(as.numeric(crop_yield)*as.numeric(crop_removal)/(as.numeric(crop_yield)*as.numeric(crop_removal)+as.numeric(cr_yield)*as.numeric(crop_removal))), 
                                        area_non_feed <- 0),
                 area_feed = ifelse(crop_residue_removal > 0,
                                    area_feed <- area_total*(as.numeric(cr_yield)*as.numeric(crop_residue_removal)/(as.numeric(crop_yield)*as.numeric(crop_removal)+as.numeric(cr_yield)*as.numeric(crop_residue_removal))),
                                    area_feed <- area_total*(crop_yield*crop_removal+crop_yield)/(crop_yield*crop_removal+cr_yield*crop_residue_removal))) %>% 
          mutate_if(is.numeric, list(~na_if(.,Inf))) %>% 
          replace(is.na(.), 0)
        
      }
      
      land_requirements <- land_requirements %>% bind_rows()
      
      
      land_requirements <- cbind(season_name = rep(selected_feed$season_name, times = nrow(land_requirements)), 
                                 livestock_category_code = rep(selected_feed$livestock_category_code, times = nrow(land_requirements)), 
                                 livestock_category_name = rep(selected_feed$livestock_category_name, times = nrow(land_requirements)), 
                                 land_requirements)
      
      # bind by rows and add into seasonal requirement list
      seasonal_requirements[[season]] <- land_requirements %>% bind_rows()
      
      

    }
    
    livestock_requirements[[livestock]] <- seasonal_requirements %>% bind_rows()

  }
  
  land_requirements_all <- livestock_requirements %>% bind_rows()
  
}

# compute soil health
soil_health <- function(para, land_required) {
  
  no_days <- 365
  
  soil_type <- para[["soil_description"]]
  
  erosivity_r <- 0.55*(as.numeric(para[["annual_precipitation"]])/(as.numeric(para[["rain_length"]])/30))-4.7
  
  erodibility_k <- as.numeric(para[["soil_k_value"]])
  
  feed_production <- unnest(para[["feed_production"]], cols = c(feed_type_name))
  
  feed_types <- unique(feed_production$feed_type_name)
  
  soil_erosion_all_feed <- list()
  
  for (i in feed_types){
    
    feed_type <- i
    
    feed_selected <- feed_production[feed_production$feed_type_name == i,]
    
    feed_item_selected <- as.data.frame(feed_selected[["feed_items"]])
    
    slope_p_factor <- feed_item_selected$slope_p_factor
    
    slope_length <- feed_item_selected$slope_length
    
    slope_steepness_length_conversion <- function(x, y){
      z <- ifelse(x == "Flat (0-5%)" & y == "1", 0.3, 
                  ifelse(x == "Hilly (5-20%)" & y == "1", 0.52, 
                         ifelse(x == "Steep (20-30%)" & y == "1", 0.59, 
                                ifelse(x == "Extremely steep (30%+)" & y == "1", 0.65, 
                                       ifelse(x == "Flat (0-5%)" & y == "3", 0.3, 
                                              ifelse(x == "Hilly (5-20%)" & y == "3", 0.85, 
                                                     ifelse(x == "Steep (20-30%)" & y == "3", 1.13, 
                                                            ifelse(x == "Extremely steep (30%+)" & y == "3", 1.36, 
                                                                   ifelse(x == "Flat (0-5%)" & y == "5", 0.3,
                                                                          ifelse(x == "Hilly (5-20%)" & y == "5", 1.06,
                                                                                 ifelse(x == "Steep (20-30%)" & y == "5", 1.53,
                                                                                        ifelse(x == "Extremely steep (30%+)" & y == "5", 1.95, 
                                                                                               ifelse(x == "Flat (0-5%)" & y == "15", 0.49,
                                                                                                      ifelse(x == "Hilly (5-20%)" & y == "15", 2.22,
                                                                                                             ifelse(x == "Steep (20-30%)" & y == "15", 3.39,
                                                                                                                    ifelse(x == "Extremely steep (30%+)" & y == "15", 4.45, 
                                                                                                                           ifelse(x == "Flat (0-5%)" & y == "30", 0.65,
                                                                                                                                  ifelse(x == "Hilly (5-20%)" & y == "30", 3.4,
                                                                                                                                         ifelse(x == "Steep (20-30%)" & y == "30", 5.34,
                                                                                                                                                ifelse(x == "Extremely steep (30%+)" & y == "30", 7.14, 
                                                                                                                                                       ifelse(x == "Flat (0-5%)" & y == "90", 1.01,
                                                                                                                                                              ifelse(x == "Hilly (5-20%)" & y == "90", 6.68,
                                                                                                                                                                     ifelse(x == "Steep (20-30%)" & y == "90", 11.01,
                                                                                                                                                                            ifelse(x == "Extremely steep (30%+)" & y == "90", 15.14, NA))))))))))))))))))))))))
      return(z)
    }
    
    ls <- slope_steepness_length_conversion(slope_p_factor, slope_length)
    
    # calculate cover factor
    landcover_c_factor <- feed_item_selected$landcover_c_factor
    
    landcover_c_factor_conversion <- function(x){
      
      z <- ifelse(x == "Dense forest", 0.001, 
                  ifelse(x == "Other forest", 0.05, 
                         ifelse(x == "Badlands hard", 0.05, 
                                ifelse(x == "Badlands soft", 0.4, 
                                       ifelse(x == "Sorghum", 0.1, 
                                              ifelse(x == "Maize", 0.1, 
                                                     ifelse(x == "Cereals", 0.15, 
                                                            ifelse(x == "Pulses", 0.15, 
                                                                   ifelse(x == "Dense grass", 0.01, 
                                                                          ifelse(x == "Degraded grass", 0.05, 
                                                                                 ifelse(x == "Fallow hard", 0.05, 
                                                                                        ifelse(x == "Fallow plouged", 0.6, 
                                                                                               ifelse(x == "Ethiopian teff", 0.25, 
                                                                                                      ifelse(x == "Continuous fallow", 1, NA))))))))))))))
      return(z)
      
    }
    
    c_factor <- landcover_c_factor_conversion(landcover_c_factor)
    
    
    # calculate management factor
    management_factor_conversion <- function(x){
      
      ifelse(x == "Flat (0-5%)", 0.11, 
             ifelse(x == "Hilly (5-20%)", 0.13, 
                    ifelse(x == "Steep (20-30%)", 0.22, 
                           ifelse(x == "Extremely steep (30%+)", 0.37, 1))))
      
    }
    
    p_factor <- management_factor_conversion(slope_p_factor)
    
    # calculate Soil loss (t/ha/year)
    soil_loss_ha_year <- erosivity_r*erodibility_k*ls*c_factor*p_factor
    
    # select feed from land rquired dataframe
    land_required_feed_selected <- land_required[land_required$feed == i,]
    
    # land requirement for feed production (ha)
    land_required_feed_selected <- sum(land_required_feed_selected$area_feed)
    
    # calculate Soil loss (t/plot/ season)
    soil_loss_plot <- soil_loss_ha_year*land_required_feed_selected
    
    # write data into a dataframe
    soil_erosion_per_feed <- as.data.frame(cbind(feed_type, soil_type, erosivity_r, erodibility_k, ls, c_factor, p_factor, soil_loss_ha_year, soil_loss_plot))
    
    
    soil_erosion_all_feed[[i]] <- soil_erosion_per_feed

  }
  
  soil_erosion_all_feed <- soil_erosion_all_feed %>% bind_rows()
  
}
