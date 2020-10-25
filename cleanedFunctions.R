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
  
  livestock_df <- para[["livestock"]]
  
  livestock_category_names <- c(livestock_df$livestock_category_code)
  
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
        
        feeding_seasons <- feeding_seasons %>% 
          mutate(livestock_category_name = ifelse(feeding_seasons$livestock_category_code  == livestock_df$livestock_category_code, livestock_df$livestock_category_name, NA))
        
        
        # feeding_seasons <- feeding_seasons %>% 
        #   mutate(livestock_category_name = ifelse(livestock_category_code %in% "01", "Cows (local)", 
        #                                           ifelse(livestock_category_code %in% "02", "Cows (improved)", 
        #                                                  ifelse(livestock_category_code %in% "03", "Adult cattle - male", 
        #                                                         ifelse(livestock_category_code %in% "04", "Calves", 
        #                                                                ifelse(livestock_category_code %in% "05", "Buffalo (dairy)", 
        #                                                                       ifelse(livestock_category_code %in% "06", "Sheep/Goats - Ewes/Does", 
        #                                                                              ifelse(livestock_category_code %in% "07", "Pigs - lactating/pregnant sows", 
        #                                                                                     ifelse(livestock_category_code %in% "08", "Calves", 
        #                                                                                            ifelse(livestock_category_code %in% "09", "Cows (high productive)", 
        #                                                                                                   "Error"))))))))))
        
        
        
        
        
        livestock_selected <- feeding_seasons[feeding_seasons$livestock_category_code == livestock, ]
        
        feed_item_select <- as.data.frame(livestock_selected[["allocation:"]])
        
        # select feed item
        feed_item_selected <- feed_item_select[feed_item_select$feed_item_code == feed_item$feed_item_code, ]
        
        feed_allocation[[i]] <- feed_item %>% mutate(fraction_as_fed = as.numeric(feed_item_selected$allocation)/100)
      }
      
      # Bind by rows
      feed_allocation_all <- feed_allocation %>% bind_rows() %>% 
        select(-feed_item_code)
      
      # Gather
      feed_allocation_all <- feed_allocation_all %>% 
        gather(feed_variables, value, cp_content_fresh:fraction_as_fed) %>% 
        spread(feed_item_name, value) %>% 
        mutate_at(-1, as.numeric)
      
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

# Compute land requirements
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
                                               as.numeric(feed_item_selected$residue_removal), 0),
                 area_total = ifelse(feed_item_selected$source_type == "Main", 
                                     feed_item_dm/(crop_yield*crop_removal),
                                     ifelse(feed_item_selected$source_type != "Main", feed_item_dm/(cr_yield*crop_residue_removal), 0)),
                 area_non_feed = ifelse(crop_residue_removal > 0,
                                        area_total*(crop_yield*crop_removal)/(crop_yield*crop_removal+cr_yield*crop_residue_removal), 0),
                 area_feed = ifelse(crop_residue_removal > 0, 
                                    area_total*(cr_yield*crop_residue_removal)/(crop_yield*crop_removal+cr_yield*crop_residue_removal), 
                                    area_total*(crop_yield*crop_removal+cr_yield*crop_residue_removal)/(crop_yield*crop_removal+cr_yield*crop_residue_removal))) %>% 
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

# Compute soil health
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

water_requirement <- function(para,land_required){
  #getting the crop parameters
  feed_production <- unnest(para[["feed_production"]], cols = c(feed_type_name))
  
  #convert columns to numeric
  cols_nam <- colnames(feed_production%>%select(kc_initial,kc_midseason,kc_late))
  feed_production[cols_nam] <- sapply(feed_production[cols_nam],as.numeric)
  #change NAs to 0
  feed_production[is.na(feed_production)] <- 0
  
  #getting the livestock parameters
  livestock <- unnest(para[["livestock"]])
  
  #convert columns to numeric
  cols_nam <- colnames(livestock%>%select(-livestock_category_code,-livestock_category_name))
  livestock[cols_nam] <- sapply(livestock[cols_nam],as.numeric)
  #change NAs to 0
  livestock[is.na(livestock)] <- 0
  
  #area evapotranspiration
  et <- as.numeric(para[["et"]])
  #annual precipitation
  annual_precipitation <- as.numeric(para[["annual_precipitation"]])
  
  #computing water use per feed item
  water_use_per_feed_item <- land_required%>%
    group_by(feed)%>%
    summarise(area_feed = sum(area_feed, na.rm = T),
              area_non_feed = sum(area_non_feed, na.rm = T),
              area_total = sum(area_total, na.rm = T))%>% #reformating land required
    left_join(feed_production, by = c("feed"="feed_type_name"))%>%
    mutate(fraction_of_land_required = area_feed/sum(area_feed,na.rm = T),
           kc_average = (kc_initial+kc_midseason+kc_late)/3,
           kc_frac = fraction_of_land_required*kc_average,
           ET = kc_frac*et,
           water_use = ET*sum(area_feed),
           feed_water_use = ifelse(is.nan(water_use*(1-(area_non_feed/area_total))),0,(water_use*(1-(area_non_feed/area_total)))),
           non_feed_water_use = water_use-feed_water_use)%>%
    select(feed,area_feed,area_non_feed,area_total,kc_average,kc_frac,ET,water_use,feed_water_use,non_feed_water_use)
  
  #computing water use for production
  ET <- et*sum(water_use_per_feed_item$kc_frac)
  fraction_of_precipitation_used_for_feed_production <- ET/annual_precipitation
  total_water_use <- ET*sum(water_use_per_feed_item$area_feed)
  water_use_fpcm <- total_water_use/sum(livestock$herd_composition*livestock$annual_milk*(0.337+(0.116*livestock$fat_content)+(0.06*livestock$protein_milkcontent)))
  water_use_meat <- total_water_use/sum(livestock$herd_composition*livestock$annual_growth*livestock$carcass_fraction)
  water_use_protein <- total_water_use/(sum(livestock$herd_composition*livestock$annual_growth*livestock$carcass_fraction*(livestock$protein_meatcontent/100))+
                                          sum(livestock$herd_composition*livestock$annual_milk*(0.337+(0.116*livestock$fat_content)+(0.06*livestock$protein_milkcontent))*(livestock$protein_milkcontent/100)))
  
  #merging water use items for production
  water_use_for_production <- cbind(ET,fraction_of_precipitation_used_for_feed_production,total_water_use,water_use_fpcm,water_use_meat,water_use_protein)
  
  water_use <- list(water_use_per_feed_item,water_use_for_production)
  
  #returning results
  return(water_use)
} #end of water function


# Compute N balance
nitrogen_balance <- function(para, land_required, soil_erosion){
  
  feed_types <- unique(land_required$feed)
  
  n_balance <- list()
  
  for (feed in feed_types){
    
    feed_production <- unnest(para[["feed_production"]], cols = c(feed_type_name))
    
    feed_production <- na_if(feed_production, "NA") %>% 
      as.data.frame()
    
    feed_production[is.na(feed_production)] <- 0
    
    feed_selected <- feed_production[feed_production$feed_type_name == feed,]
    
    dry_yield <- feed_selected$dry_yield
    
    residue_dry_yield <- feed_selected$residue_dry_yield
    
    main_n <- feed_selected$main_n
    
    residue_n <- as.numeric(feed_selected$residue_n)
    
    n_fixing <- ifelse(feed_selected$feed_category == "Legume", 0.5*(residue_n*residue_dry_yield+main_n*dry_yield)*1000, 0)
    
    feed_selected_land_required <- land_required[land_required$feed == feed,]
    
    area_total <- sum(feed_selected_land_required$area_feed)
    
    feed_item_selected <- as.data.frame(feed_selected[["feed_items"]])
    
    feed_item_selected <- na_if(feed_item_selected, "NA") %>% 
      as.data.frame()
    
    feed_item_selected[is.na(feed_item_selected)] <- 0
    
    manure_fraction <- as.numeric(feed_item_selected$manure_fraction)
    
    fertilizer_rate <- as.numeric(feed_item_selected$fertilizer_rate)
    
    main_product_removal <- as.numeric(feed_item_selected$main_product_removal)
    
    residue_removal <- as.numeric(feed_item_selected$residue_removal)
    
    sum_n_content_manure_grazing <- energy_required[1] %>% 
      as.data.frame() %>% 
      summarise(sum(n_content_manure_grazing)) %>% 
      as.numeric()
    
    yield_dm_ha <- as.numeric(dry_yield)*1000
    
    main_product_removed_kg_ha <- yield_dm_ha*main_product_removal
    
    n_content_manure_collected <- energy_required[1] %>% 
      as.data.frame() %>% 
      summarise(sum(n_content_manure_collected)) %>% 
      as.numeric()
    
    animal_manure_collected <- n_content_manure_collected*manure_fraction
    
    organic_n_imported <- manure_fraction*(as.numeric(para$purchased_manure)+as.numeric(para$purchased_compost)+as.numeric(para$purchased_organic_n)+as.numeric(para$purchased_bedding))
    
    crop_residue_dm_ha <- as.numeric(feed_selected$residue_dry_yield)*1000
    
    residue_removal <- as.numeric(feed_item_selected$residue_removal)
    
    main_product_removed_kg <- area_total*main_product_removed_kg_ha
    
    residue_removed_dm_ha <- crop_residue_dm_ha*residue_removal
    
    residue_removed_kg <- area_total*residue_removed_dm_ha
    
    annual_precipitation <- as.numeric(para[["annual_precipitation"]])
    
    soil_n <- as.numeric(para[["soil_n"]])
    
    ntot_kg_ha_20cm <- soil_n*20*as.numeric(para[["soil_bulk"]])*10
    
    n_mineralized_kg_ha_year <- ntot_kg_ha_20cm*0.03
    
    soil_c <- as.numeric(para[["soil_c"]])
    
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
    out2 <- ifelse(feed_item_selected$source_type == "Main", 0, residue_removed_dm_ha * nres * area_total)
    
    # Soil clay content
    soil_clay <- soil_type <- as.numeric(para[["soil_clay"]])
    
    # soil loss per plot per feed type
    soil_loss_plot <- as.numeric(soil_erosion[soil_erosion$feed_type == feed,]$soil_loss_plot)

    # Soil erosion
    out5 <- soil_loss_plot*soil_n*1.5
    
    # N content (kgN/kg DM ) from GHG parameters
    nfertilizer <- 0
    

    # write data into a dataframe
    n_balance[[feed]] <- as.data.frame(cbind(feed,
                                             n_fixing,
                                             area_total,
                                             fertilizer_rate,
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
                                             soil_clay,
                                             out5, nfertilizer))
    
    
  }
  
  n_balance_all <- n_balance %>% 
    bind_rows() %>% 
    mutate_at(c(-1), as.numeric)
  
  # Animal manure (N kg) grazing, Organic N (kg N) total, Organic N (kg N/ha) total
  n_balance_all <- n_balance_all %>%
    mutate(animal_manure_grazing = sum_n_content_manure_grazing * (main_product_removed_kg+residue_removed_kg)/(sum(n_balance_all$main_product_removed_kg)+sum(n_balance_all$residue_removed_kg)),
           organic_n_kg_total = animal_manure_grazing+animal_manure_collected+organic_n_imported,
           organic_n_kg_per_ha = ifelse(is.na(organic_n_kg_total/area_total), 0, organic_n_kg_total/area_total))
  
  # Manure (kgN)
  n_balance_all$in2 <- n_balance_all$organic_n_kg_total
  
  # N leached (kg N/ha/yr) @clay < 35%, >35% and <55%, >55%, Gaseous losses
  n_balance_all <- n_balance_all %>% 
    mutate(out3a = (n_mineralized_kg_ha_year + fertilizer_rate + in2) * (0.021 * (annual_precipitation - 3.9) / 100),
           out3b = (n_mineralized_kg_ha_year + fertilizer_rate + in2)* (0.014 * annual_precipitation + 0.71) / 100,
           out3c = (n_mineralized_kg_ha_year + fertilizer_rate + in2) * (0.0071 * annual_precipitation + 5.4) / 100,
           out3 = ifelse(soil_clay <=35, out3a, ifelse(soil_clay >= 35, out3c, out3b)),
           out4 = (n_mineralized_kg_ha_year + fertilizer_rate + organic_n_kg_per_ha) * (-9.4 + 0.13 * soil_clay + 0.01 * annual_precipitation) / 100 * area_total,
           nin = ifelse(area_total>0, in1+in2+in3+in4a+in4b, 0),
           nout = ifelse(area_total>0, out1+out2+out3+out4+out5, 0),
           nbalance_kg_n_total = nin-nout,
           nbalance_kg_n_ha_total = ifelse(is.na(nbalance_kg_n_total/area_total), 0, nbalance_kg_n_total/area_total),
           nbalance_feed_only_kg_n = ifelse(nbalance_kg_n_total==0, 0, ifelse(out2==0, nbalance_kg_n_total*out2/(out2+out1), 0)),
           nbalance_feed_only_kg_n_ha = ifelse(is.na(nbalance_feed_only_kg_n/area_total), 0, nbalance_feed_only_kg_n/area_total))
  
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
           soil_clay,
           out3,
           out4,
           out5,
           nin,
           nout,
           nbalance_kg_n_total,
           nbalance_kg_n_ha_total,
           nbalance_feed_only_kg_n,
           nbalance_feed_only_kg_n_ha)
  

}

# Compute meat and milk productivity
meat_milk_productivity <- function(para){
  
  livestock_df <- para[["livestock"]]
  
  livestock_category_names <- c(livestock_df$livestock_category_name)
  
  livestock_production <- list()
  
  for (livestock in livestock_category_names){
    
    livestock_selected <- livestock_df[livestock_df$livestock_category_name == livestock,]
    
    livestock_selected <- na_if(livestock_selected, "NA") %>% 
      as.data.frame()
    
    livestock_selected[is.na(livestock_selected)] <- 0
    
    # prodution per livestock
    livestock_production[[livestock]] <- livestock_selected %>% 
      mutate(number = as.numeric(herd_composition),
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

# Economics
economics_payback <- function(para, energy_required){
  
  livestock_df <- para[["livestock"]]
  
  livestock_category_names <- c(livestock_df$livestock_category_name)
  
  # products dictionary
  farm_gate_price <- function(x){
    
    ifelse(x == "Cattle Manure", 0.0058, 
           ifelse(x == "Sheep Manure", NA, 
                  ifelse(x == "Goat Manure", NA, 
                         ifelse(x == "Beef", 3, 
                                ifelse(x == "Buffalo meat", NA, 
                                       ifelse(x == "Goat/Lamb/Mutton", NA, 
                                              ifelse(x == "Pork", NA, 
                                                     ifelse(x == "Cow Milk", 0.35, 
                                                            ifelse(x == "Buffalo Milk", NA, 
                                                                   ifelse(x == "Goat/sheep milk", NA, 
                                                                          ifelse(x == "Labour", 3.5, 
                                                                                 ifelse(x == "Urea", NA, 
                                                                                        ifelse(x == "NPK", NA, NA)))))))))))))
  }
  
  # Cattle manure
  cattle_manure <- energy_required[1] %>% 
    as.data.frame() %>%
    summarise(product="Cattle Manure",
              total_production_year= sum(annual_manure_produced),
              estimated_production=sum(manure_collected))
  
  # Beef
  beef <- livestock_productivity %>% 
    as.data.frame() %>%
    summarise(product="Beef",
              total_production_year= sum(meat_production_animal),
              estimated_production="")
  
  # Beef
  cow_milk <- livestock_productivity %>% 
    as.data.frame() %>%
    summarise(product="Cow Milk",
              total_production_year= sum(milk_production_animal),
              estimated_production="")

  #economics_all <- rbind.fill(list(cattle_manure, beef, cow_milk))
  
  economics_all <- plyr::rbind.fill(list(cattle_manure, 
                                         beef, 
                                         cow_milk)) %>% 
    mutate(retail_price_kg = farm_gate_price(product),
           total_value_production= retail_price_kg*total_production_year,
           estimated_production_value = ifelse(product=="Cattle Manure", retail_price_kg*as.numeric(estimated_production), ""))
    
}

# Biomass
biomass_calculations <- function(para, land_required){
  
  tier1 <- data.frame()
  
  # Land requirement for feed production per associated crop (ha)
  land_requirement_per_feed <- land_required %>%
    select(feed, area_feed) %>% 
    group_by(feed) %>% 
    summarise(area_feed = sum(area_feed))
  
  # add feed category
  feed_production <- unnest(para[["feed_production"]], cols = c(feed_type_name))
  
  feed_production <- na_if(feed_production, "NA") %>% 
    as.data.frame()
  
  land_requirement_per_feed$feed_category <- feed_production$feed_category[match(land_requirement_per_feed$feed, 
                                                                                 feed_production$feed_type_name)]
  # To be added by rein
  dbh <- ""
  trees_annual_growth <- 0
  trees_annual_removal <- 0
  
  # based on MICCA project (Kuyah et al 2012; Chave et Al. 2005
  tier3 <- land_requirement_per_feed %>% 
    mutate(area_ha = ifelse(feed_category=="tree crop" | feed_category=="tree legume", 
                         area_feed, 0),
           nb_trees = 0,
           dbh = ifelse(dbh>0, dbh, 0),
           agbest = ifelse(dbh=="", 0, 0.091*dbh^2.472),
           agb = agbest+((2.69/100)*agbest),
           carbon_content = 0.48,
           carbon_content_tree = agb*carbon_content,
           total_carbon_stock_ha = ifelse(is.na(agb), 0, nb_trees*carbon_content_tree),
           total_carbon_stock = area_ha*total_carbon_stock_ha,
           tree_annual_growth = trees_annual_growth,
           annual_growth_ha = nb_trees*carbon_content*tree_annual_growth,
           annual_growth = area_ha*annual_growth_ha,
           annual_removal = ifelse(tree_annual_growth>0, tree_annual_growth, 0),
           total_annual_removal = carbon_content*annual_removal,
           carbon_biomass_balance = annual_growth-annual_removal,
           carbon_stock_change_biomass = carbon_biomass_balance*44/12) %>% 
    select (-c(area_feed, feed_category))
  
  carbon_stocks_change <- list(tier1, tier3)
  
  #returning results
  return(carbon_stocks_change)
  
}

#GHG function
ghg_emission <- function(para,energy_required,ghg_ipcc_data,land_required,nitrogen_balance){
  livestock <- para[["livestock"]]
  #convert columns to numeric
  cols_nam <- colnames(livestock%>%select(-livestock_category_code,-livestock_category_name))
  livestock[cols_nam] <- sapply(livestock[cols_nam],as.numeric)
  #change NAs to 0
  livestock[is.na(livestock)] <- 0
  
  #
  no_days <- 365
  annual_energy <- energy_required[[1]] 
  seasons <- para[["seasons"]]
  ###########################################################################################################
  #Preparation of ghg parameters
  #
  #
  ghg_parameters <- function(para,seasons,annual_energy,ghg_ipcc_data){
    #Computation of ghg parameters from the feedbasket
    for (i in 1:nrow(seasons)) {
      sl <- seasons$season_length[i]
      
      #Preparing the seasonal feed quality
      seasonal_feed_parameters <- filter(feed_basket_quality, season_name == seasons$season_name[i])%>%
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
    mfc_stable = filter(ghg_ipcc_data[["table_10.17"]],system == manureman_stable);names(mfc_stable) <- c("stable_mgmt_syst","mfc_stable")
    manureman_yard <- para[["manureman_yard"]]
    mfc_yard = filter(ghg_ipcc_data[["table_10.17"]],system == manureman_yard);names(mfc_yard) <- c("yard_mgmt_syst","mfc_yard")
    manureman_pasture <- para[["manureman_pasture"]]
    mfc_pasture = filter(ghg_ipcc_data[["table_10.17"]],system == manureman_pasture);names(mfc_pasture) <- c("pasture_mgmt_syst","mfc_pasture")
    
    #direct nitrous oxide factor
    direct_n2o_stable <- filter(ghg_ipcc_data[["table_10.21"]],system == manureman_stable)%>%
      select(direct_nitrous_oxide_factor)
    names(direct_n2o_stable) <- "direct_n2o_stable"
    direct_n2o_yard <- filter(ghg_ipcc_data[["table_10.21"]],system == manureman_yard)%>%
      select(direct_nitrous_oxide_factor)
    names(direct_n2o_yard) <- "direct_n2o_yard"
    
    #fraction of N loss due to manure management system
    fraction_n_loss_mms_stable <- filter(ghg_ipcc_data[["table_10.22"]],system == manureman_stable)%>%
      select(anaimal_category,fraction_n_loss_mms)
    names(fraction_n_loss_mms_stable) <- c("anaimal_category","fraction_n_loss_mms_stable")
    fraction_n_loss_mms_yard <- filter(ghg_ipcc_data[["table_10.22"]],system == manureman_yard)%>%
      select(anaimal_category,fraction_n_loss_mms)
    names(fraction_n_loss_mms_yard) <- c("anaimal_category","fraction_n_loss_mms_yard")
    
    #n2o emissions from managed soils
    n2o_emissions_from_managed_soils <- filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "EF4")%>%
      select(n2o_emissions_from_managed_soils)
    
    #Cnotinent
    region <- para[["region"]]
    
    ghg_ipcc_parameters <- ghg_ipcc_data[["livestock_parameters"]]%>%
      filter(livestock_category_name %in% ghg_energy_parameters$livestock_category_name)%>%
      mutate(mfc_stable,mfc_yard,mfc_pasture,direct_n2o_stable,direct_n2o_yard,n2o_emissions_from_managed_soils)%>%
      left_join(filter(ghg_ipcc_data[["table_10A_9"]],Continent == region), by = c("IPCC Category - methane emissions manure - Tier 1" = "anaimal_category"))%>%
      left_join(filter(ghg_ipcc_data[["table_10.19"]],Continent  == region), by = c("IPCC-Category - Default N-excretion rates Tier 1" = "anaimal_category"))%>%
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
           tier_2_enteric_methane_emissions = ((daily_ge_intake*(methferm_tier2/100)*no_days)/55.65)/herd_composition, #equation 10.21
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
  cattle_pig_poultry_n_pature <- filter(ghg_enteric_manure,livestock_category_name%in%c("Cows (local)","Cows (improved)","Cows (high productive)",
                                                                                        "Adult cattle - male","Steers/heifers","Steers/heifers (improved)",         
                                                                                        "Calves","Calves (improved)","Buffalo (dairy)","Buffalo steers/heifers",
                                                                                        "Buffalo calves",  "Pigs - lactating/pregnant sows","Pigs - dry sows/boars","Pigs - growers"))%>%
    mutate(tier_1_onfarm = tier_1_annual_N_excretion*time_in_onfarm_grazing,
           tier_1_offfarm = tier_1_annual_N_excretion*time_in_offfarm_grazing,
           tier_2_onfarm = tier_2_annual_N_excretion*time_in_onfarm_grazing,
           tier_2_offfarm = tier_2_annual_N_excretion*time_in_offfarm_grazing)%>%
    select(livestock_category_code,livestock_category_name,tier_1_onfarm,tier_1_offfarm,tier_2_onfarm,tier_2_offfarm)
  
  sheep_and_other_n_pature <- filter(ghg_enteric_manure,livestock_category_name%in%c("Sheep/Goats - Ewes/Does","Sheep/Goats - Breeding Rams/Bucks","Sheep/Goats - Fattening Rams/Bucks","Sheep/Goats - Lambs/Kids"))%>%
    mutate(tier_1_onfarm = tier_1_annual_N_excretion*time_in_onfarm_grazing,
           tier_1_offfarm = tier_1_annual_N_excretion*time_in_offfarm_grazing,
           tier_2_onfarm = tier_2_annual_N_excretion*time_in_onfarm_grazing,
           tier_2_offfarm = tier_2_annual_N_excretion*time_in_offfarm_grazing)%>%
    select(livestock_category_code,livestock_category_name,tier_1_onfarm,tier_1_offfarm,tier_2_onfarm,tier_2_offfarm)
  
  ################################################################################################################################################################################################################################
  #GHG Burning
  crop <- unnest(para[["feed_production"]], cols = c(feed_type_name))
  
  
  for (i in 1:length(crop$feed_type_name)) {
    
    feed_selected <- crop %>% filter(feed_type_name %in% crop$feed_type_name[i])
    
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
  rice <- filter(crop_ghg_parameters,grepl('Rice', feed_type_name)) #Maize
  
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
    rownames_to_column()%>%
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
    rownames_to_column()%>%
    mutate(emission_factor_flooded_rice)
  
  names(tier_1_n_flooded_rice) <- c("anthropogenic_N_input","amount_of_N_applied","emission_factors")
  
  ### Anthropogenic N input type from Urine and dung inputs to grazed soils
  tier_1_cattle_pig_poultry_n_pature_onfarm <-sum(cattle_pig_poultry_n_pature$tier_1_onfarm,na.rm = TRUE)
  tier_1_sheep_and_other_n_pature_onfarm <- sum(sheep_and_other_n_pature$tier_1_onfarm,na.rm = TRUE)
  
  emission_factor_grazed_soils <- c("EF3PRP-CPP","EF3PRP-SO")
  
  tier_1_n_grazed_soils <- data.frame(rbind(tier_1_cattle_pig_poultry_n_pature_onfarm, tier_1_sheep_and_other_n_pature_onfarm))%>%
    rownames_to_column()%>%
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
    rownames_to_column()%>%
    mutate(emission_factor_managed_soil)
  
  names(tier_2_n_managed_soil) <- c("anthropogenic_N_input","amount_of_N_applied","emission_factors")
  
  ### Anthropogenic N input types to estimate annual direct N2O-N emissions produced from flooded rice
  
  tier_2_n_synthetic_fertilizer_flooded_rice <- tier_1_n_synthetic_fertilizer_flooded_rice
  
  tier_2_n_organic_manure_flooded_rice <- tier_1_n_organic_manure_flooded_rice
  
  tier_2_n_from_crop_residue_flooded_rice <- tier_1_n_from_crop_residue_flooded_rice
  
  tier_2_n_flooded_rice <- data.frame(rbind(tier_2_n_synthetic_fertilizer_flooded_rice, tier_2_n_organic_manure_flooded_rice, tier_2_n_from_crop_residue_flooded_rice))%>%
    rownames_to_column()%>%
    mutate(emission_factor_flooded_rice)
  
  names(tier_2_n_flooded_rice) <- c("anthropogenic_N_input","amount_of_N_applied","emission_factors")
  
  ### Anthropogenic N input type from Urine and dung inputs to grazed soils
  tier_2_cattle_pig_poultry_n_pature_onfarm <-sum(cattle_pig_poultry_n_pature$tier_2_onfarm,na.rm = TRUE)
  tier_2_sheep_and_other_n_pature_onfarm <- sum(sheep_and_other_n_pature$tier_2_onfarm,na.rm = TRUE)
  
  tier_2_n_grazed_soils <- data.frame(rbind(tier_2_cattle_pig_poultry_n_pature_onfarm, tier_2_sheep_and_other_n_pature_onfarm))%>%
    rownames_to_column()%>%
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
  
  tier_1_n_pature_onfarm <- sum(tier_1_cattle_pig_poultry_n_pature_onfarm,tier_1_sheep_and_other_n_pature_onfarm,na.rm = TRUE)
  
  tier_1_N20_indirect_emission <- as.data.frame(cbind(tier_1,
                                                      tier_1_n_synthetic_fertilizer_managed_soil,
                                                      tier_1_organic_n,
                                                      tier_1_n_pature_onfarm))
  
  names(tier_1_N20_indirect_emission) <- c("tier","n_synthetic_fertilizer_managed_soil","n_organic","n_pature_onfarm")
  
  ###tier 2
  tier_2 <- 2
  
  tier_2_organic_n <- sum(tier_2_n_organic_manure_managed_soil,tier_2_n_from_crop_residue_managed_soil,tier_2_n_organic_manure_flooded_rice,tier_2_n_from_crop_residue_flooded_rice,na.rm = TRUE)
  
  tier_2_n_pature_onfarm <- sum(tier_2_cattle_pig_poultry_n_pature_onfarm,tier_2_sheep_and_other_n_pature_onfarm,na.rm = TRUE)
  
  tier_2_N20_indirect_emission <- as.data.frame(cbind(tier_2,
                                                      tier_2_n_synthetic_fertilizer_managed_soil,
                                                      tier_2_organic_n,
                                                      tier_2_n_pature_onfarm))
  
  names(tier_2_N20_indirect_emission) <- c("tier","n_synthetic_fertilizer_managed_soil","n_organic","n_pature_onfarm")
  
  FracGASF <- filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "FracGASF")
  
  FracGASM <- filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "FracGASM")
  
  EF4 <- filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "EF4")
  
  EF3PRP_CPP <- filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "EF3PRP-CPP")
  
  EF3PRP_SO <- filter(ghg_ipcc_data[["table_11.1_&_table_11.3"]],emission_factors == "EF3PRP-SO")
  
  N20_onfarm_indirect_emission <- rbind.data.frame(tier_1_N20_indirect_emission,tier_2_N20_indirect_emission)%>%
    mutate(FracGASF = FracGASF$n2o_emissions_from_managed_soils,
           FracGASM = FracGASM$n2o_emissions_from_managed_soils,
           EF4 = EF4$n2o_emissions_from_managed_soils,
           annual_N20N_from_atmospheric_deposition = ((n_synthetic_fertilizer_managed_soil*FracGASF)+((n_organic+n_pature_onfarm)*FracGASM))*EF4*(44/28))
  
  ## Off-farm
  
  tier_1_cattle_pig_poultry_n_pature_offfarm <-sum(cattle_pig_poultry_n_pature$tier_1_offfarm,na.rm = TRUE)
  tier_1_sheep_and_other_n_pature_offfarm <- sum(sheep_and_other_n_pature$tier_1_offfarm,na.rm = TRUE)
  
  tier_2_cattle_pig_poultry_n_pature_offfarm <-sum(cattle_pig_poultry_n_pature$tier_2_offfarm,na.rm = TRUE)
  tier_2_sheep_and_other_n_pature_offfarm <- sum(sheep_and_other_n_pature$tier_2_offfarm,na.rm = TRUE)
  
  N20N_offfarm <- data.frame(rbind(tier_1_cattle_pig_poultry_n_pature_offfarm,tier_1_sheep_and_other_n_pature_offfarm,tier_2_cattle_pig_poultry_n_pature_offfarm,tier_2_sheep_and_other_n_pature_offfarm))%>%
    rownames_to_column()%>%
    rename(category = rowname,n_offfarm_pasture = rbind.tier_1_cattle_pig_poultry_n_pature_offfarm..tier_1_sheep_and_other_n_pature_offfarm..)%>%
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
                        cattle_pig_poultry_n_pature = cattle_pig_poultry_n_pature,
                        sheep_and_other_n_pature = sheep_and_other_n_pature,
                        ghg_off_farm = fetilizer_ghg,
                        ghg_burning = ghg_burn,
                        ghg_soil = ghg_soil,
                        ghg_rice = ghg_rice)
  
  return(ghg_emissions)
  
} #end of ghg function


