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
energy_requirement <- function(para){
  
  #get the seasons
  seasons <- para[["seasons"]]
  
  #get livestock requirement per season
  for (i in 1:nrow(seasons)) {
    
    livestock <- unnest(para[["livestock"]],cols = c(livestock))%>%filter(season_name%in%seasons$season_name[i])
    temp <- livestock%>%select(season_name,livestock_category_code,livestock_category_name)
    er <- paste("energy_required_s", i, sep = "")
    cp <- paste("protein_required_s", i, sep = "")
    
    #convert columns to numeric
    cols_nam <- colnames(livestock%>%select(-season_name,-livestock_category_code,-livestock_category_name))
    livestock[cols_nam] <- sapply(livestock[cols_nam],as.numeric)
    #change NAs to 0
    livestock[is.na(livestock)] <- 0 
    #compute energy require by season
    temp[,er] <- ((livestock$er_maintenance*seasons$season_legnth[i])+(livestock$er_grazing*livestock$grazing_displacement*seasons$season_legnth[i])+
                    ifelse(is.nan(((livestock$er_pregnancy/(365*livestock$birth_interval))*seasons$season_legnth[i])),0,((livestock$er_pregnancy/(365*livestock$birth_interval))*seasons$season_legnth[i]))+
                    ifelse(is.nan(((livestock$er_lactation/(365*livestock$birth_interval))*seasons$season_legnth[i])),0,((livestock$er_lactation/(365*livestock$birth_interval))*seasons$season_legnth[i]))+
                    (0*livestock$er_lactmilk)+(0*livestock$er_growth))*livestock$herd_composition
    #compute protein require by season
    temp[,cp] <- ((livestock$cp_maintenance*seasons$season_legnth[i])+(livestock$cp_grazing*livestock$grazing_displacement*seasons$season_legnth[i])+
                    ifelse(is.nan(((livestock$cp_pregnancy/(365*livestock$birth_interval))*seasons$season_legnth[i])),0,((livestock$cp_pregnancy/(365*livestock$birth_interval))*seasons$season_legnth[i]))+
                    ifelse(is.nan(((livestock$cp_lactation/(365*livestock$birth_interval))*seasons$season_legnth[i])),0,((livestock$er_lactation/(365*livestock$birth_interval))*seasons$season_legnth[i]))+
                    (0*livestock$cp_lactmilk)+(0*livestock$cp_growth))*livestock$herd_composition
    if (i==1) {df <- temp}
    else{df <- left_join(df,temp,by = c("livestock_category_code","livestock_category_name"))}
  }
  #Compute annual values
  df <- df%>%dplyr::mutate(energy_required_annually=df%>%dplyr::select(starts_with("energy_required"))%>%rowSums(na.rm = TRUE),
                           protein_required_annually=df%>%dplyr::select(starts_with("protein_required"))%>%rowSums(na.rm = TRUE))
  return(df)
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
        
        feed_item <- feed_item %>% select(feed_item_code, feed_item_name, 
                                          cp_content, me_content, dm_content)
        
        # Convert columns of interest to numeric
        feed_item <- feed_item %>% mutate_at(c("cp_content", "me_content", 
                                               "dm_content"), as.numeric) %>% mutate(de_fraction = me_content * 
                                                                                       0.066)  # calculate de content
        
        
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
        gather(feed_variables, value, cp_content:fraction_as_fed) %>% 
        spread(feed_item_name, value) %>% 
        mutate_at(c(2,8), as.numeric)
      
      # Calculate fraction of dry matter
      feed_allocation_all <- rbind(feed_allocation_all, c(feed_variables = "fraction_dry_matter", 
                                   feed_allocation_all[feed_allocation_all$feed_variables == "fraction_as_fed", -1] * feed_allocation_all[feed_allocation_all$feed_variables == "dm_content", -1]/sum(unlist(feed_allocation_all[feed_allocation_all$feed_variables == "dm_content", -1]))))


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