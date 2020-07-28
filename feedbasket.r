# CLEANED XtRa Feedbasket Coputation
#
#Preparing a working environment
#Loading packages
reqPackages = c("jsonlite","tidyverse")
numPackages = length(reqPackages)
for (pkg in 1:numPackages) {
  package = reqPackages[pkg]
  if (!is.element(package, installed.packages()[,1])) {
    install.packages(package,repos = 'http://cran.us.r-project.org')
  }else{
    Exists = paste(package, "...Package",".......E X I S T S ",sep = "")
    print(Exists)
  }
}

lapply(reqPackages,require,character.only=TRUE)

#Working directory
path <- "C:/Users/S.Oloo/Documents/ILRI Work/CLEANED R vs Ex/Gitpulledcleaned/cleaned-XtRa"

#Loading data
para <- fromJSON(paste0(path,"/data/example.json"), flatten = TRUE)

#Function computing energy requirement
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

energy_required <- energy_requirement(para)

