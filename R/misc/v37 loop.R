
# Load packages and function
pacman::p_load(readxl,cleaned,jsonlite,tidyr,dplyr,miceadds,data.table,openxlsx)

source("R/ghg_emission_v2.R")
source("R/nitrogen_balance.R")
source("R/energy_requirement.R")
source("R/feed_quality.R")

# 1) Load Data ####
  ## 1.1) V37 data #####
  file<-"data/v37/Kenya NPA data.xlsx"
  save_file<-file.path(dirname(file),"Kenya NPA emissions.xlsx")
  # data <- readxl::read_excel(file,sheet=1)

  herd <- data.table(readxl::read_excel(file,sheet="milk-bodyweight"))
  herd[,livetype_code:=as.character(livetype_code)]
  setnames(herd,"livetype_desc","livetype_desc_v37")

  v37_feed_items<-data.table(readxl::read_excel(file,sheet="Feed_items"))
  v37_feed_items$intercrop<-as.numeric(0)
  v37_feed_type<-unique(data.table(readxl::read_excel(file,sheet="feed_type")))[order(feed_type_code)]

  # Deal with duplicate entry for item 5 Brachiaria
  x<-v37_feed_type[feed_type_code==5][, feed_type_name := "Brachiaria"]
  by_cols<-colnames(x)[1:5]
  num_cols<-colnames(x)[!colnames(x) %in% by_cols]

  x <- x[, lapply(.SD, mean), by = by_cols, .SDcols = num_cols]

  v37_feed_type<-rbind(v37_feed_type[feed_type_code!=5],x)

  # V37 - Merge feed_items & feed type
  v37_feed_items_merge<-merge(v37_feed_items,v37_feed_type,by="feed_type_code",all.x=T)

  # Feed basket
  v37_feed_basket <- data.table(readxl::read_excel(file,sheet="Feedproportions"))
  v37_feed_basket[,livetype_code:=tolower(livetype_code)]

  # Remove duplicates
  v37_feed_basket<-unique(v37_feed_basket)

  v37_feed_basket<-melt(v37_feed_basket,
                        id.vars = c("Ids","v37_livestock_type","livetype_code","livetype_desc"),
                        variable.name = "feed_item_code",
                        value.name = "allocation")

  # Check proportions sum to 1
  v37_feed_basket<-v37_feed_basket[allocation!=0][order(Ids,livetype_code)
                                                  ][,total:=sum(allocation),by=.(Ids,livetype_code,livetype_desc)]

  unique(v37_feed_basket[total==2,.(Ids,livetype_code,total)])

  v37_feed_basket<-v37_feed_basket[,.(Ids,livetype_code,livetype_desc,feed_item_code,allocation)
                                   ][,allocation:=allocation*100
                                     ][,feed_item_code:=as.numeric(as.character(feed_item_code))]

  # Add feed type code
  v37_feed_basket<-merge(v37_feed_basket,v37_feed_items[,.(feed_item_code,feed_type_code)],by="feed_item_code",all.x=T)
  v37_feed_basket[is.na(feed_type_code)]
  v37_feed_basket[,feed_item_code:=as.character(feed_item_code)]
  v37_feed_basket[,feed_type_code:=as.character(feed_type_code)]

  # Simple field parameters
  v37_simple_fields<-data.table(readxl::read_excel(file,sheet="simple_fields"))

  ## 1.2) Read in parameter tables #####
  lkp_livetype<-fread("https://raw.githubusercontent.com/CIAT/icleaned/refs/heads/staging/data/primary_database/lkp_livetype.csv")
  lkp_manureman<-fread("https://raw.githubusercontent.com/CIAT/icleaned/refs/heads/staging/data/primary_database/lkp_manureman.csv")

  energy_parameters <- fromJSON(
    system.file("extdata", "energy_parameters.json", package = "cleaned"),
    flatten = TRUE
  )

  # Read static parameters directory and files
  ghg_ipcc_data <- fromJSON(
    system.file("extdata", "ghg_parameters.json", package = "cleaned"),
    flatten = TRUE
  )

  ## 1.3) Input template #####
  file<-"https://raw.githubusercontent.com/CIAT/icleaned/refs/heads/staging/data/shared_folder/study_objects/Study_2.json"
  template <- fromJSON(file)
  colnames(template$livestock)

  feed_items<-data.table(template$feed_items)
  fertilizer<-data.table(template$fertilizer)[0,]
  fertilizer<-NULL

# 2) Set "fixed" parameters ####

  ## 2.0) "Simple" fields #####
  template_names<-data.table(field_name=names(template),
                         class=sapply(template,base::class),
                         length=sapply(template,length))

  simple_field_names<-template_names[length==1,field_name]

  simple_fields<-template[simple_field_names]

  for(i in 1:length(simple_fields)){
    field<-names(simple_fields)[i]
    simple_fields[field]<-v37_simple_fields[field_name==field,value]
  }


  ## 2.1) Livestock #####
  mm_code<-"storage"
  mm_des<-lkp_manureman[manureman_code==mm_code,manureman_desc]

  mm_code2<-"pasture"
  mm_des2<-lkp_manureman[manureman_code==mm_code,manureman_desc]

  livestock_fixed<-data.frame(
    manureman_stable=mm_des,
    manureman_onfarm_grazing=mm_des2,
    manureman_non_roofed_enclosure=0,
    manureman_offfarm_grazing=mm_des2,
    annual_growth=0,
    annual_wool=0,
    manure_in_stable=1,
    manure_in_non_roofed_enclosure=0,
    manure_in_field=0,
    manure_onfarm_fraction=0,
    manure_sales_fraction=0,
    body_weight_weaning=0,
    body_weight_year_one=0,
    adult_weight=600,
    work_hour=0,
    piglets_relying_on_milk=0
  )

  ## 2.2) Feed_Items #####
  feed_fixed<-c(
    slope=1,
    slope_desc="Flat (0-5%)",
    slope_p_factor=0.11,
    slope_length=15,
    water_regime=0,
    n_content=0,
    cultivation_period=0,
    ecosystem_type=0,
    organic_amendment=0,
    cut_carry_fraction=0,
    fraction_as_fertilizer=0, # Needs to be distributed by crop
    fraction_as_manure=0, # Needs to be distributed by crop
    n_fertilizer=0,
    urea=0,
    npk=0,
    dap=0,
    ammonium_nitrate=0,
    ammonium_sulfate=0,
    n_solutions=0,
    ammonia=0,
    time_horizon=0
  )

  fi_cols<-colnames(feed_items)
  zero_cols<-fi_cols[grep("trees|dbh|diameter_",fi_cols)]
  names(zero_cols)<-zero_cols
  zero_cols[1:length(zero_cols)]<-0
  feed_fixed<-c(feed_fixed,zero_cols)

    ### 2.2.1)  Add fixed items to input template #####
    for(k in 1:length(feed_fixed)){
      variable<-names(feed_fixed)[k]
      v37_feed_items_merge[,(variable):=feed_fixed[k]]
    }

    # Check all cols in template are present
    colnames(feed_items)[!colnames(feed_items) %in% colnames(v37_feed_items_merge)]

    # Enforce numeric class
    num_cols<-c("slope_p_factor","slope_length")
    v37_feed_items_merge[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

  ## 2.3) Seasons #####
  season_dat<-data.frame(season_length=180,season_name="season_x")

# 3) Loop through farms ####
# unique farm ids
farms<-na.omit(unique(herd$Ids))
farms<-farms[2]

farm_paras<-lapply(1:length(farms),FUN=function(i){
  farm<-farms[i]

  ## 3.1) Herds #####
  # Make a blank herd template to populate
  livestock<-data.table(template$livestock[1,])
  livestock[1,1:ncol(livestock)]<-NA

  # Subset v37 data to selected farm
  livestock_37<-herd[Ids==farm & !grepl("calve",livetype_desc_v37,ignore.case = T)]

  # Loop through herds on the farm
  paras<-lapply(1:nrow(livestock_37),FUN=function(j){
    cat("\r", farm, "i =", i, "/", length(farms), "herd (j) =", j, "/", nrow(livestock_37))

    # Subset herds
    herd_1<-livestock_37[j]

    # Merge lkp_livetype
    l_code<-herd_1$livetype_code
    livestock$livetype_code<-l_code
    livestock$livetype_code<-as.integer(livestock$livetype_code)

    mergedat<-lkp_livetype[livetype_code==l_code]

    cols_m<-colnames(mergedat)
    cols_m<-cols_m[cols_m != "livetype_code"]
    cols_l<-c(colnames(livestock)[!colnames(livestock) %in% cols_m])
    livestock<-livestock[,cols_l,with=F]

    livestock<-merge(livestock,mergedat,by="livetype_code",all.x = T)

    # Insert farm specific V37 herd data
    cols_m<-colnames(herd_1)
    livestock$livetype_desc_v37<-herd_1$livetype_desc_v37
    livestock$herd_composition<-herd_1$number
    livestock$body_weight<-herd_1$body_weight
    livestock$annual_milk<-herd_1$annual_milk
    livestock$time_in_stable<-herd_1$time_in_stable
    livestock$time_in_onfarm_grazing<-herd_1$time_in_onfarm_grazing
    livestock$time_in_offfarm_grazing<-herd_1$time_in_offfarm_grazing
    livestock$time_in_non_roofed_enclosure<-herd_1$time_in_non_roofed_enclosure
    livestock$distance_to_pasture<-herd_1$distance_to_pasture

    # Add fixed parameters
    for(k in 1:ncol(livestock_fixed)){
      variable<-colnames(livestock_fixed)[k]
      livestock[,(variable):=livestock_fixed[1,k]]
    }

    # Rename ipcc parameter columns
    ipcc_new<-c(
    "ipcc_ef_category_t1",
    "ipcc_ef_category_t2",
    "ipcc_meth_man_category",
    "ipcc_n_exc_category"
    )

    ipcc_old<-c(
    "ipcc_meth_ef_t1",
    "ipcc_meth_ef_t2",
    "ipcc_meth_man",
    "ipcc_meth_exc"
    )

    livestock<-livestock[,!ipcc_new,with = F]
    setnames(livestock,ipcc_old,ipcc_new)

  # Enforce codes to be character
  livestock[,livetype_code:=as.character(livetype_code)]

  ## 3.2) Fertilizer #####
    # Blank table created in section 1.3
  # 3.3) Feed basket ####
    ### 3.3.1) Seasons #####
     # Created in section 2.3
    ### 3.3.2) Feeds #####
      feeds<-v37_feed_basket[Ids==farm & tolower(livetype_desc)==tolower(livestock$livetype_desc_v37)]
      if(nrow(feeds)==0){
        stop("Feed table is blank, livetype_desc in feed basket not matching herd.")
      }

      # Average across duplicate rows
      #feeds<-feeds[,.(allocation=mean(allocation)),by=.(Ids,feed_type_code,feed_item_code,livetype_code)]

      feeds<-feeds[,.(livestock=list(data.table(livetype_code=livetype_code,allocation=allocation))),by=.(feed_item_code,feed_type_code)]
      feeds<-data.frame(feeds)
    ### 3.3.3) Combine Season and Feeds #####
      feed_basket <- data.table(
        feeds = list(feeds=feeds),
        season_name = season_dat$season_name[1]
      )
      feed_basket<-data.frame(feed_basket)

  ## 3.4) Feed Items #####
  # v37_feed_items_merge is creted in section 2.2, subset to items in the diet
  feed_items<-v37_feed_items_merge[feed_item_code %in% feeds$feed_item_code]
  ## 3.5) Join sections into list structure ####
  input_object<-c(simple_fields,list(livestock=livestock,
                                     feed_items=feed_items,
                                     fertilizer=fertilizer,
                                     seasons=season_dat,
                                     feed_basket=feed_basket))
  input_object$farm_name<-farm
  return(input_object)
  })
  names(paras)<-paste0("herd-",1:nrow(livestock_37))

  return(paras)
})

names(farm_paras)<-paste0("f",farms)


# 4) Pass to cleaned functions ####
messages<-F

ghg_emissions<-lapply(1:length(farm_paras),FUN=function(i){

  farm_para<-farm_paras[[i]]

  result<-lapply(1:length(farm_para),FUN=function(j){

    cat("farm",names(farm_paras)[i],i,"/",length(farm_paras),"herd",j,"      \r")

    para<-farm_para[[j]]

    ## 4.1) feed basket quality #####
    feed_basket_quality <- feed_quality(para)

    ## 4.2) energy #####
    # Table 10.4 is in list form this throws an error, convert to data.frame
    energy_required <- energy_requirement(para,feed_basket_quality,energy_parameters = energy_parameters)

    ## 4.3) land #####
    para$feed_items$intercrop<-as.numeric(para$feed_items$intercrop)
    land_required <- land_requirement(feed_basket_quality, energy_required, para)

    ## 4.4) soil erosion #####
    para$feed_items$slope_p_factor<-as.numeric(para$feed_items$slope_p_factor)
    soil_erosion <- soil_health(para, land_required)

    ## 4.5) n balance #####
    para$feed_items$ammonia<-as.numeric(para$feed_items$ammonia)
    para$feed_items$ammonium_nitrate<-as.numeric(para$feed_items$ammonium_nitrate)
    para$feed_items$ammonium_sulfate<-as.numeric(para$feed_items$ammonium_sulfate)
    para$feed_items$dap<-as.numeric(para$feed_items$dap)
    para$feed_items$n_solutions<-as.numeric(para$feed_items$n_solutions)
    para$feed_items$npk<-as.numeric(para$feed_items$npk)
    para$feed_items$urea<-as.numeric(para$feed_items$urea)

    nitrogen_balance <- n_balance(para, land_required, soil_erosion,energy_required)

    ## 4.6) livestock productivity #####
    livestock_productivity <- land_productivity(para,energy_required)

    ## 4.7) biomass #####
    num_cols<-c("trees_ha_dbh25","trees_ha_dbh2550","trees_ha_dbh50","increase_dbh25","increase_dbh2550","increase_dbh50",
                "time_horizon","average_dbh2550","average_dbh25","average_dbh50")
    para$feed_items<-para$feed_items[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

    biomass <- biomass_calculation(para, land_required)

    ## 4.8) ghg_emissions #####
    char_cols<-c("manureman_non_roofed_enclosure")
    para$livestock<-para$livestock[, (char_cols) := lapply(.SD, as.character), .SDcols = char_cols]

    if(messages==F){
      results<-suppressMessages(ghg_emission(para,energy_required,ghg_ipcc_data,land_required,nitrogen_balance,feed_basket_quality,ym_prod=F))
    }else{
      results<-ghg_emission(para,energy_required,ghg_ipcc_data,land_required,nitrogen_balance,feed_basket_quality,ym_prod=F)
    }

    # Unpack soil into 3 tables not a list
    results$soil_annual_N20N_soil_direct_emission<-results$ghg_soil$annual_N20N_soil_direct_emission
    results$soil_annual_N20N_soil_indirect_emission<-results$ghg_soil$annual_N20N_soil_indirect_emission
    results$soil_N20N_off_farm<-results$ghg_soil$N20N_off_farm
    results$ghg_soil<-NULL
    results
  })
  names(result)<-names(farm_para)
  return(result)
})

names(ghg_emissions)<-names(farm_paras)

## 4.9) Add farm and herd name to tables
ghg_emissions_merge<-lapply(1:length(ghg_emissions),FUN=function(i){
  farm_name<-names(ghg_emissions)[i]
  ghg_farm<-ghg_emissions[[i]]

  ghg_farm_updated<-lapply(1:length(ghg_farm),FUN=function(j){
    cat("\r","i",i,"j",j,"      ")
    herd_name<-names(ghg_farm)[j]
    ghg_herd<-ghg_farm[[j]]
    ghg_herd_updated<-lapply(1:length(ghg_herd),FUN=function(k){
      data<-ghg_herd[[k]]
      data$farm<-farm_name
      data$herd<-herd_name
      return(data)
    })
    names(ghg_herd_updated)<-names(ghg_herd)
    return(ghg_herd_updated)
  })

  tab_names<-names(ghg_farm_updated[[1]])
  ghg_farm_merged<-lapply(tab_names,FUN=function(tab_name){
    x<-lapply(ghg_farm_updated,"[[",tab_name)
    x<-rbindlist(x)
    x
  })

  names(ghg_farm_merged)<-tab_names
  return(ghg_farm_merged)
})
names(ghg_emissions_merge)<-names(ghg_emissions)

## 4.10) Combine tables #####
tab_names<-names(ghg_emissions_merge[[1]])

ghg_emissions_merge_all<-lapply(tab_names,FUN=function(tab_name){
  x<-lapply(ghg_emissions_merge,"[[",tab_name)
  x<-rbindlist(x)
  x
})
names(ghg_emissions_merge_all)<-tab_names

n<-nchar(names(ghg_emissions_merge_all))
names(ghg_emissions_merge_all)[n>31]
names(ghg_emissions_merge_all)[n>31]<-c("annual_N20N_soil_direct","annual_N20N_soil_indirect")

ghg_emissions_merge_all$ef[farm=="f596171010",.(farm,livetype_desc_v37,ipcc_ef_category_t1,ipcc_ef_category_t2,de,de_intake,ge_intake,dmi_tot,ym,enteric_methane_emissions)]

# Create a new Excel workbook
wb <- createWorkbook()

# Loop through the list and add each data.frame as a sheet
for (sheet_name in names(ghg_emissions_merge_all)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x = ghg_emissions_merge_all[[sheet_name]])
}

# Save the workbook to a file
saveWorkbook(wb, file = save_file, overwrite = TRUE)

print("Excel file saved as 'output.xlsx'")
