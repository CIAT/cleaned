pacman::p_load(data.table,readxl,cleaned,jsonlite)

# 1) Load Data ####
## 1.1) V37 data #####
file<-"data/v37/Kenya NPA data.xlsx"
data <- readxl::read_excel(file,sheet=1)

data <- readxl::read_excel(file,sheet=1)
ls_param <- data.table(readxl::read_excel(file,sheet="Livestockparameters"))
herd <- data.table(readxl::read_excel(file,sheet="milk-bodyweight"))

v37_feed_items<-data.table(readxl::read_excel(file,sheet="Feed_items"))
v37_feed_type<-unique(data.table(readxl::read_excel(file,sheet="feed_type")))[order(feed_type_code)]

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
v37_feed_basket<-v37_feed_basket[allocation!=0][order(Ids,livetype_code)][,total:=sum(allocation),by=.(Ids,livetype_code)]
unique(v37_feed_basket[total==2,.(Ids,livetype_code,total)])

v37_feed_basket<-v37_feed_basket[,.(Ids,livetype_code,feed_item_code,allocation)
                                 ][,allocation:=allocation*100
                                   ][,feed_item_code:=as.numeric(as.character(feed_item_code))]

# Add feed type code
v37_feed_basket<-merge(v37_feed_basket,v37_feed_items[,.(feed_item_code,feed_type_code)],by="feed_item_code",all.x=T)
v37_feed_basket[is.na(feed_type_code)]

## 1.2) Read in parameter tables #####
lkp_livetype<-fread("https://raw.githubusercontent.com/CIAT/icleaned/refs/heads/staging/data/primary_database/lkp_livetype.csv")
lkp_manureman<-fread("https://raw.githubusercontent.com/CIAT/icleaned/refs/heads/staging/data/primary_database/lkp_manureman.csv")

## 1.3) Input template #####
file<-"https://raw.githubusercontent.com/CIAT/icleaned/refs/heads/staging/data/shared_folder/study_objects/Study_2.json"
template <- fromJSON(file)
colnames(template$livestock)


feed_items<-data.table(template$feed_items)
fertilizer<-data.table(template$fertilizer)[0,]

# 2) Set "fixed" parameters ####

## 2.0) "Simple" fields #####
template_names<-data.table(field_name=names(template),
                       class=sapply(template,base::class),
                       length=sapply(template,length))

simple_fields<-template_names[length==1,field_name]

system(paste("echo", shQuote(paste(simple_fields, collapse = "\t")), "| pbcopy"))
## 2.1) Livestock #####
mm_code<-"storage"
mm_des<-lkp_manureman[manureman_code==mm_code,manureman_desc]

mm_code2<-"pasture"
mm_des2<-lkp_manureman[manureman_code==mm_code,manureman_desc]

livestock_fixed<-c(
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
## 2.3) Seasons #####
  seasons<-data.frame(season_length="season_name",season_length=180)

# 3) Loop throught farms ####
# unique farm ids
farms<-na.omit(unique(herd$Ids))

i<-1

for(i in 1:length(farms)){
  farm<-farms[i]

  ## 3.1) Herds #####
  # Make a blank herd template to populate
  livestock<-data.table(template$livestock[1,])
  livestock[1,1:ncol(livestock)]<-NA

  # Subset v37 data to selected farm
  livestock_37<-herd[Ids==farm]

  # Loop through herds on the farm
  for(j in 1:nrow(livestock_37)){
  cat(farm,i,"/",length(farms),herd,j,"/",nrow(livestock_37))

  # Subset herds
  herd_1<-livestock_37[j]

  # Merge lkp_livetype
  l_code<-herd_1$livetype_code
  livestock$livetype_code[j]<-l_code
  livestock$livetype_code<-as.integer(livestock$livetype_code)

  mergedat<-lkp_livetype[livetype_code==l_code]

  cols_m<-colnames(mergedat)
  cols_m<-cols_m[cols_m != "livetype_code"]
  cols_l<-colnames(livestock)[!colnames(livestock) %in% cols_m]
  livestock<-livestock[,cols_l,with=F]

  livestock<-merge(livestock,mergedat,by="livetype_code",all.x = T)

  # Insert farm specific V37 herd data
  cols_m<-colnames(herd_1)
  livestock$herd_composition<-herd_1$number
  livestock$body_weight<-herd_1$body_weight
  livestock$annual_milk<-herd_1$annual_milk
  livestock$time_in_stable<-herd_1$time_in_stable
  livestock$time_in_onfarm_grazing<-herd_1$time_in_onfarm_grazing
  livestock$time_in_offfarm_grazing<-herd_1$time_in_offfarm_grazing
  livestock$time_in_non_roofed_enclosure<-herd_1$time_in_non_roofed_enclosure
  livestock$distance_to_pasture<-herd_1$distance_to_pasture

  # Add fixed parameters
  for(k in 1:length(livestock_fixed)){
    variable<-names(livestock_fixed)[k]
    livestock[,(variable):=livestock_fixed[k]]
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

  ## 3.2) Feed Items ####
    # Already created in section 2.2
  ## 3.3 Fertilizer #####
    # Blank table created in section 1.3

  # 4) Feed basket ####


  ## 4.1) Seasons #####
  # Created in section 2.3

  ## 4.2) Feeds #####

    feed_basket<-v37_feed_basket[Ids==farm]
    feed_basket<-feed_basket[,.(livestock=list(data.frame(livetype_code=livetype_code,allocation=allocation))),by=.(feed_item_code,feed_type_code)]

  ## 4.3) Combine Season and Feeds #####
    feed_basket<-data.frame(feeds=feed_basket,seasons=seasons)

  # 5) Join sections into list structure ####


  }
}
