pacman::p_load(data.table,readxl,cleaned,jsonlite)

# V27 raw data
file<-file.choose()
data <- readxl::read_excel(file,sheet=1)

data <- readxl::read_excel(file,sheet=1)
ls_param <- readxl::read_excel(file2,sheet="Livestockparameters")
herd <- data.table(readxl::read_excel(file2,sheet="milk-bodyweight"))
feed_prop <- readxl::read_excel(file2,sheet="Feedproportions")

v37_feed_items<-data.table(readxl::read_excel(file2,sheet="Feed_items"))[,1:6]
v37_feed_type<-unique(data.table(readxl::read_excel(file2,sheet="feed_type"))[,1:6])[order(feed_type_code)]

# V37 - Merge feed_items & feed type
v37_feed_items_merge<-merge(v37_feed_items,v37_feed_type,by="feed_type_code",all.x=T)

# Create input object
# Input mappings
file3<-file.choose()
i_map <- readxl::read_excel(file3,sheet=1)

# Sarah Data
file2<-file.choose()

# Read in parameter tables
lkp_livetype<-fread("https://raw.githubusercontent.com/CIAT/icleaned/refs/heads/staging/data/primary_database/lkp_livetype.csv")
lkp_manureman<-fread("https://raw.githubusercontent.com/CIAT/icleaned/refs/heads/staging/data/primary_database/lkp_manureman.csv")
lkp_manureman<-fread("https://raw.githubusercontent.com/CIAT/icleaned/refs/heads/staging/data/primary_database/lkp_manureman.csv")

# Read in json
file<-"https://raw.githubusercontent.com/CIAT/icleaned/refs/heads/staging/data/shared_folder/study_objects/Study_2.json"
template <- fromJSON(file)
colnames(template$livestock)

# Cleaned Template - Feed Items
feed_items<-template$feed_items


# Fixed Parameters

# Livestock
mm_code<-"storage"
mm_des<-lkp_manureman[manureman_code==mm_code,manureman_desc]

mm_code2<-"pasture"
mm_des2<-lkp_manureman[manureman_code==mm_code,manureman_desc]

annual_growth<-0
annual_wool<-0
manure_in_stable<-1
manure_in_non_roofed_enclosure<-0
manure_in_field<-0
manure_onfarm_fraction<-0
manure_sales_fraction<-0
body_weight_weaning<-0
body_weight_year_one<-0
adult_weight<-600
work_hour<-0
piglets_relying_on_milk<-0

# Feed_Items
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
  ammomium_sulfate=0,
  n_solutions=0,
  ammonia=0
)

fi_cols<-colnames(feed_items)

# Add these to fixed items
zero_cols<-fi_cols[grep("trees|dbh|diameter_",fi_cols)]




# select farm
farms<-na.omit(unique(herd$Ids))
i<-1

for(i in 1:length(farms)){
farm<-farms[i]

## 1) Herd ####
# Make a blank template to populate
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

# Manure
livestock$manureman_stable<-mm_des
livestock$manureman_onfarm_grazing<-mm_des2
livestock$manureman_non_roofed_enclosure<-0
livestock$manureman_offfarm_grazing<-mm_des2

# V37 herd data
cols_m<-colnames(herd_1)
livestock$herd_composition<-herd_1$number
livestock$body_weight<-herd_1$body_weight
livestock$annual_milk<-herd_1$annual_milk
livestock$time_in_stable<-herd_1$time_in_stable
livestock$time_in_onfarm_grazing<-herd_1$time_in_onfarm_grazing
livestock$time_in_offfarm_grazing<-herd_1$time_in_offfarm_grazing
livestock$time_in_non_roofed_enclosure<-herd_1$time_in_non_roofed_enclosure
livestock$distance_to_pasture<-herd_1$distance_to_pasture

# Set fixed parameters we have assumed
livestock$annual_growth<-annual_growth
livestock$annual_wool<-annual_wool
livestock$annual_growth<-annual_growth
livestock$annual_wool<-annual_wool
livestock$manure_in_stable<-manure_in_stable
livestock$manure_in_non_roofed_enclosure<-manure_in_non_roofed_enclosure
livestock$manure_in_field<- manure_in_field
livestock$manure_onfarm_fraction<-manure_onfarm_fraction
livestock$manure_sales_fraction<-manure_sales_fraction
livestock$body_weight_weaning<-body_weight_weaning
livestock$adult_weight<-adult_weight
livestock$work_hour<-work_hour
livestock$piglets_relying_on_milk<-piglets_relying_on_milk
livestock$body_weight_year_one<-body_weight_year_one

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

# 2) Feed Items ####
# The items fed come from the feedproprotions table in the v37 excel (cols)
# Sometimes these are split by rows of the herd
# We can just supply the big data

# 3) Fertilizer ####
# Blank
# 4) Seasons ####
# length =
# name =
# 5) Feed basket ####
#

}
}
