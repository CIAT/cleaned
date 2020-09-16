# Load required packages
.packages = c("jsonlite","tidyverse", "lubridate", "data.table", "tidyr", "dplyr")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# Working directory
path <- "."

# load functions
source("./cleanedFunctions.R")

# Loading data
para <- fromJSON(paste0(path,"/data/example_JM_Test_1_1.json"),  flatten = TRUE)

# Feed quality calculation
feed_basket_quality <- feed_quality(para)

# Energy requirement
energy_required <- energy_requirement(para,feed_basket_quality)

# Land requirement
land_required <- land_requirement(feed_basket_quality, energy_required, para)

#soil health status
soil_erosion <- soil_health(para, land_required)
