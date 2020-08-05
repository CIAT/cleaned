# Load required packages
.packages = c("jsonlite","tidyverse", "lubridate", "data.table")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# Working directory
path <- "."

# load functions
source("./cleanedFunctions.R")

# Loading data
para <- fromJSON(paste0(path,"/data/example.json"),  flatten = TRUE)

# Feed quality calculation
feed_basket_quality <- feed_quality(para)

# Energy requirement
energy_required <- energy_requirement(para,feed_basket_quality)
