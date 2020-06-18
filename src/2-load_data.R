# this code loads all the data into the memory it needs to be run
# before the feed basket

# read libraries
library(pacman)
pacman::p_load(raster, maptools, RColorBrewer, rgdal)

# read parameter
para <- read.csv(paste0(path, "/1-input/parameter/para.csv"))
para$value <- as.numeric(para$value)

dim <- dim(para)
obs <- dim[1]

for (i in seq_along(para$name)) {
  assign(as.character(para$name[i]), para$value[i])
}

# read necessary spatial data
sarea <- readOGR(paste0(path, "/1-input/spatial/atsbi_sarea.shp"))

rain <- raster("1-input/spatial/rain.tif")
y_maize <- raster("1-input/spatial/y_maize.tif")
y_pulse <- raster("1-input/spatial/y_pulse.tif")
y_rice <- raster("1-input/spatial/y_rice.tif")
ET_gras <- raster("1-input/spatial/ET_gras.tif")
ET_grasl <- raster("1-input/spatial/ET_grasl.tif")
ET_maize <- raster("1-input/spatial/ET_maize.tif")
ET_pulse <- raster("1-input/spatial/ET_pulse.tif")
ET_rice <- raster("1-input/spatial/ET_pulse.tif")

# riceland<-raster('1-input/spatial/lu/riz.tif') from soil
lu <- raster("1-input/spatial/lu/lu.tif")
rr <- raster("1-input/spatial/rr.tif")
SN2 <- raster("1-input/spatial/soil/SN2.tif")
SN1 <- raster("1-input/spatial/soil/SN1.tif")
BD1 <- raster("1-input/spatial/soil/BD1.tif")
BD2 <- raster("1-input/spatial/soil/BD2.tif")
BD3 <- raster("1-input/spatial/soil/BD3.tif")
CL1 <- raster("1-input/spatial/soil/CL1.tif")
CL2 <- raster("1-input/spatial/soil/CL2.tif")
CL3 <- raster("1-input/spatial/soil/CL3.tif")
Nsoil <- raster("1-input/spatial/soil/Nsoil.tif")
CLavg <- raster("1-input/spatial/soil/CLavg.tif")
# BDavg<-raster('1-input/spatial/soil/BDavg.tif')
E <- raster("1-input/spatial/soil/E.tif")

pixel <- read.csv("1-input/parameter/pixel.csv")
pixel <- pixel$x[1]
A <- raster("1-input/spatial/soil/A.tif")


# ncellrice<-cellStats(riceland,stat='sum') from ghg pathway read
# spatial layer
tempr <- raster("1-input/spatial/tempr.tif")
# livdist is here a layer of 1
livdist <- raster("1-input/spatial/dum.tif")
# climate<-raster('1-input/spatial/climate.tif')

soil <- raster("1-input/spatial/soil.tif")
soilref <- raster("1-input/spatial/soilref.tif")
Flu_c <- raster("1-input/spatial/Flu_c.tif")
Flu_pc <- raster("1-input/spatial/Flu_pc.tif")
Flu_rice <- raster("1-input/spatial/Flu_rice.tif")
Flu_sa <- raster("1-input/spatial/Flu_sa.tif")
clim_wtmoist <- raster("1-input/spatial/ipcc_climate/clim_wtmoist.tif")
clim_wtdry <- raster("1-input/spatial/ipcc_climate/clim_wtdry.tif")
clim_ctmoist <- raster("1-input/spatial/ipcc_climate/clim_ctmoist.tif")
clim_ctdry <- raster("1-input/spatial/ipcc_climate/clim_ctdry.tif")
clim_tr_mont <- raster("1-input/spatial/ipcc_climate/clim_tr_mont.tif")
clim_tr_wet <- raster("1-input/spatial/ipcc_climate/clim_tr_wet.tif")
clim_tr_moist <- raster("1-input/spatial/ipcc_climate/clim_tr_moist.tif")
clim_tr_dry <- raster("1-input/spatial/ipcc_climate/clim_tr_dry.tif")
graz_clim <- raster("1-input/spatial/graz_clim.tif")
forest_clim <- raster("1-input/spatial/forest_clim.tif")
y_maize <- raster("1-input/spatial/y_maize.tif")
y_pulse <- raster("1-input/spatial/y_pulse.tif")


lother <- raster("1-input/spatial/lu/lother.tif")
l1 <- raster("1-input/spatial/lu/l1.tif")
l2 <- raster("1-input/spatial/lu/l2.tif")
l3 <- raster("1-input/spatial/lu/l3.tif")
l4 <- raster("1-input/spatial/lu/l4.tif")
l5 <- raster("1-input/spatial/lu/l5.tif")
l6 <- raster("1-input/spatial/lu/l6.tif")
l7 <- raster("1-input/spatial/lu/l7.tif")
l8 <- raster("1-input/spatial/lu/l8.tif")
l9 <- raster("1-input/spatial/lu/l9.tif")
l10 <- raster("1-input/spatial/lu/l10.tif")
l11 <- raster("1-input/spatial/lu/l11.tif")
l14 <- raster("1-input/spatial/lu/l14.tif")
l17 <- raster("1-input/spatial/lu/l17.tif")

cropland <- l10 + l6
grazland <- l14 + l17 + l5 + l3 + l4 + l11 + l7 + l8


# the percentage computation are now in the feedbasket
ncellcrop <- cellStats(cropland, stat = "sum")
ncellgraz <- cellStats(grazland, stat = "sum")
mms_lagoon_mcf <- raster("1-input/spatial/mms_lagoon_mcf.tif")
mms_liquidslurry_mcf <- raster("1-input/spatial/mms_liquidslurry_mcf.tif")
mms_solidstorage_mcf <- raster("1-input/spatial/mms_solidstorage_mcf.tif")
mms_drylot_mcf <- raster("1-input/spatial/mms_drylot_mcf.tif")
mms_pasture_mcf <- raster("1-input/spatial/mms_pasture_mcf.tif")
mms_dailyspread_mcf <- raster("1-input/spatial/mms_dailyspread_mcf.tif")
mms_digester_mcf <- raster("1-input/spatial/mms_digester_mcf.tif")
mms_burned_mcf <- raster("1-input/spatial/mms_burned_mcf.tif")
mms_other_mcf <- raster("1-input/spatial/mms_other_mcf.tif")

mms_lagoon_mcf2 <- raster("1-input/spatial/mms_lagoon_mcf2.tif")
mms_liquidslurry_mcf2 <- raster("1-input/spatial/mms_liquidslurry_mcf2.tif")
mms_solidstorage_mcf2 <- raster("1-input/spatial/mms_solidstorage_mcf2.tif")
mms_drylot_mcf2 <- raster("1-input/spatial/mms_drylot_mcf2.tif")
mms_pasture_mcf2 <- raster("1-input/spatial/mms_pasture_mcf2.tif")
mms_dailyspread_mcf2 <- raster("1-input/spatial/mms_dailyspread_mcf2.tif")
mms_digester_mcf2 <- raster("1-input/spatial/mms_digester_mcf2.tif")
mms_burned_mcf2 <- raster("1-input/spatial/mms_burned_mcf2.tif")
mms_other_mcf2 <- raster("1-input/spatial/mms_other_mcf2.tif")


mcf_sh <- raster("1-input/spatial/mcf_sh.tif")


# disttocrop<-raster( '1-input/spatial/disttocrop.tif')
# disttocrop2<-raster( '1-input/spatial/disttocrop2.tif')
# suit_c<-raster( '1-input/spatial/suit_c.tif')