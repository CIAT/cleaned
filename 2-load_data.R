# this code loads all the data into the memory 
# it needs to be run before the feed basket 

# read libraries
library(raster)
library(maptools)
library(RColorBrewer)

setwd(path)

#read parameter
para<-read.csv('1-input/parameter/para.csv')
para$value<-as.numeric(para$value)

dim<-dim(para)
obs<-dim [1]

for(i in seq_along(para$name))
{
  assign(as.character(para$name[i]), para$value[i])
}



#read necessary spatial data
#from water
sarea<-readShapePoly('1-input/spatial/Lushoto_nnp.shp')
#proj4string(sarea)<-' +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 '
rain<-raster('1-input/spatial/rain.tif')
y_maize<-raster('1-input/spatial/y_maize.tif')
y_pulse<-raster('1-input/spatial/y_pulse.tif')
y_rice<-raster('1-input/spatial/y_rice.tif')
ET_gras<-raster('1-input/spatial/ET_gras.tif')
ET_grasl<-raster('1-input/spatial/ET_grasl.tif')
ET_maize<-raster('1-input/spatial/ET_maize.tif')
ET_pulse<-raster('1-input/spatial/ET_pulse.tif')
ET_rice<-raster('1-input/spatial/ET_pulse.tif')

cropland<-raster('1-input/spatial/lu/cropland.tif')
grazland<-raster('1-input/spatial/lu/grazland.tif')
obush<-raster('1-input/spatial/lu/obush.tif')
cbush<-raster('1-input/spatial/lu/cbush.tif')
grazland<-grazland+obush+cbush

#riceland<-raster('1-input/spatial/lu/riz.tif')
# from soil 
lu<-raster('1-input/spatial/lu/lu.tif')
rr<-raster('1-input/spatial/rr.tif')
SN2<-raster('1-input/spatial/soil/SN2.tif')
SN1<-raster('1-input/spatial/soil/SN1.tif')
BD1<-raster('1-input/spatial/soil/BD1.tif')
BD2<-raster('1-input/spatial/soil/BD2.tif')
BD3<-raster('1-input/spatial/soil/BD3.tif')
CL1<-raster('1-input/spatial/soil/CL1.tif')
CL2<-raster('1-input/spatial/soil/CL2.tif')
CL3<-raster('1-input/spatial/soil/CL3.tif')
Nsoil<-raster('1-input/spatial/soil/Nsoil.tif')
CLavg<-raster('1-input/spatial/soil/CLavg.tif')
#BDavg<-raster('1-input/spatial/soil/BDavg.tif')
E<-raster('1-input/spatial/soil/E.tif')

pixel<-read.csv('1-input/parameter/pixel.csv')
pixel<-pixel$x
A<-raster('1-input/spatial/soil/A.tif')

# the percentage computation are now in the feedbasket 
ncellcrop<-cellStats(cropland,stat='sum')
ncellgraz<-cellStats(grazland,stat='sum') 
#ncellrice<-cellStats(riceland,stat='sum') 
# from ghg pathway
#read spatial layer
tempr<-raster('1-input/spatial/tempr.tif')
#livdist is here a layer of 1 
livdist <-raster('1-input/spatial/dum.tif')
#climate<-raster('1-input/spatial/climate.tif')

soil<-raster('1-input/spatial/soil.tif')
soilref<-raster('1-input/spatial/soilref.tif')
Flu_c<-raster('1-input/spatial/Flu_c.tif')
Flu_pc<-raster('1-input/spatial/Flu_pc.tif')
Flu_rice<-raster('1-input/spatial/Flu_rice.tif')
Flu_sa<-raster('1-input/spatial/Flu_sa.tif')
clim_wtmoist<-raster('1-input/spatial/ipcc_climate/clim_wtmoist.tif')
clim_wtdry<-raster('1-input/spatial/ipcc_climate/clim_wtdry.tif')
clim_ctmoist<-raster('1-input/spatial/ipcc_climate/clim_ctmoist.tif')
clim_ctdry<-raster('1-input/spatial/ipcc_climate/clim_ctdry.tif')
clim_tr_mont<-raster('1-input/spatial/ipcc_climate/clim_tr_mont.tif')
clim_tr_wet<-raster('1-input/spatial/ipcc_climate/clim_tr_wet.tif')
clim_tr_moist<-raster('1-input/spatial/ipcc_climate/clim_tr_moist.tif')
clim_tr_dry<-raster('1-input/spatial/ipcc_climate/clim_tr_dry.tif')
graz_clim<-raster('1-input/spatial/graz_clim.tif')
forest_clim<-raster('1-input/spatial/forest_clim.tif')
y_maize<-raster('1-input/spatial/y_maize.tif')
y_pulse<-raster('1-input/spatial/y_pulse.tif')


dforest<-raster('1-input/spatial/lu/dforest.tif')
mforest<-raster('1-input/spatial/lu/mforest.tif')
sforest<-raster('1-input/spatial/lu/sforest.tif')
pforest<-raster('1-input/spatial/lu/pforest.tif')
maforest<-raster('1-input/spatial/lu/maforest.tif')
cgrass<-raster('1-input/spatial/lu/cgrass.tif')
ograss<-raster('1-input/spatial/lu/ograss.tif')
cbush<-raster('1-input/spatial/lu/cbush.tif')
obush<-raster('1-input/spatial/lu/obush.tif')
pcrop<-raster('1-input/spatial/lu/pcrop.tif')
wetland<-raster('1-input/spatial/lu/wetland.tif')
waterbody<-raster('1-input/spatial/lu/waterbody.tif')
otherland<-raster('1-input/spatial/lu/otherland.tif')


mms_lagoon_mcf <- raster('1-input/spatial/mms_lagoon_mcf.tif')
mms_liquidslurry_mcf <- raster('1-input/spatial/mms_liquidslurry_mcf.tif')
mms_solidstorage_mcf <- raster('1-input/spatial/mms_solidstorage_mcf.tif')
mms_drylot_mcf <- raster('1-input/spatial/mms_drylot_mcf.tif')
mms_pasture_mcf <- raster( '1-input/spatial/mms_pasture_mcf.tif')
mms_dailyspread_mcf <- raster( '1-input/spatial/mms_dailyspread_mcf.tif')
mms_digester_mcf <- raster( '1-input/spatial/mms_digester_mcf.tif')
mms_burned_mcf <- raster( '1-input/spatial/mms_burned_mcf.tif')
mms_other_mcf <- raster( '1-input/spatial/mms_other_mcf.tif')
disttocrop<-raster( '1-input/spatial/disttocrop.tif')
disttocrop2<-raster( '1-input/spatial/disttocrop2.tif')
suit_c<-raster( '1-input/spatial/suit_c.tif')
