############################################computation resulting from land use change#########
# cbush<-raster('1-input/spatial/lu/cbush.tif')
# cgrass<-raster('1-input/spatial/lu/cgrass.tif')
# cropland<-raster('1-input/spatial/lu/cropland.tif')
# dforest<-raster('1-input/spatial/lu/dforest.tif')
# grazland<-raster('1-input/spatial/lu/grazland.tif')
# maforest<-raster('1-input/spatial/lu/maforest.tif')
# mforest<-raster('1-input/spatial/lu/mforest.tif')
# obush<-raster('1-input/spatial/lu/obush.tif')
# ograss<-raster('1-input/spatial/lu/ograss.tif')
# pcrop<-raster('1-input/spatial/lu/pcrop.tif')
# pforest<-raster('1-input/spatial/lu/pforest.tif')
# sforest<-raster('1-input/spatial/lu/sforest.tif')
# wetland<-raster('1-input/spatial/lu/wetland.tif')
# otherland<-raster('1-input/spatial/lu/otherland.tif')
# waterbody<-raster('1-input/spatial/lu/waterbody.tif')
# 


################################################################################################
#extracting data for scenarios 
# here we will need to adjust the reclassification depending on the input layer 
setwd(path)
fun=function(x,y){x+y}
cropland<-overlay(cropland,addcrop,fun=fun)
rcl2<- cbind(c(0,1,2 ),
             c(0,1,1))
cropland <- reclassify(cropland, rcl=rcl2)

rcl10<- cbind(c(0,1 ),
             c(0,2))
addcrop<-reclassify(addcrop, rcl=rcl10)


fun2=function(x,y){(x-y)}
temp<-overlay(grazland,addcrop,fun=fun2)
rcl3<- cbind(c(-2,-1,0,1 ),
             c(0,0,0,1))
grazland <- reclassify(temp, rcl=rcl3)
rcl4<- cbind(c(-2,-1,0,1 ),
             c(0,1,0,0))
lostgraz=reclassify(temp, rcl=rcl4)


# extracting dense forest
temp<-overlay(dforest,addcrop,fun=fun2)
dforest <- reclassify(temp, rcl=rcl3)
lostdforest <- reclassify(temp, rcl=rcl4)

temp<-overlay(mforest,addcrop,fun=fun2)
mforest <- reclassify(temp, rcl=rcl3)
lostmforest <- reclassify(temp, rcl=rcl4)

temp<-overlay(sforest,addcrop,fun=fun2)
sforest <- reclassify(temp, rcl=rcl3)
lostsforest <- reclassify(temp, rcl=rcl4)

temp<-overlay(pforest,addcrop,fun=fun2)
pforest <- reclassify(temp, rcl=rcl3)
lostpforest <- reclassify(temp, rcl=rcl4)

temp<-overlay(maforest,addcrop,fun=fun2)
maforest <- reclassify(temp, rcl=rcl3)
lostmaforest <- reclassify(temp, rcl=rcl4)

temp<-overlay(cgrass,addcrop,fun=fun2)
cgrass <- reclassify(cgrass, rcl=rcl3)
lostcgrass <- reclassify(temp, rcl=rcl4)


temp<-overlay(ograss,addcrop,fun=fun2)
ograss <- reclassify(temp, rcl=rcl3)
lostograss <- reclassify(temp, rcl=rcl4)

temp<-overlay(cbush,addcrop,fun=fun2)
cbush <- reclassify(temp, rcl=rcl3)
lostcbush <- reclassify(temp, rcl=rcl4)

temp<-overlay(obush,addcrop,fun=fun2)
obush <- reclassify(temp, rcl=rcl3)
lostobush <- reclassify(temp, rcl=rcl4)

temp<-overlay(pcrop,addcrop,fun=fun2)
pcrop <- reclassify(temp, rcl=rcl3)
lostpcrop <- reclassify(temp, rcl=rcl4)

temp<-overlay(wetland,addcrop,fun=fun2)
wetland <- reclassify(temp, rcl=rcl3)
lostwetland <- reclassify(temp, rcl=rcl4)

temp<-overlay(waterbody,addcrop,fun=fun2)
waterbody <- reclassify(temp, rcl=rcl3)
lostwaterbody <- reclassify(temp, rcl=rcl4)

fun=function(a,b,c,d,e){a+b+c+d+e}
lostforest<-overlay(lostdforest,lostmforest,lostsforest,lostpforest,maforest,fun=fun)

