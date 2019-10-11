################################land use change module##############################

#CLEANED version 2
#Code by Catherine Pfeifer, c.pfeifer@cgiar.org


#this code allows to create a crop change layer, i.e. a layer that indicated which cells in the landscape
#will be converted to cropland based on a land use change model 

#this code results in the  "addcrop"layer, which can be requested from 0-userdefinition to activate a 
# land use change based scenario 
# 
# #set path to cleaned tool
# path<-'D:/Dropbox/Cleaned Tanzania'
# #clearing all memory 
# setwd(path)
# # read libraries
# library(raster)
# library(maptools)
# library(RColorBrewer)
# setwd(path)
# 
# 
# #read parameter
# para<-read.csv('1-input/parameter/para.csv')
# para$value<-as.numeric(para$value)
# dim<-dim(para)
# obs<-dim [1]
# 
# for(i in seq_along(para$name))
# {assign(as.character(para$name[i]), para$value[i])}
# source('3-cleaned/2-load_data.r')
# 
# source('3-cleaned/2-feedbasket_nonlinear2.r')
# 

#set the persentage of additional land required 
setwd(path)
add = add/100
ratio = 2

#set here the yield objective in tons Tanzania, 
#tot biomass
biomass<- fw_rc+fw_rl+fw_pf+ fwg_sil # in kg
objyield= biomass * add
start2<-Sys.time()
while(ratio > 1.005| ratio< 0.995 ) {
    #######################computing the ranking of cells ######################

    fun<-function(x,y){x*y}
    dist_suit_c<-overlay(disttocrop2,suit_c,fun=fun)
    dist_suit_c[cropland==1]<-NA
    dist_suit_c[]<-rank(dist_suit_c[],ties.method="random",na.last='keep')


    ncellc <-cellStats(cropland, stat='sum', na.rm=TRUE)

    tres<-add * ncellc
    m<-c(0, tres,1,tres+0.0000001,maxValue(dist_suit_c),0)
    rcl<-matrix(m,ncol=3,byrow=T)
    addcrop<-reclassify(dist_suit_c,rcl=rcl)
    addcrop[is.na(addcrop)]<-0
    
    # name<-paste(add,'addcrop_area.tif',sep='_')
    # name<-paste(path,'1-input/spatial/landuse_scenario',sep='/')
    # writeRaster(addcrop,name,overwrite=T)

    #compute the yield on the new cell 
    #calculate the production on the new land 
    fun=function(w,x,y){w*(ar_rc/(ar_rc+ar_rl+ar_pf)*x*rfm+ar_rl/(ar_rc+ar_rl+ar_pf)*y*rfl+fy_pf*1000/10000*pixel[[1]]^2*rfpf*ar_pf/(ar_rc+ar_rl+ar_pf))}
    yield<-overlay(addcrop,y_maize,y_pulse,fun=fun)

    addyield <-cellStats(yield, stat='sum', na.rm=TRUE)
    addyield

   
    ratio=objyield/addyield
 
    print(ratio) 
    add<-add*(ratio)
    print(add)} #end of while loop 




end2<-Sys.time()

end2-start2
    
