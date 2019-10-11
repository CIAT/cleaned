#########################################biodiverity pathway##################################


#cleaned version 2


#this code computes the biodiversity pathway 



library(raster)
library(maptools)
library(RColorBrewer)

#read required data 

setwd(path)
#read parameter
tspecies<-read.csv('1-input/parameter/threatenedspecies.csv')
tspecies[is.na(tspecies)]<-0



#run the landuse change scenario if relevant


bio_base<-read.csv('1-input/base_run/bio_ind.csv', row.names = NULL)
#bio_base<-bio_base[ ,2:3 ]
bio_map<-raster('1-input/base_run/bio_map.tif')

############compute biodiversity index###########
tspecies["dm"]<-1
num<-sum(tspecies$dm)
ndforest<-sum(tspecies$dforest)
nmforest<-sum(tspecies$mforest)
nsforest<-sum(tspecies$sforest)
npforest<-sum(tspecies$pforest)
nmaforest<-sum(tspecies$maforest)
ncgrass<-sum(tspecies$cgrass)
nograss<-sum(tspecies$ograss)
ncbush<-sum(tspecies$cbush)
nobush<-sum(tspecies$obush)
nacrop<-sum(tspecies$acrop)
npcrop<-sum(tspecies$pcrop)
nwetland<-sum(tspecies$wetland)
nwaterbody<-sum(tspecies$waterbody)


fun=function (x) {ndforest*x/num}
bi_dforest<-calc(dforest,fun=fun)

fun=function (x) {nmforest*x/num}
bi_mforest<-calc(mforest,fun=fun)

fun=function (x) {nsforest*x/num}
bi_sforest<-calc(sforest,fun=fun)

fun=function (x) {npforest*x/num}
bi_pforest<-calc(pforest,fun=fun)

fun=function (x) {nmaforest*x/num}
bi_maforest<-calc(maforest,fun=fun)

fun=function (x) {nograss*x/num}
bi_ograss<-calc(ograss,fun=fun)

fun=function (x) {ncgrass*x/num}
bi_cgrass<-calc(cgrass,fun=fun)

fun=function (x) {nobush*x/num}
bi_obush<-calc(obush,fun=fun)

fun=function (x) {ncbush*x/num}
bi_cbush<-calc(cbush,fun=fun)

fun=function (x) {nacrop*x/num}
bi_acrop<-calc(cropland,fun=fun)

fun=function (x) {npcrop*x/num}
bi_pcrop<-calc(pcrop,fun=fun)

fun=function (x) {nwaterbody*x/num}
bi_waterbody<-calc(waterbody,fun=fun)

fun=function (x) {nwetland*x/num}
bi_wetland<-calc(wetland,fun=fun)

fun=function (a,b,c,d,e,f,g,h,i,j,k,l,m) {a+b+c+d+e+f+g+h+i+j+k+l+m}
bio_index<-overlay(bi_acrop,bi_cbush,bi_cgrass,bi_dforest,bi_maforest, bi_mforest,bi_obush,bi_ograss,
                   bi_pcrop,bi_pforest,bi_sforest,bi_waterbody,bi_wetland,fun=fun)

#plot(bio_index)


###########number of endangered species that have lost their habitat

if (add>0) {
  
  #computation for the base run 
  tspecies$dmt<-tspecies$dm*tspecies$treatBIN
  numt<-sum(tspecies$dmt*tspecies$treatBIN)
  ndforest<-sum(tspecies$dforest*tspecies$treatBIN)
  nmforest<-sum(tspecies$mforest*tspecies$treatBIN)
  nsforest<-sum(tspecies$sforest*tspecies$treatBIN)
  npforest<-sum(tspecies$pforest*tspecies$treatBIN)
  nmaforest<-sum(tspecies$maforest*tspecies$treatBIN)
  ncgrass<-sum(tspecies$cgrass*tspecies$treatBIN)
  nograss<-sum(tspecies$ograss*tspecies$treatBIN)
  ncbush<-sum(tspecies$cbush*tspecies$treatBIN)
  nobush<-sum(tspecies$obush*tspecies$treatBIN)
  nacrop<-sum(tspecies$acrop*tspecies$treatBIN)
  npcrop<-sum(tspecies$pcrop*tspecies$treatBIN)
  nwetland<-sum(tspecies$wetland*tspecies$treatBIN)
  nwaterbody<-sum(tspecies$waterbody*tspecies$treatBIN)
  
  #computing habitat change for endangered species due to scenario conversion to annual cropland
  # note it is overwriting variables from the biodiversity index 
  fun=function (x) {ndforest*x}
  bis_dforest<-calc(lostdforest,fun=fun)
  
  fun=function (x) {nmforest*x}
  bis_mforest<-calc(lostmforest,fun=fun)
  
  fun=function (x) {nsforest*x}
  bis_sforest<-calc(lostsforest,fun=fun)
  
  fun=function (x) {npforest*x}
  bis_pforest<-calc(lostpforest,fun=fun)
  
  fun=function (x) {nmaforest*x}
  bis_maforest<-calc(lostmaforest,fun=fun)
  
  fun=function (x) {nograss*x}
  bis_ograss<-calc(lostograss,fun=fun)
  
  fun=function (x) {ncgrass*x}
  bis_cgrass<-calc(lostcgrass,fun=fun)
  
  fun=function (x) {nobush*x}
  bis_obush<-calc(lostobush,fun=fun)
  
  fun=function (x) {ncbush*x}
  bis_cbush<-calc(lostcbush,fun=fun)
  
  fun=function (x) {npcrop*x}
  bis_pcrop<-calc(lostpcrop,fun=fun)
  
  fun=function (x) {nwaterbody*x}
  bis_waterbody<-calc(lostwaterbody,fun=fun)
  
  fun=function (x) {nwetland*x}
  bis_wetland<-calc(lostwetland,fun=fun)
  
  fun=function (b,c,d,e,f,g,h,i,j,k,l,m) {b+c+d+e+f+g+h+i+j+k+l+m}
  esp_sc<-overlay(bis_cbush,bis_cgrass,bis_dforest,bis_maforest, bis_mforest,bis_obush,bis_ograss,
                  bis_pcrop,bis_pforest,bis_sforest,bis_waterbody,bis_wetland,fun=fun)
  
  #esp_sc<-zoom(esp_sc, ext=addcrop@extent)
  #plot(esp_sc)
  
}
  

##################landscape level indicators ######### 
#average biodiversity index

bio_index_avg<- round(cellStats(bio_index,stat='mean'),2)

if (add>0) {
  
  esp_sc_max<- round(cellStats(esp_sc,stat='max'),2)
} else {esp_sc_max<- NA }




bio_ind<-data.frame(bio_index_avg, esp_sc_max)
bio_ind_diff<-bio_ind-bio_base
bio_ind_perc<-round(bio_ind_diff/bio_base*100, digits = 1) 
bio_map<-round(bio_index-bio_map,digit=4)
bio_ind2<-rbind(bio_ind,bio_ind_diff,bio_ind_perc)
names(bio_ind2)<-c("avg richness index", "nbr of species loosing critical habitat")
bio_ind2<-data.frame(t(bio_ind2))
colnames(bio_ind2)<-c('result','diffence','percent')

bio_ind_val<-read.csv('1-input/parameter/bio_val.csv')
bio_ind2$evaluation <- ifelse(bio_ind2$percent <= bio_ind_val$val, 'low', ifelse((bio_ind2$percent >= bio_ind_val$val) & (bio_ind2$percent <= 2* bio_ind_val$val),'medium','high'))





title1<-paste('Biodiversity: richness index', name, sep=" ")
title2<-paste('Biodiversity: critically affected species', name, sep=" ")
title3<-paste('Biodiversity: difference of richness index', name, sep=" ")

col<-colorRampPalette(brewer.pal(9,'YlGn'))(100)
col2<-colorRampPalette(rev(brewer.pal(9,'Reds')))(100)
col3<-colorRampPalette((brewer.pal(9,'Reds')))(100)


par(mfrow=c(1,3),mar=c(2, 4.5, 2, 6))
#pdf(paste(name, "biodiversity_pathway.pdf", sep='-'))

plot(bio_index, legend.width=1, legend.shrink=0.45,col=col) 
plot(sarea, add=TRUE) 
title(title1) 

plot(bio_map, legend.width=1, legend.shrink=0.45,col=col2) 
plot(sarea, add=TRUE) 
title(title3) 


if (add>0) {
  plot(esp_sc, legend.width=1, legend.shrink=0.45,col=col3)
  plot(sarea, add=TRUE)
  title(title2)}


#grid.table(bio_ind2)







#####################################################################
#extract the maps for final user
setwd("4-output")

title1<-paste('Biodiversity pathway : richness index', name, sep=" ")
title2<-paste('Biodiversity pathway : critically affected species', name, sep=" ")
title3<-paste('Biodiversity pathway : difference of richness index', name, sep=" ")

col<-colorRampPalette(brewer.pal(9,'YlGn'))(100)
col2<-colorRampPalette(rev(brewer.pal(9,'Reds')))(100)
col3<-colorRampPalette((brewer.pal(9,'Reds')))(100)




pdf(paste(name, "biodiversity_pathway.pdf", sep='-'))

plot(bio_index, legend.width=1, legend.shrink=0.75,col=col) 
plot(sarea, add=TRUE) 
title(title1) 

plot(bio_map, legend.width=1, legend.shrink=0.75,col=col2) 
plot(sarea, add=TRUE) 
title(title3) 


if (add>0) {
  plot(esp_sc, legend.width=1, legend.shrink=0.75,col=col3)
  plot(sarea, add=TRUE)
  title(title2)}


plot(NA, xlim=c(0,100), ylim=c(0,10), bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
grid.table(bio_ind2)


dev.off() 

# tiff(paste(name,"biodiversity_pathway.tiff", sep="-"))
# 
# plot(bio_index, legend.width=1, legend.shrink=0.75,col=col)
# plot(sarea, add=TRUE)
# title(title1)
# 
# dev.off()


if (add>0) {
  outputname<-paste(name,"biodiversity_pathway2.tiff", sep="-")
  tiff(outputname)
  
  plot(esp_sc, legend.width=1, legend.shrink=0.75,col=col)
  plot(sarea, add=TRUE)
  title(title2)
  dev.off()
  
  writeRaster(esp_sc,paste(name,"bioES_map.tif", sep="-"), overwrite=TRUE)
  
}#end of if

writeRaster(bio_index,paste(name,"bio_map.tif", sep="-"),overwrite =TRUE)
write.csv(bio_ind,paste(name,"bio_ind.csv", sep="-"),row.names = F)

