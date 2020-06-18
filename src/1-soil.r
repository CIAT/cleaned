######################### soil health  pathway ##############
#by Catherine Pfeifer
#7.6.2016
#CLEANED version S adapted for CLEANED Tanzania
#modified 23.1.2018

#clearing all memory 

# read libraries
library(raster)
library(maptools)
library(RColorBrewer)
setwd(path)
#pixel<-pixel$x[1]
soil_base<-read.csv('1-input/base_run/soil_ind.csv', row.names = NULL)
#soil_base<-soil_base[ ,2:4]
SB_map<-raster('1-input/base_run/soil_SB_map.tif')
Nin_map<-raster('1-input/base_run/soil_Nin_map.tif')
Nout_map<-raster('1-input/base_run/soil_Nout_map.tif')

########################################################################

#compute the average fertilizer on cropland
#computing manure to be spread per ha based on the data from GHG pathway per yea5
manure_tot<-(numcow_is*mprod_is+numcow_sis*mprod_sis+numcow_es*mprod_es+numcow_da*mprod_da+numsheep*mprod_sh)*365
mprod_g<- ((numcow_is*mprod_is+numcow_es*mprod_es)*mms_pasture_perc/100+(numcow_sis*mprod_sis+numcow_da*mprod_da)*mms_pasture_perc2/100+(numsheep*mprod_sh)*mms_pasture_perc3/100)   *365
mprod_c<- ((numcow_is*mprod_is+numcow_es*mprod_es)*mms_lagoon_perc/100+(numcow_sis*mprod_sis+numcow_da*mprod_da)*mms_lagoon_perc2/100)*365+ 
            ((numcow_is*mprod_is+numcow_es*mprod_es)*mms_liquidslurry_perc/100+(numcow_sis*mprod_sis+numcow_da*mprod_da)*mms_liquidslurry_perc2/100)*365+
            ((numcow_is*mprod_is+numcow_es*mprod_es)*mms_solidstorage_perc/100+(numcow_sis*mprod_sis+numcow_da*mprod_da)*mms_solidstorage_perc2/100)*365+
            ((numcow_is*mprod_is+numcow_es*mprod_es)*mms_drylot_perc/100+(numcow_sis*mprod_sis+numcow_da*mprod_da)*mms_drylot_perc2/100)*365+
            ((numcow_is*mprod_is+numcow_es*mprod_es)*mms_dailyspread_perc/100+(numcow_sis*mprod_sis+numcow_da*mprod_da)*mms_dailyspread_perc2/100)*365+
            ((numsheep*mprod_sh)*(100-mms_pasture_perc3)/100)*365
#slurry production  mprod_sis
slprod_c<- ((numcow_is*mprod_is+numcow_es*mprod_es)*mms_digester_perc/100+(numcow_sis*mprod_sis+numcow_da*mprod_da)*mms_digester_perc2/100)*365

########################################################################
#calculate Nitrogen In Ni+m+A+B
#manure application in kg per hectare per year

#cereal (mprod_c *% to this crop for linking with production)
manc2<-mprod_c * manc
# legumes  (mprod_c *% to this crop for linking with production )
manl2<-mprod_c *manl
#planted fodder  (mprod_c *% to this crop for linking with production )
manpf2<- mprod_c * manpf
#grazing land (mprod_g for linking with other pathway)
mangraz2<-mprod_g 
#rice (mprod_c *% to this crop for linking with production )
manr2<-mprod_c * manr

manconv<- 0.02 #conversion rate between cow manure (NPK) and Nitrogen
Fertconv<-0.2
slurryconv<- 0.001

#pixel<-pixel$x[1]
#fertilizer application rate is in kg per ha, so we need to transform in from ha to pixel 
#so the required transformation is pixel*pixel/10000
Ni_c_u<-(frc*fertc+frl*fertl+fpf*fertpf)*Fertconv * (pixel*pixel/10000) # per pixel
# at this stage we use SEVIR LU so 1 pixel = 30 m 
Ni_g_u<-(fertgraz) *Fertconv*(pixel*pixel/10000) # per pixel

M_c<-(frc*manc2+frl*manl2+fpf*manpf2)*manconv *(pixel*pixel/10000)  # per pixel
M_g<-(mangraz) *manconv *(pixel*pixel/10000)  # per pixel 


S_c<-(frc*sluc+frl*slul+fpf*slupf)*slurryconv *(pixel*pixel/10000)  # per pixel
S_g<-(slugraz) *slurryconv*(pixel*pixel/10000)  # per pixel


# apply the fertilizer manure and slurry to cropland
m <- c(0, 0, 1, (Ni_c_u+M_c+S_c))
rclmat <- matrix(m, ncol=2, byrow=TRUE)
Ni_c <- reclassify(cropland, rclmat)
Ni_c <- cropland
Ni_c[Ni_c==1]<-(Ni_c_u+M_c+S_c)



#apply fertilizer manure and slurry to grazing land
m <- c(0, 0, 1, (Ni_g_u+M_g+S_g))
rclmat <- matrix(m, ncol=2, byrow=TRUE)
Ni_g <- reclassify(grazland, rclmat)


Ni<-Ni_c+Ni_g

#Biological N fixation 
#compute average yield for crop not residues 
avg_l<-cellStats(y_pulsec*(1+pgl),stat='mean')
avg_c<-cellStats(y_maizec*(1+pgc),stat='mean')

# transforming crude protein to nitrogen is 6.25 #ref = ilri feed database, or DAVID A. BENDER. "Nitrogen determination." A Dictionary of Food and Nutrition. 2005. Encyclopedia.com. 25 Nov. 2015 <http://www.encyclopedia.com>.
# we multiply with dry weight so that we get the amount of N depending on biomass
#yield is in terms of kg/pixel (adjusted in spatialdata)
Nc<- cpc*0.01/6.25 * avg_c
#1000 to transform ton into kg, *0.3*0.3 from ha to pixel
#with avg_rc contains post harvest loss, therefore we need to convert it back to biomass in the field
#avg_rc/rfc =ymaize, ymaize/hic =total plant biomass 
Nrc<-cprc*0.01/6.25 * (avg_rc/rfm)/ hic *hirc 
#Nrc is the nitrogen of the whole residue, harvested and fed + remaining in soils
Nl<- cpl*0.01/6.25 * avg_l
Nrl<-cprl*0.01/6.25 *(avg_rl/rfl)/hil*hirl
Npf<-cppf*0.01/6.25 * fy_pf
Ng<-cpg*0.01/6.25 *fy_g

#we multiply proportion of land used for croping in the study area check this do we need 
#the whole biomass here :now we do the whole crop, as we use the proportion of area for each crop
BNcrop<-(ar_pf/(ar_rl+ar_rc+ar_pf))*Npf+(ar_rc/(ar_rl+ar_rc+ar_pf))*(Nrc+Nc)+(ar_rl/(ar_rl+ar_rc+ar_pf))*(Nrl+Nl)
BNgraz<-(ar_g)*Ng

fun<-function (x,y){((2+(x-1350)*0.005)+(0.5*BNcrop))*y}
BNc<-overlay(rain,cropland,fun=fun)
m<-c(-100000,0,0,0.000001,100000,1)
rclmat<-matrix(m,ncol=3, byrow=T)
BNdum<-reclassify(BNc,rclmat)
#BNdum2<-reclassify(BN_c_sc,rclmat)
fun1<-function(x,y){x*y}
BNc<-overlay(BNc,BNdum,fun=fun1 )

fun<-function (x,y){((2+(x-1350)*0.005)+(0.5*BNgraz))*y}
BNg<-overlay(rain,grazland,fun=fun)

BNdum<-reclassify(BNg,rclmat)
#BNdum2<-reclassify(BNg_sc,rclmat)
BNg<-overlay(BNg,BNdum,fun=fun1 )
#BNg_sc<-overlay(BNg_sc,BNdum2,fun=fun1 )
#plot(BNg)


#compute Nitrogen IN
fun<-function(a,b,c,d,e){a+b+c+d+e}
N_in <-overlay(Ni_c,Ni_g, A, BNc, BNg,fun=fun)
#plot(N_in)



###########################################################
#compute the Nitogen out, Nyield + Nres + L + G + E


fun<-function(x,y) {(BNcrop*x)+BNgraz*y}
Nh<-overlay(cropland, grazland, fun=fun)
#plot(Nh)
# Nh_sc<-overlay(newcrop, newgraz, fun=fun)
# plot(Nh_sc)fun<-function(a,b,c){(a+b+c)/3}


# compute L 
Nsoil<-raster('1-input/spatial/soil/Nsoil.tif')

#0.2 comes from the fact that we only take the first 20 cm Ni into account
#SN is in gr/kg so *0.001 => kg/kg
#L= (Nsoil + Ni + M) * rainfall factor (%of N loss in relation to rainfallgiven clay content)
# see Smaling1993

miner=2.5 #mineralization rate
fun=function(x,y,z){20*(x+y+z)*miner*(pixel*pixel/10000)}
#*0.03*0.03 in order to transform ha into pixel 

Nmin<-overlay(Nsoil,Ni_c,Ni_g, fun=fun)
m<-c(0,35,1,35.0001,100,0)
rclmat<-matrix(m,ncol=3,byrow=T)
CLclass1<-reclassify(CLavg,rclmat)

# m<-c(0,35,0,35.0001,55,1,55.0001,100,0)
# rclmat<-matrix(m,ncol=3,byrow=T)
# CLclass2<-reclassify(CLavg,rclmat)
# 
# m<-c(0,35,0,35.0001,55,0,55.0001,100,1)
# rclmat<-matrix(m,ncol=3,byrow=T)
# CLclass3<-reclassify(CLavg,rclmat)
# 
# fun=function(x,y){(0.0205*x-2.2846)*y}
# RFc1<-overlay(rain,CLclass1,fun=fun)
# 
# fun=function(x,y){(0.0138*x+1.4615)*y}
# RFc2<-overlay(rain,CLclass2,fun=fun)
# 
# fun=function(x,y){(0.0066*x+6.2538)*y}
# RFc3<-overlay(rain,CLclass3,fun=fun)
# this part is now compted is spatialdata_soil

CLclass2<-raster('1-input/spatial/soil/CLclass2.tif')
CLclass3<-raster('1-input/spatial/soil/CLclass3.tif')
fun=function(x,y){(0.0205*x-2.2846)*y}
RFc1<-overlay(rain,CLclass1,fun=fun)
RFc2<-raster('1-input/spatial/soil/RFc2.tif')
RFc3<-raster('1-input/spatial/soil/RFc3.tif')


fun<-function(x,y,z,a){(x+y+z)/100*a}
L<-overlay(RFc1,RFc2,RFc3,Nmin, fun=fun)
#plot(L)
remove(RFc1,RFc2,RFc3)


#G = (Nsoil + Ni + M) * SoilRainfactor
# see Smaling1993 # 

# this equation does not work for the ETH context, so we used that equation and normalized it to fit between 0 and 1


DN <- (-9.4+(0.13*CLavg)+(0.01*rain))
DNmin<-cellStats(DN, stat="min")
DNmax<-cellStats(DN, stat="max")

#normalize
DN<-(DN-DNmin)/(DNmax-DNmin)

fun<-function(x,y,z,a,b){(x+y+z)*DN}
G<-(Nsoil+Ni_c+Ni_g)*DN

#plot(G)

#Nitrogen loss through erosion 

#soil loss is in tons per hectare so now we tranform into kg of N per pixel 

fun<-function(x,y){x*1000*(pixel*pixel/10000)*y/1000*1.5}
NE<- overlay(E,SN1,fun=fun)
#plot(NE)


#Nitrogen OUT with RUSLE
#N_out<-sum(Nh,L,G,NE)
fun<-function (x,y,z, w) {x+y+z+w}
N_out<-overlay(Nh,L,G,NE,fun=fun)

#N_out_sc<-overlay(Nh_sc,L_sc,G_sc,NE, fun=fun)

#plot(N_out)
#plot(N_out_sc)


####################################soil nutrient balance #############

fun<-function(x,y){x-y}
SB<-overlay(N_in, N_out,fun=fun)
#plot(SB)

#################################visualisation 
###################landscape level indicator############## 


# overall soil balance
SB_l<-round(cellStats(SB,stat='sum'),0) # total soil balance 
# Ni applied through manure 
Ni_l<-round(cellStats(Ni,stat='sum'),0) # total applied manure 
# Ni applied through manure per cow
Ni_cow<-round(Ni_l/numcow,3) # total Nitrogen producted from manure 

soil_ind<-data.frame(SB_l,Ni_l,manure_tot)

#indifference computation 
soil_ind_diff<-soil_ind-soil_base
soil_ind_perc<-round(soil_ind_diff/soil_base*100, digits = 1)
soil_ind2<-rbind(soil_ind,soil_ind_diff,soil_ind_perc)
names(soil_ind2)<-c("nitrogen balance", "total nitrogen added","total manure produced" )
soil_ind2<-data.frame(t(soil_ind2))
colnames(soil_ind2)<-c('result','difference','percent')

 ##steve edits
 soil_ind_val<-read.csv('1-input/parameter/soil_val.csv')
 soil_ind2$evaluation <- ifelse(abs(soil_ind2$percent) <= soil_ind_val$val, 'low', ifelse((abs(soil_ind2$percent) >= soil_ind_val$val) & (abs(soil_ind2$percent) <= 2*soil_ind_val$val),'medium','high'))
 

SB_diff<-round(SB-SB_map,digit=2)
Nin_diff<- round(N_in-Nin_map,digit=2)
Nout_diff<- round(N_out-Nout_map,digit=2 )



title<-paste('Soil balance', name, sep=" ")
title2<-paste('Nitrogen input', name, sep=" ")
title3<-paste('Nitrogen outake', name, sep=" ")
title4<-paste('difference in soil balance', name, sep=" ")
title5<-paste('difference in nitrogen input', name, sep=" ")
title6<-paste('difference in nitrogen outake', name, sep=" ")
col<-colorRampPalette(brewer.pal(9,'Oranges'))(100)
col2<-colorRampPalette(brewer.pal(9,'BuPu'))(100)
col3<-colorRampPalette(brewer.pal(9,'PuRd'))(100)
col4<-colorRampPalette(rev(brewer.pal(9,'Reds')))(100)

par(mfrow=c(3,2),mar=c(2, 4.5, 2, 6))
#pdf(paste(name, "soil_pathway.pdf", sep='-'))

plot(SB, legend.width=1, legend.shrink=0.45,col=col)
plot(sarea, add=TRUE)
title(title)

plot(SB_diff, legend.width=1, legend.shrink=0.45,col=col4)
plot(sarea, add=TRUE)
title(title4)

plot(N_in, legend.width=1, legend.shrink=0.45,col=col2)
plot(sarea, add=TRUE)
title(title2)

plot(Nin_diff, legend.width=1, legend.shrink=0.45,col=col4)
plot(sarea, add=TRUE)
title(title5)

plot(N_out, legend.width=1, legend.shrink=0.45,col=col3)
plot(sarea, add=TRUE)
title(title3)


plot(Nout_diff, legend.width=1, legend.shrink=0.45,col=col4)
plot(sarea, add=TRUE)
title(title6)

#################################visualisation 
#extract the maps for final user
setwd("4-output")
title<-paste('Soil balance', name, sep=" ")
title2<-paste('Nitrogen input', name, sep=" ")
title3<-paste('Nitrogen outake', name, sep=" ")
title4<-paste('difference in soil balance', name, sep=" ")
title5<-paste('difference in nitrogen input', name, sep=" ")
title6<-paste('difference in nitrogen outake', name, sep=" ")
col<-colorRampPalette(brewer.pal(9,'Oranges'))(100)
col2<-colorRampPalette(brewer.pal(9,'BuPu'))(100)
col3<-colorRampPalette(brewer.pal(9,'PuRd'))(100)
col4<-colorRampPalette(rev(brewer.pal(9,'Reds')))(100)


pdf(paste(name, "soil_pathway.pdf", sep='-'))
plot(SB, legend.width=1, legend.shrink=0.75,col=col)
plot(sarea, add=TRUE)
title(title)

plot(N_in, legend.width=1, legend.shrink=0.75,col=col2)
plot(sarea, add=TRUE)
title(title2)

plot(N_out, legend.width=1, legend.shrink=0.75,col=col3)
plot(sarea, add=TRUE)
title(title3)

plot(SB_diff, legend.width=1, legend.shrink=0.75,col=col4)
plot(sarea, add=TRUE)
title(title4)

plot(Nin_diff, legend.width=1, legend.shrink=0.75,col=col4)
plot(sarea, add=TRUE)
title(title5)

plot(Nout_diff, legend.width=1, legend.shrink=0.75,col=col4)
plot(sarea, add=TRUE)
title(title6)

plot(NA, xlim=c(0,100), ylim=c(0,10), bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
grid.table(soil_ind2)

dev.off()

writeRaster(SB,paste(name,"soil_SB_map.tif", sep="-"), overwrite= T)
writeRaster(N_in,paste(name,"soil_Nin_map.tif", sep="-"), overwrite= T)
writeRaster(N_out,paste(name,"soil_Nout_map.tif", sep="-"), overwrite= T)
write.csv(soil_ind,paste(name,"soil_ind.csv", sep="-"),row.names = F)

