library(gridExtra)

#read parameter
para<-read.csv('1-input/parameter/para.csv')
para$value<-as.numeric(para$value)
dim<-dim(para)
obs<-dim [1]
for(i in seq_along(para$name))
{assign(as.character(para$name[i]), para$value[i])}

#do we run a land use change driven scenario? then we need to run the land use change module first 
# and read here the file indicating the pixels that have changed

#add the path to the changes in land use rasters
#path to changing cropland i.e. cropland change layer


#now overwrite variables with preset
if(preset==1){
    # get the parameter of the pastoral extensive system
    temp<-scenario[ ,c('Lvar', L)]
    names(temp)[2]<-'ppara'
    for(i in  which(!temp$Lvar== ''))
    {
        assign(as.character(temp$Lvar[i]), temp$ppara[i])
    }}

if(preset==1){  
    # get the parameter of semi intensive system
    temp<-scenario[ , c('Cbvar', Cb )]
    names(temp)[2]<-'ppara'
    for(i in  which(! temp$Cbvar== ''))
    {
        assign(as.character(temp$Cbvar[i]), temp$ppara[i])
    }}

if(preset==1){
    # get the parameter of the  dairy pure breed  system
    temp<-scenario[ , c('Evar', Eb )]
    names(temp)[2]<-'ppara'
    for(i in  which(! temp$Evar== ''))
    {
        assign(as.character(temp$Evar[i]), temp$ppara[i])
    }}


if(preset==1){
    # get the parameter of the cropping system
    temp<-scenario[ , c('Crvar',Cr)]
    names(temp)[2]<-'ppara'
    for(i in  which(! temp$Crvar== ''))
    {
        assign(as.character(temp$Crvar[i]), temp$ppara[i])
    }}

# end of else loop




###################computation of the number of animals in the landscape##########
#numbers of cow in the area

numcow<-numcow_es+numcow_sis+numcow_is

############################these are the computation for cattle############################
# for any other animal, this has to be adjusted based on IPCC guidlines 


#total energy requirement per average cow per year per system

#maintenance energy eq 10.3, and table 10.4 energy per day

er_mes<-0.386 *lwes^0.75
er_msis<-0.386*lwsis^0.75
er_mis<-0.386*lwis^0.75

#activity energy equ 10.4 table 10.5 # per day

er_ames<-0.36*er_mes # large gazing area
er_amsis<-0.17*er_mis 
er_amis<-0*er_mis # assumtion these animal still graze a bit  so the value is between 0 and 0.17

#lactation energy eq 10.8 assuming milk fat content of 3.5%, this equation is per year 
# because milk production is per year
er_les<-myes*(1.47+0.4*3.5)
er_lsis<-mysis *(1.47+0.4*3.5)
er_lis<-myis*(1.47+0.4*3.5)
#net energy requirement per cow per day
erc_es<-(er_mes+er_ames)+ er_les/365 
erc_sis<-(er_msis+er_amsis)+ er_lsis/365 
erc_is<-(er_mis+er_amis)+ er_lis/365 

#total energy needed for maintaining dairy animals in Lushoto
erc<-(erc_es * numcow_es + numcow_sis * erc_sis + numcow_is* erc_is)

##############################################################################################
#calculate energy requirement in by season  from each feed source 
#dry  season 
ng_ds<-(efng1/100*numcow_es*erc_es + sfng1/100*numcow_sis * erc_sis + ifng1/100 * numcow_is*erc_is )*ds/12 * 365
rc_ds<-(efrc1/100*numcow_es*erc_es + sfrc1/100*numcow_sis * erc_sis + ifrc1/100 * numcow_is*erc_is )*ds/12 * 365
rr_ds<-(efrr1/100* numcow_es*erc_es +sfrr1/100*numcow_sis * erc_sis + ifrr1/100 * numcow_is*erc_is )*ds/12 * 365
rl_ds<-(efrl1/100*numcow_es*erc_es + sfrl1/100*numcow_sis * erc_sis + ifrl1/100 * numcow_is*erc_is )*ds/12 * 365
pf_ds<-(efpf1/100*numcow_es*erc_es + sfpf1/100*numcow_sis * erc_sis + ifpf1/100 * numcow_is*erc_is )*ds/12 * 365
conc_ds<-(efconc1/100*numcow_es*erc_es+ sfconc1/100*numcow_sis*erc_sis + ifconc1/100 * numcow_is*erc_is)*ds/12 * 365
conos_ds<-(efconos1/100*numcow_es*erc_es + sfconos1/100*numcow_sis*erc_sis + ifconos1/100 * numcow_is*erc_is)*ds/12 * 365
hay_ds<-(efhay1/100*numcow_es*erc_es + sfhay1/100*numcow_sis * erc_sis + ifhay1/100 * numcow_is*erc_is )*ds/12 * 365
sil_ds<-(efsil1/100*numcow_es*erc_es + sfsil1/100*numcow_sis * erc_sis + ifsil1/100 * numcow_is*erc_is )*ds/12 * 365


#wet season 
ng_ws<-(efng2/100* numcow_es*erc_es + sfng2/100*numcow_sis*erc_sis + ifng2/100 * numcow_is*erc_is)*ws/12 * 365
rc_ws<-(efrc2/100* numcow_es*erc_es + sfrc2/100*numcow_sis*erc_sis + ifrc2/100 * numcow_is*erc_is)*ws/12 * 365
rr_ws<-(efrr2/100* numcow_es*erc_es + sfrr2/100*numcow_sis*erc_sis + ifrr2/100 * numcow_is*erc_is)*ws/12 * 365
rl_ws<-(efrl2/100* numcow_es*erc_es + sfrl2/100*numcow_sis*erc_sis + ifrl2/100 * numcow_is*erc_is)*ws/12 * 365
pf_ws<-(efpf2/100* numcow_es*erc_es + sfpf2/100*numcow_sis*erc_sis + ifpf2/100 * numcow_is*erc_is)*ws/12 * 365
conc_ws<-(efconc2/100* numcow_es*erc_es + sfconc2/100*numcow_sis*erc_sis + ifconc2/100 * numcow_is)*ws/12 * 365
conos_ws<-(efconos2/100* numcow_es*erc_es + sfconos2/100*numcow_sis*erc_sis + ifconos2/100 * numcow_is)*ws/12 * 365
hay_ws<-(efhay2/100*numcow_es*erc_es + sfhay2/100*numcow_sis * erc_sis + ifhay2/100 * numcow_is*erc_is )*ws/12 * 365
sil_ws<-(efsil2/100*numcow_es*erc_es + sfsil2/100*numcow_sis * erc_sis + ifsil2/100 * numcow_is*erc_is )*ws/12 * 365

#total energy required from each source of food
ng<-ng_ws + ng_ds
rc<- rc_ws + rc_ds
rr<- rr_ws+ rr_ds
rl<-rl_ws + rl_ds
pf<-pf_ws + pf_ds
conc<-conc_ws + conc_ds
conos<-conos_ws + conos_ds
hay<-hay_ds+hay_ws
sil<-sil_ds+sil_ws

# fraction of each fodder at landscape scale over the year step. 
fng<- ng/(ng+rc+rl+pf+conc+conos+hay+sil)
frc<- rc/(ng+rc+rl+pf+conc+conos+hay+sil)
frr<- rr/(ng+rc+rl+pf+conc+conos+hay+sil)
frl<-rl/(ng+rc+rl+pf+conc+conos+hay+sil)
fpf<-pf/(ng+rc+rl+pf+conc+conos+hay+sil)
fconc<-conc/(ng+rc+rl+pf+conc+conos+hay+sil)
fconos<-conos/(ng+rc+rl+pf+conc+conos+hay+sil)
fhay<-hay/(ng+rc+rl+pf+conc+conos+hay+sil)
fsil<-sil/(ng+rc+rl+pf+conc+conos+hay+sil)


#calculating the fresh weight of feed in basket percent were in fresh weigh already so we need to correct only for dried stuff
# corrected for the fact that meg (metabolizing energy)
fw_g<-ng/(meg)
fw_rc<-rc/(merc)
fw_rl<-rl/(merl)
fw_pf<-pf/(mepf)
#fw_rr<-rr/(merr)
fw_conc<-conc/(meconc)
fw_conos<-conos/(meconos)
fwg_hay<-hay/meh/(dm_h)
fwg_sil<-sil/mes/dm_s


#ratio of net energy available in the diet for maintenance to digestible energy consumed (REM)
#first compute digestibiliy of the landscape level feed
#de<-(ng*d_g+rc*d_rc+rl*d_rl+pf*d_pf+conc*d_conc+conos*d_conos+rr*d_rr)*100 # this seems to be wrong 11.3.2017
de<-(fng*d_g+frc*d_rc+frl*d_rl+fpf*d_pf+fconc*d_conc+fconos*d_conos+fhay*d_h+fsil*d_s)*100 

#REM = ratio of net energy available in a diet for maintenance to digestible energy consumed
rem=(1.123-(4.092*10^-3*de)+(1.126*10^-5*de^2)-(25.4/de))

#yearly gross energy requirement at landscape scale 
#23.01.2018 seems to be daily
gerc<-(erc/rem)/(de/100) #equation 10.16 this is therefore GE at landscape scale per day


# calculate production 
# milk production in tons of litre 
milk_es<-numcow_es*myes *0.001
milk_sis<- numcow_sis * mysis * 0.001 
milk_is<- numcow_is * myis * 0.001 
milk<-milk_es+milk_sis+milk_is

# area needed for the feed production (with adjustement to pass from ha to km2, and tons to kg)
#first we need to compute the fodder yield. This is done by multiplying the crop yield with the
# residue factor, which is the part that is used as feed. This is computed in the water pararmeter
#excel sheet and accounts for post harvest loss

fun=function(x){x*(1+pgc)*rfm} # residue factor to account for what is consummed by livestock only
fy_rc<-overlay(y_maize,fun=fun) # ymaize is in kg/pixel 

fun=function(x){x*rfl}
fy_rl<-overlay(y_pulse,fun=fun)

fun=function(x,y){x*(1+pgc)*y}
y_maizec<-overlay(y_maize,cropland,fun=fun) 
# max_c<-cellStats(y_maizec,stat='sum')
# 
y_pulsec<-overlay(y_pulse,cropland,fun=fun)
# max_l<-cellStats(y_pulsec,stat='sum')
# 
#y_ricec<-overlay(y_rice,riceland,fun=fun)
# max_r<-cellStats(y_ricec,stat='sum')

fy_rcc<-overlay(fy_rc,cropland,fun=fun)
# max_rc<-cellStats(fy_rcc,stat='sum')
# 
fy_rlc<-overlay(fy_rl,cropland,fun=fun)
# max_rl<-cellStats(fy_rlc,stat='sum')
# 
# max_rl<-cellStats(fy_rlc,stat='sum')
###check from here in principle one should not work with the area but the produce... 


#we assume that every cropland pixel produces the basket so we need to correct the total production
#from the area to avoid double planting 

#compute the percentage of each feed on cropland
#so we need to compute the area used for each crop (in term of pixels, so in a 900m2)
fy_rcc[fy_rcc==0]<-NaN
avg_rc<-cellStats(fy_rcc,stat='mean') # per pixel
#area required 
ar_rc<-fw_rc/avg_rc  # number pixel * adjustement in km2 21.3.2018 the data is already adjusted in the spatial extraction 
fy_rlc[fy_rlc==0]<-NaN

avg_rl<-cellStats(fy_rlc,stat='mean')
ar_rl<-fw_rl/avg_rl 
ar_pf<-(fw_pf  + fwg_sil )/(fy_pf*1000/10000*pixel[[1]]^2*rfpf) # we consider silage  as planted forrage hay is considered to be from natural grass 3.5.2018
#fy_pf is in tons per ha so to get kg we 1000 and from ha to m is /10000, so the denominator is in kg/pixel and the result is a number of pixel
 #total cropland area required
ar<-(ar_rc+ar_rl+ar_pf )*pixel[[1]]^2/1000000 # all ar are in numbers of pixel so we need transform in km2

dforest<-raster('1-input/spatial/lu/dforest.tif')
mforest<-raster('1-input/spatial/lu/mforest.tif')
cropland<-raster('1-input/spatial/lu/cropland.tif')
grazland<-raster('1-input/spatial/lu/grazland.tif')
obush<-raster('1-input/spatial/lu/obush.tif')
cbush<-raster('1-input/spatial/lu/cbush.tif')
grazland<-grazland+obush+cbush
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


# check if ther e is a land use change required and run the code that will yield at add crop
if (add > 0){
    source('3-cleaned/2-luc.r')}
# compute the new land cover layers
if (add>0) {
    source('3-cleaned/2-luccomp.r')
}


#calculate the production of planted fodder under cropland 
croparea<-cellStats(cropland,stat='sum')*pixel[[1]]^2/1000000 #area in km
# computing needs for the crossing animals assuming they are staying 1 month in the area

diff_ar<-croparea-ar   # ar is in pixel so need t be transformed in km
if (diff_ar>0){
  import_c = 0
  totmaize= cellStats(y_maizec*(1+pgc),stat='mean')*ar_rc*0.001+  cellStats(y_maizec*(1+pgc),stat='mean')*(diff_ar)*0.001
  
  } else {import_c= -diff_ar
       
  totmaize= cellStats(y_maizec*(1+pgc),stat='mean')*ar_rc*0.001+  cellStats(y_maizec*(1+pgc),stat='mean')*(diff_ar)*0.001
  } # now imports are in area in km2 
diff_ar<-round(diff_ar, digit=0)
#calculate the production under grazing land 

# lets compute the maize produced in the area
# original yield is kg/pixel and ar_rc is in pixel so *0.001 it to get tons
# we also assume that all the cropland that is not used for livestock is actually maize
# then we can add this to the maize produced in the area
# it can be that there is less maize as by product to livestock, yet the total land used for livestock 
# is less, so there is no food security issue





grazarea<-cellStats(grazland,stat='sum')*pixel[[1]]^2/1000000 #area in km2
ar_g<-(fw_g+ fwg_hay)/(fy_g)*0.01/1000 #*0.01 from ha to km2 and 1000 for passing from tons to kg,  we are alredy in tons, so we need to go into kg
#now hay is from natural grass! 3.5.2018 reports from stakeholders (district representative) that hey is from grassland
# and sillage from crops and planted fodder.

diff_g <- grazarea-ar_g
if (diff_g>0){
  import_g = 0
} else {import_g<--diff_g}
diff_g <- round(diff_g, digit=0)

####################################moving the manure management computation here as they are used in soil and ghg 
species<-'dairyCows'

species4<- 'Cattle'

#Africa, Asia, Latin America
region='Africa'
country='Tanzania' # for global forest ressource
#for dairy 
# extracting the IPCC parameter for each management type depending on temperature 
mms_lagoon      <-(es_lagoon_perc*numcow_es+      sis_lagoon_perc*numcow_sis+      is_lagoon_perc*numcow_is)
mms_liquidslurry<-(es_liquidslurry_perc*numcow_es+sis_liquidslurry_perc*numcow_sis+is_liquidslurry_perc*numcow_is)
mms_solidstorage<-(es_solidstorage_perc*numcow_es+sis_solidstorage_perc*numcow_sis+is_solidstorage_perc*numcow_is)
mms_drylot      <-(es_drylot_perc*numcow_es+      sis_drylot_perc*numcow_sis+      is_drylot_perc*numcow_is)
mms_pasture     <-(es_pasture_perc*numcow_es+     sis_pasture_perc*numcow_sis+     is_pasture_perc*numcow_is)
mms_dailyspread <-(es_dailyspread_perc*numcow_es+ sis_dailyspread_perc*numcow_sis+ is_dailyspread_perc*numcow_is)
mms_digester    <-(es_digester_perc*numcow_es+    sis_digester_perc*numcow_sis+    is_digester_perc*numcow_is)
mms_fuel        <-(es_fuel_perc*numcow_es+        sis_fuel_perc*numcow_sis+        is_fuel_perc*numcow_is)
mms_other       <-(es_other_perc*numcow_es+       sis_other_perc*numcow_sis+       is_other_perc*numcow_is)
mms_total <-mms_lagoon+mms_liquidslurry+mms_solidstorage+mms_drylot+mms_pasture+mms_dailyspread+mms_fuel+mms_other

mms_lagoon_perc<-mms_lagoon/mms_total*100
mms_liquidslurry_perc<- mms_liquidslurry/mms_total*100
mms_solidstorage_perc<- mms_solidstorage/mms_total*100
mms_drylot_perc<- mms_drylot/mms_total*100
mms_pasture_perc<- mms_pasture/mms_total*100
mms_dailyspread_perc<- mms_dailyspread/mms_total*100
mms_digester_perc<- mms_digester/mms_total*100
mms_fuel_perc<- mms_fuel/mms_total*100
mms_other_perc<- mms_other/mms_total*100

mms_params1 <- read.csv('1-input/parameter/MMSparams.csv')
mms_params <-subset(mms_params1 , mms_params1$Species==species&mms_params1$Region==region)
B0<-mms_params$B0

if(ipcc==1){
  mms_lagoon_perc<-mms_params$lagoon_perc
  mms_liquidslurry_perc<-mms_params$Liquid.slurry_perc
  mms_solidstorage_perc<-mms_params$Solid.storage_perc
  mms_drylot_perc<-mms_params$Dry.lot_perc
  mms_pasture_perc<-mms_params$Pasture_perc
  mms_dailyspread_perc<-mms_params$Daily.spread_perc
  mms_digester_perc<-mms_params$Digester_perc
  mms_fuel_perc<-mms_params$Burned.for.fuel_perc
  mms_other_perc<-mms_params$Other_perc}


# if (add > 0) {
#   source('3-cleaned/2-luccomp.r') taken out on 28.3.2018 check if it still works, there is this code at the begining of the code
# }

prod_base<-read.csv('1-input/base_run/prod_ind.csv', row.names = NULL)
#prod_base<-prod_base[,2:length(prod_base)]

import_c<-round(import_c,digits=0)
import_g<-round(import_g,digits=0)
croparea<-round(croparea,digits=0)
grazarea<-round(grazarea,digits=0)
#ricearea<-round(ricearea,digits=0)
totmaize<-round(totmaize,digits=0)
ar_g<-round(ar_g,digits=1)
ar<-round(ar,digits=1)
#ar_rr<-round(ar_rr,digits=1)





prod_ind<-data.frame(milk,totmaize ,croparea,grazarea, ar, ar_g,import_c, import_g  , numcow, numcow_es,numcow_sis, numcow_is)
prod_ind_diff<-prod_ind-prod_base
prod_ind_per<-round(prod_ind_diff/prod_base*100, digit=1)
prod_ind_per<-ifelse(is.na(prod_ind_per) ,0,prod_ind_per)
prod_ind2<-rbind(prod_ind,prod_ind_diff)
prod_ind2<-rbind(prod_ind2, prod_ind_per)
names(prod_ind2)<-c("milk produced", "tons of maize produced", 'total area available for crop', 'total area available for pasture', "crop area used", 'pasture area used'
                    , "import crop", 'import pasture', 'total numbers of cows', "local breed cow", 'cross-breed cows', 'pure breed cows')

prod_ind2<-data.frame(t(prod_ind2))
colnames(prod_ind2)<-c('result','diff','percent')
##steve edits
prod_ind_val<-read.csv('1-input/parameter/productivity_val.csv')
prod_ind2$evaluation <- ifelse(abs(prod_ind2$percent) <= prod_ind_val$val, 'low', ifelse((abs(prod_ind2$percent) >= prod_ind_val$val) & (abs(prod_ind2$percent) <= 2*prod_ind_val$val),'medium','high'))


setwd("4-output")
pdf(paste(name,"productivity.pdf", sep="-"))
#par(mfrow=(c(2,1)))

plot(NA, xlim=c(0,100), ylim=c(0,10), bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
grid.table(prod_ind2)

dev.off()

write.csv(prod_ind,paste(name,"prod_ind.csv", sep="-"),row.names = F)
setwd(path)

