######################### greenhouse gases  pathway ##############
#by Catherine Pfeifer
#21.6.2016
#last modified 22.1.2018


setwd(path)
Coe_base<-read.csv('1-input/base_run/ghg_ind.csv', row.names = NULL)
#Coe_base<-Coe_base[ ,2:7]
Coe_map<-raster('1-input/base_run/ghg_map.tif')
####################################

########################################enteric fermetation ########################################


#livestock distribution 

#####calculating percentage of DM total
dmi<-((fw_g*dm_g)+(fw_rc*dm_rc)+(fw_rl*dm_rl)+(fw_pf*dm_pf)+(fw_conc*dm_conc)+(fw_conos*dm_conos)+(fwg_hay*dm_h)+(fwg_sil*dm_s))
perc_dm_g<-(fw_g*dm_g)/dmi
perc_dm_rc<-(fw_rc*dm_rc)/dmi
perc_dm_rl<-(fw_rl*dm_rl)/dmi
perc_dm_pf<-(fw_pf*dm_pf)/dmi
perc_dm_conc<-(fw_conc*dm_conc)/dmi
perc_dm_conos<-(fw_conos*dm_conos)/dmi
perc_dm_sil<-(fwg_hay*dm_h)/dmi
perc_dm_h<-(fwg_sil*dm_s)/dmi

# this computation if for dairy system as proposed by Gerber (2011)
#calculating LCIDE and gama m (ym)
#Average digestibilities assumed to be: Pasture 0.66, crop residue 0.526, fodder 0.567, Concentrate 0.8
lcide <- (perc_dm_g*d_g)+(perc_dm_rc*d_rc)+(perc_dm_rl*d_rl)+(perc_dm_pf*d_pf)+(perc_dm_conc*d_conc)+(perc_dm_conos*d_conos)+(perc_dm_sil*d_s)+(perc_dm_h*d_h)
# methane conversion factor, constants are taken from (tier 2 estimation) Gerber et al 2011
ym<- (9.75 - 0.05 * lcide) # SF report page 11 
#gross energy intake, constant is default value IPCC, IPCC guideline p 10.21

#ge <- (dmi*18.45)
ge<-gerc/numcow #gerc is at landscape scale daily 
#we now compute the full energy requirement equations so we do not rely on ipcc shortcut average anymore. 
# when digestibility increases then entiric fermentation decreases
# with our base run feed basket, on digestibility is lower than ipcc standards

#emissions (per animal) per day 
rum_ch4_day <- (ge*(ym/100))/55.65
# IPCC default value IPCC guideline 10.31
rum_ch4_year <- rum_ch4_day*365
#computation of the CO2 equivalent IPCC guideline, 100 years
rum_co2e_yeartot <- rum_ch4_year*25*numcow

#spatially allocation of the impact

sumlivdist<-cellStats(livdist,stat='sum',na.rm=TRUE)
fun<-function(x){x*rum_co2e_yeartot/sumlivdist}
rum_co2e_map<-overlay(livdist,fun=fun)


###################################################emissions from manure management#################################

#volatile solids 
#ash = mineral content, this is an IPCC value (2006-) but can be customized 
#ge is  per cow
vs <- (ge*(1-lcide)+(0.04*ge))*((1-ash)/18.45) #IPCC 10.24
#0.04=urinary energy default
#vs <- ((dmi) * (1.04 - lcide))*(1-ash) #LEAP guidelines gives the same answer

#the percentage comuptation are now it in the feedbasket 

# differentiate between pasture = grazing land, solid storage, slurry and daily spread= cropland, other =uniform)

fun <-function (a,b,c,d,f,g,h,i ) {vs*365*B0*0.67 * ((a/100*mms_lagoon_perc/100)+
                                                       (b/100*mms_liquidslurry_perc/100)+
                                                       (c/100*mms_solidstorage_perc/100)+
                                                       (d/100*mms_drylot_perc/100)+
                                                       (f/100*mms_dailyspread_perc/100)+
                                                       (g/100*mms_digester_perc/100)+
                                                       (h/100*mms_fuel_perc/100)+
                                                       (i/100*mms_other_perc/100))}

efc<- overlay (mms_lagoon_mcf, mms_liquidslurry_mcf, mms_solidstorage_mcf ,mms_drylot_mcf,mms_dailyspread_mcf,mms_digester_mcf,mms_burned_mcf,mms_other_mcf, fun=fun )


fun1<-function(a,b){a*numcow/ncellcrop*b }
manure_CH4_year_herd_c<-overlay(efc,cropland, fun=fun1)

#we assume that the proportion of cows on pastures is equivalent of the manure on pastures, so here we have all the others, so 1-percent pasture


#plot(manure_CH4_year_herd_c)

#CH4 estimates per cow
fun <-function (e,d ) {(vs*365*B0*0.67 *(e/100*mms_pasture_perc/100))*d}
efg <- overlay (mms_pasture_mcf,grazland, fun=fun )

#spatial allocation
fun<- function(a,c){a*numcow/ncellgraz*c}
manure_CH4_year_herd_g <- overlay (efg,grazland, fun=fun )

#total
fun=function(x,y) {x+y}
manure_CH4_year_herd_tot <-overlay(manure_CH4_year_herd_g, manure_CH4_year_herd_c, fun=fun)
#plot(manure_CH4_year_herd_tot)


#N2O emissions
NrateLook <- read.csv('1-input/parameter/MMS_Nrate.csv')
MMS_N_EF <- read.csv('1-input/parameter/MMSn2oEF.csv')
MMS_N_EFmean <- mean(MMS_N_EF$EF, na.rm=TRUE)

#for dairy
Nrate <- NrateLook$ExcretionRate[NrateLook$Species==species & NrateLook$Region==region]
Nex <- Nrate*((lwis*numcow_is+lwes*numcow_es+lwsis*numcow_sis)/1000)*365 #for all dairy animals this is tier 1  equation 10.30

#Direct
#dairy
N2O_d_year_herd <- sum(Nex*mms_lagoon_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="UncoveredAnerobic"] , 
                         Nex*mms_liquidslurry_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="Liquid"],
                         Nex*mms_solidstorage_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="SolidStorage"],
                         Nex*mms_drylot_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="DryLot"],
                         Nex*mms_dailyspread_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="DailySpread"],
                         Nex*mms_digester_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="AnerobicDigester"],
                         Nex*mms_other_perc/100 * MMS_N_EFmean)*(44/28) 
#44/28 = conversion of (N2O-N)(mm) emissions to N2O(mm) emissions

# Pasture (grazed and deposited) calculated is now calculted here 

#Direct and indirect N2O on pastures
Ndeposit <- (Nex)*(mms_pasture_perc/100) #Equation 11.5 IPCC

EF_CPP <- 0.02 #Default emission factor for cattle, pigs and poultry
#EF_SO <- 0.01 #Default emission factor for sheep and others
N20deposit_direct <- (Ndeposit * EF_CPP)*(44/28) #Equation 11.1 IPCC

FracGASM <- 0.2 #Default value for fraction of applied organic N that volatilises as NH3 and NOx
EF_indirectN2Opasture <- 0.01 # Default emission factor
N2Odeposit_indirect <- (Ndeposit * FracGASM * EF_indirectN2Opasture)*(44/28) #Equation 11.9 IPCC


N_loss <- read.csv('1-input/parameter/MMS_Nloss.csv')
N_loss<-N_loss[N_loss$Species==species, ]
N_loss$N_loss<-N_loss$N_loss/100
N_lossmean <- mean(N_loss$N_loss, na.rm=TRUE)


#Indirect (from ammonia)
N_vol_herd <- sum(Nex*mms_lagoon_perc/100 * N_loss$N_loss[ N_loss$MMS=="UncoveredAnerobic"] , 
                    Nex*mms_liquidslurry_perc/100 * N_loss$N_loss[N_loss$MMS=="Liquid"],
                    Nex*mms_solidstorage_perc/100 * N_loss$N_loss[ N_loss$MMS=="SolidStorage"],
                    Nex*mms_drylot_perc/100 * N_loss$N_loss[ N_loss$MMS=="DryLot"],
                    Nex*mms_dailyspread_perc/100 * N_loss$N_loss[N_loss$MMS=="DailySpread"],
                    Nex*mms_other_perc/100 * N_lossmean)



N2O_i_year_herd <-((N_vol_herd) *0.01)*(44/28)


#0.01 is the default EF for emission factor for N2O emissions from atmospheric deposition of nitrogen on soils and water surfaces
#44/28 density of N2O 
#assigning to cropland only

fun<-function(x){((N2O_i_year_herd+N2O_d_year_herd)/ncellcrop)*(x)} 
N2O_t_year_herd_map_c <- overlay(cropland,fun=fun )
fun<-function(x){((N20deposit_direct+N2Odeposit_indirect)/ncellgraz)*x} 
N2O_t_year_herd_map_g <- overlay(grazland,fun=fun )
fun<-function(x,y){x+y}
N2O_t_year_herd_map <- overlay(N2O_t_year_herd_map_c,N2O_t_year_herd_map_g,fun=fun )


#CO2 equ from manure 
fun <- function(x,y){(x*298)+(y*23)}
#298 conversion factor to Co2 equ for 100 year horizon (gwp) for N2O, and 23 for CH4
co2e_manure<-overlay(N2O_t_year_herd_map,manure_CH4_year_herd_tot,fun=fun)

##############################emissions from feed and fodder production  ########################

#ghg soil stock change
fun<-function(a,b){a*b}
Flu_c<-overlay(Flu_c,cropland, fun=fun) 
ssc<-read.csv('1-input/parameter/ghg_soil_stock_change.csv')

ssc$avgtil<-perc_til/100 * ssc$Full+perc_redtil/100 * ssc$Reduced + perc_notil/100* ssc$No.till
fun=function(a,b,c,d,e,f,g,h) {
  ((a*ssc[6,col])+(b*ssc[5,col])+(c*ssc[4,col])+(d*ssc[3,col])+(e*ssc[10,col])+(f*ssc[9,col])+(g*ssc[8,col])+(h*ssc[7,col]))
}
col=16
Fmg_c<-overlay(clim_wtmoist,clim_wtdry,clim_ctmoist,clim_ctdry,  clim_tr_mont,clim_tr_wet,clim_tr_moist,clim_tr_dry,fun=fun)
fun2<-function(x,y){x*y}
Fmg_c<-overlay(Fmg_c,cropland, fun=fun2) 
#plot(Fmg_c)

#create a layer from soil carbon stock change factor related to inputs (Fi)
ssc$input<-perc_inlow/100 * ssc$Low+perc_inmedium/100 * ssc$Medium + perc_inhighnoman/100* ssc$High.without.manure  + perc_inhighman/100*ssc$High.with.manure
col=17
Fi_c<-overlay(clim_wtmoist,clim_wtdry,clim_ctmoist,clim_ctdry,  clim_tr_mont,clim_tr_wet,clim_tr_moist,clim_tr_dry,fun=fun)
Fi_c<-overlay(Fi_c,cropland, fun=fun2) 
#plot(Fi_c)



#make a total Flu map, adjust here with the new land uses xxwhere necessary 



#fun3<-fun(x,y) {x+y}
#Flu_tot<-(Flu_c, Flu_rice, fun=fun3)
Flu_tot<-Flu_c 

fun4<-function(a,b,c,d){a*b*c*d}
SOCa<-overlay(soilref, Flu_tot, Fmg_c, Fi_c, fun=fun4)

fun<-function(x,y){x*y}
SOCr<-overlay(soilref, Flu_c,fun=fun)

#soil carbon balance on cropland
fun5=function(x,y){(x-y)*3.664*1/20}
scb<-overlay(SOCr,SOCa,fun=fun5)
scb<-overlay(scb,cropland, fun=fun)
#plot(scb)
#emissions from  land use change 
csluc<-raster(ext=cropland@extent, res=res(cropland),vals=0)
if (add>0){
  gfrs<-read.csv('1-input/parameter/GlobalForestResourcesAssessment2010.csv')
  gfrdom<-gfrs$Dead.organic.matter.forests[gfrs$Country==country] 
  fun3<-function(x,y) {(x*y)*(3.664*1/20)*gfrdom}
  rvc_forest<-overlay(forest_clim,lostforest, fun=fun3)
  fun3<-function(x,y) {x*y*3.664*0.05}
  rvc_gras<-overlay(graz_clim,lostgraz, fun=fun3)
  
  fun=function(x,y){x+y}
  csluc<-overlay(rvc_gras,rvc_forest,fun=fun )}




#############################################################################
# adding up the 4 indicators
fun=function(w,x,y,z){w+x+y+z}
COe<-overlay(rum_co2e_map,co2e_manure,scb,csluc,fun=fun)
#plot(COe)

#landscape level indicator
COe_l<-round(cellStats(COe,stat='sum'), 0)  # total CO2 producted 
co2e_manure_l<-round(cellStats(co2e_manure,stat='sum'),0) # total CO2 producted from manure 
co2cow<-round(COe_l/numcow,0)
co2milk<-round(COe_l/(milk),0)
rum_co2e_yeartot<-round(rum_co2e_yeartot,0)
Coe_ind<-data.frame(COe_l,co2e_manure_l,rum_co2e_yeartot,co2cow,co2milk)


#indifference computation 
Coe_ind_diff<-Coe_ind-Coe_base
Coe_ind_perc<-round(Coe_ind_diff/Coe_base*100, digits = 1)
Coe_ind2<-rbind(Coe_ind,Coe_ind_diff,Coe_ind_perc)
names(Coe_ind2)<-c("CO2 emmissions", "CO2 from manure", "CO2 from interic fermetation","CO2 per cow", "CO2 per tonnes of milk")
Coe_ind2<-data.frame(t(Coe_ind2))
colnames(Coe_ind2)<-c('result','difference','percent')

 ##steve edits
 Coe_ind_val<-read.csv('1-input/parameter/ghg_val.csv')
 Coe_ind2$evaluation <- ifelse(abs(Coe_ind2$percent) <= Coe_ind_val$val, 'low', ifelse((abs(Coe_ind2$percent) >= Coe_ind_val$val) & (abs(Coe_ind2$percent) <= 2*Coe_ind_val$val),'medium','high'))



Coe_map_diff<-round(COe-Coe_map,digit=4)



title1<-paste('GHG : CO2 equivalent', name, sep=" ")
title2<-paste('GHG : CO2 equ from manure only', name, sep=" ")
title3<-paste('GHG : CO2 difference from baseline', name, sep=" ")

col<-colorRampPalette(brewer.pal(9,'YlOrRd'))(100)
col2<-colorRampPalette((brewer.pal(9,'Reds')))(100)



par(mfrow=c(1,3),mar=c(2, 4.5, 2, 6))
plot(COe, legend.width=1, legend.shrink=0.45,col=col) 
plot(sarea, add=TRUE)
title(title1)

plot(co2e_manure, legend.width=1, legend.shrink=0.45,col=col)
plot(sarea, add=TRUE)
title(title2)

plot(Coe_map_diff, legend.width=1, legend.shrink=0.45,col=col2)
plot(sarea, add=TRUE)
title(title3)

#####################################################################
#extract the maps for final user
setwd("4-output")

title1<-paste('Greenhouse gas pathway : CO2 equivalent', name, sep=" ")
title2<-paste('Greenhouse gas pathway : CO2 equ from manure only', name, sep=" ")
title3<-paste('Greenhouse gas pathway : CO2 difference from baseline', name, sep=" ")

col<-colorRampPalette(brewer.pal(9,'YlOrRd'))(100)
col2<-colorRampPalette((brewer.pal(9,'Reds')))(100)

pdf(paste(name, "ghg_pathway.pdf", sep='-')) 
plot(COe, legend.width=1, legend.shrink=0.75,col=col) 
plot(sarea, add=TRUE)
title(title1)
plot(co2e_manure, legend.width=1, legend.shrink=0.75,col=col)
plot(sarea, add=TRUE)
title(title2)
plot(co2e_manure, legend.width=1, legend.shrink=0.75,col=col)
plot(sarea, add=TRUE)
title(title2)
plot(Coe_map_diff, legend.width=1, legend.shrink=0.75,col=col2)
plot(sarea, add=TRUE)
title(title3)
plot(NA, xlim=c(0,100), ylim=c(0,10), bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
grid.table(Coe_ind2)

dev.off()


writeRaster(COe,paste(name,"ghg_map.tif", sep="-"), overwrite=TRUE)
#writeRaster(co2e_manure,paste(name,"ghg_co2e_manure.tif", sep="-"),overwrite=TRUE)
write.csv(Coe_ind,paste(name,"ghg_ind.csv", sep="-"),row.names = F)


