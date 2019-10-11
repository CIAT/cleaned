######################################CLEANED Tanzania SAIRLA################################################

# cleaned version 2
# code by Catherine Pfeifer, ILRI, c.pfeifer@cgiar.org
#code created on 20.6.2016
# last modified 22.1.2018
#clearing all memory 
rm(list=ls(all=TRUE))
#set path to cleaned tool
path<-'D:/Dropbox/Cleaned Tanzania'

setwd(path)
###################################This sheet defines all user defined variables#################

source('3-cleaned/2-load_data.r')
setwd(path)

# enter the number of animals per system 
#the extensive system
numcow_es<-25000 # see parameterization excel file 
# the semi-intensive - cross breeds
numcow_sis <- 12500

# the intensive system - pure breeds
numcow_is <-0
  

preset <-1 #if 1 the interface uses the presets if 0 or other it uses the manual input below. 
scenario<-read.csv('1-input/parameter/preset.csv',  skip=2)

# select from the preset scenario

ES<- 'E1'  #options are (E0, E1,E2)
SI<- 'SI1'  #options are (SI0, SI1, SI2)
IS <- 'I1'  #options are (I0, I1, I2)

Cr<- 'C1' #options are (C0,C1)

#give your scanario a name 
name<-"baserun"

#############################manual defintion of the parameters#####################


#seasonality 
# climate seasonality 
ds<- 4 # dry season
ws<- 12-ds # wet season  

#DEFINE THE DIFFERENT SYSTEM 
#################################

# the extensive system
#define liveweigtht in kg for the the breed in the extensive system (200)
lwes=200 
#define milk yield (kg/cow/year) for the breed in the extensive system (400) #500 for health
myes=1000

#dry season 
#natural grass in percent (51)
efng1<- 0
#crop residues cerals (49)
efrc1<- 0
#crop residue legumes (0) 
efrl1<-0
#planted fodder (0)
efpf1<-0
# concentrates cereal (maize bran) (0)
efconc1<- 0
# concentrates oilseed cake (0)
efconos1<- 0
#hey()
efhay1<-0
# silage made from grass
efsil1<-0

#wet season  
#natural grass in percent (98)
efng2<- 0  
#crop residues cerals (2)
efrc2<- 0 
#crop residue legumes (0) 
efrl2<-0
#planted fodder (0)
efpf2<-0  #
# concentrates cereal (maize bran) (0)
efconc2<- 0
# concentrates oilseed cake (0)
efconos2<- 0
#hey()
efhay2<-0
# silage made from grass
efsil2<-0
#check if a 100%


# manure management in the extensive system  system in percent (100%)
#(if ipcc=1, then no need to adjust this )
es_lagoon_perc<- 00
es_liquidslurry_perc<-00
es_solidstorage_perc<-00
es_drylot_perc<-00
es_pasture_perc<-100
es_dailyspread_perc<-00
es_digester_perc<-00
es_fuel_perc<-00
es_other_perc<-00

####################################### the semi-intensive system 

#define liveweigtht in kg for the the breed in the semi intensive system (220)
lwsis=220  
#define milk yield (kg/cow/year) for the breed in the semi intensive system (2000) 
mysis=2000

#feed basket  for semi intensive system 
#dressing percentage http://www.dpi.nsw.gov.au/__data/assets/pdf_file/0006/103992/dressing-percentages-for-cattle.pdf
dsis = 0

#feed basket  semi-intensive  system season dry
#natural grass in percent (33)
sfng1<-0
#crop residues cereals (35)
sfrc1<-0
#crop residue from rice
sfrr1<- 0
#crop residue legumes (12)
sfrl1<-0
#planted fodder (10)
sfpf1<-0
# concentrates cereal (maize bran) (5)
sfconc1<- 0
# concentrates oilseed cake (5)
sfconos1<- 0
#hey()
sfhay1<-0
# silage made from grass
sfsil1<-0



# wet season 
#natural grass in percent (57)
sfng2<-0
#crop residues cereals (10)
sfrc2<-0
#crop residue from rice (0)
sfrr2<-0
#crop residue legumes (5) 
sfrl2<-0
#planted fodder (14)
sfpf2<-0
# concentrates cereal (maize bran) (5)
sfconc2<- 0
# concentrates oilseed cake (9)
sfconos2<- 0
#hey()
sfhay2<-0
# silage made from grass
sfsil2<-0

# manure management in the semi-intensive system in percent (100%)
#(if ipcc=1, then no need to adjust this )
sis_lagoon_perc<- 00
sis_liquidslurry_perc<-00
sis_solidstorage_perc<-00
sis_drylot_perc<-00
sis_pasture_perc<-00
sis_dailyspread_perc<-80
sis_digester_perc<-00
sis_fuel_perc<-00
sis_other_perc<-20


################################################### intensive system 
#define liveweigtht in kg for the the breed in the intensive system (300)
lwis=250

#define milk yield (kg/cow/year) for the breed in the intensive system (7500)
myis=2500

#feed basket for intensive system
#dry season
#natural grass in percent (20)
ifng1<-0
#crop residues cerals (30)
ifrc1<-0
#crop residue from rice (0)
ifrr1<-0
#crop residue legumes (10)
ifrl1<-0
#planted fodder (30)
ifpf1<-0
# concentrates cereal (maize bran) (10)
ifconc1<- 0
# concentrates oilseed cake (10)
ifconos1<- 0
#hey()
ifhay1<-0
# silage made from grass
ifsil1<-0



#feed basket for dairy wet season
#natural grass in percent (30)
ifng2<-0
#crop residues cerals (10)
ifrc2<-0
#crop residue from rice (0)
ifrr2<-  0
#crop residue legumes (5)
ifrl2<-0
#planted fodder (35)
ifpf2<-0
# concentrates cereal (maize bran) (10)
ifconc2<- 0
# concentrates oilseed cake (10)
ifconos2<- 0
#hey()
ifhay2<-0
# silage made from grass
ifsil2<-0

# manure management in the semi-intensive system in percent (100%)
#(if ipcc=1, then no need to adjust this )
is_lagoon_perc<- 00
is_liquidslurry_perc<-00
is_solidstorage_perc<-00
is_drylot_perc<-00
is_pasture_perc<-00
is_dailyspread_perc<-90
is_digester_perc<-5
is_fuel_perc<-00
is_other_perc<-5


#######################################################################################
#global variable definition

#ipcc= 1 the code will use ipcc tier 2 standards for manure storage in stead of the user defined one
ipcc=0


#exogenous yield productivity gain in percentage of yield
#crop
pgc= 0.0
#legumes
pgl=0.0
#planted fodder
pgpf=0.0
#grassland
pgg= 0.0



#linking the manure availability to the production system 
mprod_es<- 2 #manure production from a cow in the traditional system per day
mprod_sis<- 3 #manure production from a cow from in the troupeau laitier per day
mprod_is<- 3 #manure production from a cow in the fattening system per day


#percent of stored manure applied to the different crop
#cereal (mprod_c *% to this crop for linking with production )
manc<-0.4
# legumes  (mprod_c *% to this crop for linking with production )
manl<-0
#planted fodder  (mprod_c *% to this crop for linking with production )
manpf<- 0
#rice  (mprod_r *% to this crop for linking with production )
manr<- 0.4 
#grazing land  (mprod_r *% to this crop for linking with production )
mangraz<-0 

# application of slurry kg/ha
#cereal ()
sluc<-0
# legumes (0)
slul<-0
#planted fodder ()
slupf<-0
#grazing land
slugraz<-0
#rice land
slur<-0


slurryconv<- 0.001 #conversion rate between slurry (NPK) and Nitrogen
#we need a source here What about compost and other manure. 

#inorganic fertilizer application in kg per hectare

#cereal (50 is recommended)
fertc<- 0
#rice (50 is recommended)
fertr<- 0
# legumes (0)
fertl<- 0
#planted fodder 
fertpf<- 0
#grazing land
fertgraz<- 0

Fertconv<- 0.2 #conversion rate between fertilizer (NPK) and Nitrogen, depends on the locally available ferilizer, +/- 20%
# from impact lit we know that DAP is most commonly used - Joanne is looking for conversion rates


#exogenous yield productivity gain in percentage of yield
#crop
pgc= 0.0
#legumes
pgl=0.0
#planted fodder
pgpf=0.0
#grassland
pgg= 0.0

# rice 
pgr= 0.0

#############soil management option on cropland (ghg)
perc_til= 0 #percentage of cropland that is tilled 
perc_redtil = 100 #percentage of cropland that is on reduced till
perc_notil = 0 #percentage of cropland that is on no till

perc_inlow = 100  #percentage of land with low input 
perc_inmedium = 0  #percentage of land with medium input 
perc_inhighnoman =0  #percentage of land with high input no manure 
perc_inhighman =0   #percentage of land with high input with manure


#####reading some r info
setwd(path)
pixel<-read.csv('1-input/parameter/pixel.csv')
pixel<-pixel[2]

##############################land use driven scenarios
library(raster)

#do we run a land use change driven scenario? then we need to run the land use change module first 
# and read here the file indicating the pixels that have changed


add<-10 # in percent, will be devided by 100 in the luc code

# sourcing the land use change code now happens in the feed basket 

setwd(path)
# #now overwrite variables with preset
# if(preset==1){
#   # get the parameter of the pastoral extensive system
#   temp<-scenario[ ,c('Evar', ES)]
#   names(temp)[2]<-'ppara'
#   for(i in  which(! temp$Evar== ''))
#   {
#     assign(as.character(temp$Evar[i]), temp$ppara[i])
#   }}
# 
# if(preset==1){  
#   # get the parameter of the pastoral dairy system
#   temp<-scenario[ , c('Svar', SI )]
#   names(temp)[2]<-'ppara'
#   for(i in  which(! temp$Svar== ''))
#   {
#     assign(as.character(temp$Svar[i]), temp$ppara[i])
#   }}
# 
# if(preset==1){
#   # get the parameter of the  dairy system
#   temp<-scenario[ , c('Ivar', IS )]
#   names(temp)[2]<-'ppara'
#   for(i in  which(! temp$Ivar== ''))
#   {
#     assign(as.character(temp$Ivar[i]), temp$ppara[i])
#   }}
# 
# 
# if(preset==1){
#   # get the parameter of the cropping system
#   temp<-scenario[ , c('Cvar',Cr)]
#   names(temp)[2]<-'ppara'
#   for(i in  which(! temp$Cvar== ''))
#   {
#     assign(as.character(temp$Cvar[i]), temp$ppara[i])
#   }}
# 
# # end of else loop
# 


start<- Sys.time()



source('3-cleaned/2-feedbasket_nonlinear2.r')


#########################################run CLEANED##################################


#run the water pathway 
setwd(path)
source('3-cleaned/1-water.r')
#run the greenhouse gas pathway
setwd(path)  
source('3-cleaned/1-ghg.r')
#run the biodiversity pathway
setwd(path)
source('3-cleaned/1-biodiv.r')
# run the soil pathway
setwd(path)
source('3-cleaned/1-soil.r')


#ouput maps can be found in the 4- ouput map folder 
end<-Sys.time()

end-start