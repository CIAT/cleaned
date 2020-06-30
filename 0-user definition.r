# cleaned version 2 
# initial code by Catherine Pfeifer, ILRI,

# clear environment
rm(list = ls(all = TRUE))

# set path to cleaned tool
path <- "D:/OneDrive - CGIAR/Dev/cleaned-XtRa"

# load data
source(paste0(path, "/src/2-load_data.r"))

# enter the number of animals per system
numcow_es <- 22000  # dual purpose dairy cow
numcow_sis <- 19000 # the dual puropse fattening and rearing
numcow_da <- 10000 # dual system : draft animals
numcow_is <- 500 # the specialized dairy with improved breeds
numsheep <- 1e+05 # sheep

preset <- 1  # if 1 the interface uses the presets if 0 or other it uses the manual input below. 
scenario <- read.csv(paste0(path, "/1-input/parameter/preset.csv"), skip = 2)

# select from the preset scenario
DD <- "DD1"  # options are (DD0, DD1,DD2)
DF <- "DF1"  # options are (DF0, DF1, DF2)
DA <- "DA1"  # options are (DA0, DA1, DA2)
SD <- "SD1"  # options are (SD0, SD1, SD2)
SH <- "SH1"  # options are (SH0, SH1 and SH2)
Cr <- "Cr1"  # options are (Cr0,Cr1)

# give your scanario a name
name <- "mybaserun"

############################# manual defintion of the parameters #############################

# seasonality climate seasonality # based on usaid livelihood zones
ds <- 8  # dry season
ws <- 12 - ds  # wet season  

############################# DEFINE DIFFERENT SYSTEMS #############################

############################# dual purpose dairy system #############################

lwes <- 150 # live weigtht in kg
myes <- 1100  # milk yield (kg/cow/year); 5 litre a day for 220 days 
des <- 0.3 # dressing percentage

# feed basket for dry season
efng1 <- 40 # natural grass in percent (51)
efrc1 <- 5 # crop residues cerals (49)
efrl1 <- 0 # crop residue legumes (0)
efpf1 <- 0 # planted fodder (0)
efconc1 <- 10 # concentrates cereal (maize bran) (0)
efconos1 <- 5 # concentrates oilseed cake (0)
efhay1 <- 40 # hay()
efsil1 <- 0 # silage made from grass
# check if a 100%

# feed basket wet season
efng2 <- 95 # wet season natural grass in percent (98)
efrc2 <- 5 # crop residues cerals (2)
efrl2 <- 0 # crop residue legumes (0)
efpf2 <- 0  # planted fodder (0)
efconc2 <- 0 # concentrates cereal (maize bran) (0)
efconos2 <- 0 # concentrates oilseed cake (0)
efhay2 <- 0 # hay
efsil2 <- 0 # silage made from grass
# check if a 100%

# manure management in the dual purpose dairy in percent (100%) (if ipcc=1, then no need to adjust this)
es_lagoon_perc <- 0
es_liquidslurry_perc <- 0
es_solidstorage_perc <- 45
es_drylot_perc <- 0
es_pasture_perc <- 0
es_dailyspread_perc <- 0
es_digester_perc <- 0
es_fuel_perc <- 55
es_other_perc <- 0
# check if a 100%

############################# dual system fattening and rearing #############################

lwsis <- 150 # live weigtht in kg
mysis <- 0 # milk yield (kg/cow/year)
dsis <- 0.5 # dressing percentage

# feed basket dry season 
sfng1 <- 40 # natural grass in percent
sfrc1 <- 5 # crop residues cereals (35)
sfrr1 <- 0 # crop residue from rice
sfrl1 <- 0 # crop residue legumes (12)
sfpf1 <- 0 # planted fodder (10)
sfconc1 <- 10 # concentrates cereal (maize bran) (5)
sfconos1 <- 5 # concentrates oilseed cake (5)
sfhay1 <- 40 # hay
sfsil1 <- 0 # silage made from grass
# check if a 100%

# feed basket wet season 
sfng2 <- 95 # natural grass in percent (57)
sfrc2 <- 5 # crop residues cereals (10)
sfrr2 <- 0 # crop residue from rice (0)
sfrl2 <- 0 # crop residue legumes (5)
sfpf2 <- 0 # planted fodder (14)
sfconc2 <- 0 # concentrates cereal (maize bran) (5)
sfconos2 <- 0 # concentrates oilseed cake (9)
sfhay2 <- 0 # hay
sfsil2 <- 0 # silage made from grass
# check if a 100%

# manure management in the dual system fattening and rearing system in percent (100%) (if ipcc=1, then no need to adjust this )
sis_lagoon_perc <- 0
sis_liquidslurry_perc <- 0
sis_solidstorage_perc <- 70
sis_drylot_perc <- 0
sis_pasture_perc <- 0
sis_dailyspread_perc <- 0
sis_digester_perc <- 0
sis_fuel_perc <- 30
sis_other_perc <- 0
# check if a 100%

############################# draft system ############################# 

lwda <- 150 # live weigtht in kg 
myda <- 0 # milk yield (kg/cow/year) 
dda <- 0.4 # dressing percentage

# feed basket dry season 
dafng1 <- 40 # natural grass in percent (15)
dafrc1 <- 10 # crop residues cerals (40)
dafrr1 <- 0 # crop residue from rice
dafrl1 <- 0 # crop residue legumes (30)
dafpf1 <- 0 # planted fodder ()
dafconc1 <- 5 # concentrates cereal (maize bran) (5)
dafconos1 <- 5 # concentrates oilseed cake (10)
dafhay1 <- 40 # hay
dafsil1 <- 0 # silage made from grass
# check if a 100%

# feed basket for wet season 
dafng2 <- 95 # natural grass in percent (60)
dafrc2 <- 5 # crop residues cerals (0)
dafrr2 <- 0 # crop residue from rice
dafrl2 <- 0 # crop residue legumes (10)
dafpf2 <- 0 # planted fodder (10)
dafconc2 <- 0 # concentrates cereal (maize bran) (20)
dafconos2 <- 0 # concentrates oilseed cake (10)
dafhay2 <- 0 # hay
dafsil2 <- 0 # silage made
# check if a 100%

# manure management in the draft system in percent (100%) (if ipcc=1, then no need to adjust this )
da_lagoon_perc <- 0
da_liquidslurry_perc <- 0
da_solidstorage_perc <- 45
da_drylot_perc <- 0
da_pasture_perc <- 0
da_dailyspread_perc <- 0
da_digester_perc <- 0
da_fuel_perc <- 55
da_other_perc <- 0
# check if a 100%

############################# specialized dairy system #############################

lwis <- 210 # live weigtht in kg for the the breed
myis <- 2200 # milk yield (kg/cow/year)
dis <- 0 # dressing percentage

# feed basket for dry season 
ifng1 <- 0 # natural grass in percent 
ifrc1 <- 10 # crop residues cerals (30)
ifrr1 <- 0 # crop residue from rice (0)
ifrl1 <- 0 # crop residue legumes (10)
ifpf1 <- 0 # planted fodder (30)
ifconc1 <- 25 # concentrates cereal (maize bran) (10)
ifconos1 <- 25 # concentrates oilseed cake (10)
ifhay1 <- 40 # hay
ifsil1 <- 0 # silage made from grass
# check if a 100%

# feed basket for wet season
ifng2 <- 20 # natural grass in percent (30)
ifrc2 <- 0 # crop residues cerals (10)
ifrr2 <- 0 # crop residue from rice (0)
ifrl2 <- 0 # crop residue legumes (5)
ifpf2 <- 50 # planted fodder (35)
ifconc2 <- 20 # concentrates cereal (maize bran) (10)
ifconos2 <- 10 # concentrates oilseed cake (10)
ifhay2 <- 0 # hay
ifsil2 <- 0 # silage made from grass
# check if a 100%

# manure management in the specialized dairy system  in percent (100%) (if
# ipcc=1, then no need to adjust this )
is_lagoon_perc <- 0
is_liquidslurry_perc <- 0
is_solidstorage_perc <- 75
is_drylot_perc <- 0
is_pasture_perc <- 0
is_dailyspread_perc <- 0
is_digester_perc <- 0
is_fuel_perc <- 25
is_other_perc <- 0
# check if a 100%


############################# sheep system ############################# 
lwsh <- 25 # live weigtht in kg 
mysh <- 0 # milk yield (kg/sheep/year)
dsh <- 0.4 # dressing persentage

# feed basket for wet season 
shfng1 <- 45 # natural grass in percent (15)
shfrc1 <- 10 # crop residues cerals (40)
shfrr1 <- 0 # crop residue from rice
shfrl1 <- 0 # crop residue legumes (30)
shfpf1 <- 0 # planted fodder ()
shfconc1 <- 5 # concentrates cereal (maize bran) (5)
shfconos1 <- 5 # concentrates oilseed cake (10)
shhay1 <- 35 # hay
shsil2 <- 0 # silage made from grass
# check if a 100%


# feed basket for wet season 
shfng2 <- 95 # natural grass in percent (60)
shfrc2 <- 5 # crop residues cerals (0)
shfrr2 <- 0 # crop residue from rice
shfrl2 <- 0 # crop residue legumes (10)
shfpf2 <- 0 # planted fodder (10)
shfconc2 <- 0 # concentrates cereal (maize bran) (20)
shfconos2 <- 0 # concentrates oilseed cake (10)
shhay2 <- 0 # hay()
shsil2 <- 0 # silage made
sh_pasture_perc <- 50
# check if a 100%

# manure management in the sheep system  in percent (100%) (if
# ipcc=1, then no need to adjust this )
sh_lagoon_perc <- 0
sh_liquidslurry_perc <- 0
sh_solidstorage_perc <- 40
sh_drylot_perc <- 0
sh_pasture_perc <- 40
sh_dailyspread_perc <- 18
sh_digester_perc <- 0
sh_fuel_perc <- 2
sh_other_perc <- 0
# check if a 100%

############################# GLOBAL VARIABLE DEFINITION #############################

# ipcc= 1 the code will use ipcc tier 2 standards for manure storage in
# stead of the user defined one
ipcc <- 0

# exogenous yield productivity gain in percentage of yield crop
pgc <- 0 # cereals
pgl <- 0 # legumes
pgpf <- 0 # planted fodder
pgg <- 0 # grassland

# linking the manure availability to the production system
mprod_es <- 2  # manure production from a cow in the dual system - rearing/fattening and draf per day
mprod_da <- 2  # manure production from a cow in the dual system - rearing/fattening and draf per day
mprod_sis <- 3  # manure production from a cow from in the dual system lactating animals per day
mprod_is <- 4  # manure production from a cow in the specialized dairy per day
mprod_sh <- 0.1 # manure production from a sheep system per day

# percent of stored manure applied to the different 
manc <- 0.8 # crop cereal (mprod_c *% to this crop for linking with production )
manl <- 0 # legumes (mprod_c *% to this crop for linking with production )
manpf <- 0 # planted fodder (mprod_c *% to this crop for linking with production )
manr <- 0 # rice (mprod_r *% to this crop for linking with production )
mangraz <- 0 # grazing land (mprod_r *% to this crop for linking with production )

# application of slurry kg/ha 
sluc <- 0 # cereal ()
slul <- 0 # legumes (0)
slupf <- 0 # planted fodder ()
slugraz <- 0 # grazing land
slur <- 0 # rice land

slurryconv <- 0.001  #conversion rate between slurry (NPK) and Nitrogen
# we need a source here What about compost and other manure.

# inorganic fertilizer application in kg per hectare
fertc <- 0 # cereal (50 is recommended)
fertr <- 0 # rice (50 is recommended)
fertl <- 0 # legumes (0)
fertpf <- 0 # planted fodder
fertgraz <- 0 # grazing land
fertconv <- 0.2  #conversion rate between fertilizer (NPK) and Nitrogen, depends on the locally available ferilizer, +/- 20%
# from impact lit we know that DAP is most commonly used - Joanne is
# looking for conversion rates

# exogenous yield productivity gain in percentage of yield crop
pgc <- 0 # cereal ()
pgl <- 0 # legumes
pgpf <- 0 # planted fodder
pgg <- 0 # grassland
pgr <- 0 # rice

# soil management option on cropland (ghg)
perc_til <- 0  # percentage of cropland that is tilled 
perc_redtil <- 100  # percentage of cropland that is on reduced till
perc_notil <- 0  # percentage of cropland that is on no till

perc_inlow <- 100  # percentage of land with low input 
perc_inmedium <- 0  # percentage of land with medium input 
perc_inhighnoman <- 0  # percentage of land with high input no manure 
perc_inhighman <- 0  # percentage of land with high input with manure

# reading some r info
pixel <- read.csv(paste0(path, "/1-input/parameter/pixel.csv"))
pixel <- pixel[2]

############################# LAND USE DRIVEN SCENARIOS #############################

# run the feed basket
source(paste0(path, "/src/", "2-feedbasket_nonlinear.r"))

############################# run CLEANED ############################# 

# run the water pathway
source(paste0(path, "/src/", "1-water.r"))

# run the greenhouse gas pathway
source(paste0(path, "/src/", "1-ghg.r"))

# run the biodiversity pathway
source(paste0(path, "/src/", "1-biodiv.r")) 

# run the soil pathway
source(paste0(path, "/src/", "1-soil.r"))

# ouput maps can be found in the 4- ouput map folder