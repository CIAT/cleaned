######################### greenhouse gases pathway ############## by Catherine Pfeifer
######################### 21.6.2016 last modified 22.1.2018

# read libraries
library(pacman)
pacman::p_load(raster, maptools, RColorBrewer, gridExtra)

# read data
Coe_base <- read.csv(paste0(path, "/1-input/base_run/ghg_ind.csv", row.names = NULL))
# Coe_base<-Coe_base[ ,2:7]
Coe_map <- raster(paste0(path, "/1-input/base_run/ghg_map.tif"))

############################ enteric fermetation ############################

# livestock distribution

##### calculating percentage of DM total
dmi_ct <- ((fw_g_ct * dm_g) + (fw_rc_ct * dm_rc) + (fw_rl_ct * dm_rl) + 
             (fw_pf_ct * dm_pf) + (fw_conc_ct * dm_conc) + (fw_conos_ct * dm_conos) + 
             (fwg_hay_ct * dm_h) + (fwg_sil_ct * dm_s))
perc_dm_g_ct <- (fw_g_ct * dm_g)/dmi_ct
perc_dm_rc_ct <- (fw_rc_ct * dm_rc)/dmi_ct
perc_dm_rl_ct <- (fw_rl_ct * dm_rl)/dmi_ct
perc_dm_pf_ct <- (fw_pf_ct * dm_pf)/dmi_ct
perc_dm_conc_ct <- (fw_conc_ct * dm_conc)/dmi_ct
perc_dm_conos_ct <- (fw_conos_ct * dm_conos)/dmi_ct
perc_dm_sil_ct <- (fwg_hay_ct * dm_h)/dmi_ct
perc_dm_h_ct <- (fwg_sil_ct * dm_s)/dmi_ct

dmi_sh <- ((fw_g_sh * dm_g) + (fw_rc_sh * dm_rc) + (fw_rl_sh * dm_rl) + 
             (fw_pf_sh * dm_pf) + (fw_conc_sh * dm_conc) + (fw_conos_sh * dm_conos) + 
             (fwg_hay_sh * dm_h) + (fwg_sil_sh * dm_s))
perc_dm_g_sh <- (fw_g_sh * dm_g)/dmi_sh
perc_dm_rc_sh <- (fw_rc_sh * dm_rc)/dmi_sh
perc_dm_rl_sh <- (fw_rl_sh * dm_rl)/dmi_sh
perc_dm_pf_sh <- (fw_pf_sh * dm_pf)/dmi_sh
perc_dm_conc_sh <- (fw_conc_sh * dm_conc)/dmi_sh
perc_dm_conos_sh <- (fw_conos_sh * dm_conos)/dmi_sh
perc_dm_sil_sh <- (fwg_hay_sh * dm_h)/dmi_sh
perc_dm_h_sh <- (fwg_sil_sh * dm_s)/dmi_sh

# this computation if for dairy system as proposed by Gerber (2011)
# calculating LCIDE and gama m (ym) Average digestibilities assumed to
# be: Pasture 0.66, crop residue 0.526, fodder 0.567, Concentrate 0.8
lcide_ct <- (perc_dm_g_ct * d_g) + (perc_dm_rc_ct * d_rc) + (perc_dm_rl_ct * 
                                                               d_rl) + (perc_dm_pf_ct * d_pf) + (perc_dm_conc_ct * d_conc) + (perc_dm_conos_ct * 
                                                                                                                                d_conos) + (perc_dm_sil_ct * d_s) + (perc_dm_h_ct * d_h)
# methane conversion factor, constants are taken from (tier 2
# estimation) Gerber et al 2011
ym_ct <- (9.75 - 0.05 * lcide_ct)  # SF report page 11 
# gross energy intake, constant is default value IPCC, IPCC guideline p
# 10.21

# Average digestibilities
lcide_sh <- (perc_dm_g_sh * d_g) + (perc_dm_rc_sh * d_rc) + (perc_dm_rl_sh * 
                                                               d_rl) + (perc_dm_pf_sh * d_pf) + (perc_dm_conc_sh * d_conc) + (perc_dm_conos_sh * 
                                                                                                                                d_conos) + (perc_dm_sil_sh * d_s) + (perc_dm_h_sh * d_h)
# methane conversion factor, constants are taken from (tier 2
# estimation) Gerber et al 2011
ym_sh <- (9.75 - 0.05 * lcide_sh)  # SF report page 11 
# gross energy intake, constant is default value IPCC, IPCC guideline p
# 10.21

# ge <- (dmi*18.45)
ge_ct <- gerc_ct/numcow  #gerc is at landscape scale daily for cattle
ge_sh <- gerc_sh/numsheep  #gerc is at landscape scale daily for cattle

# we now compute the full energy requirement equations so we do not
# rely on ipcc shortcut average anymore.  when digestibility increases
# then entiric fermentation decreases with our base run feed basket, on
# digestibility is lower than ipcc standards

# emissions (per animal) per day
rum_ch4_day_ct <- (ge_ct * (ym_ct/100))/55.65
rum_ch4_day_sh <- (ge_sh * (ym_sh/100))/55.65

# IPCC default value IPCC guideline 10.31
rum_ch4_year_ct <- rum_ch4_day_ct * 365
rum_ch4_year_sh <- rum_ch4_day_sh * 365

# computation of the CO2 equivalent IPCC guideline, 100 years
rum_co2e_yeartot_ct <- rum_ch4_year_ct * 25 * numcow
rum_co2e_yeartot_sh <- rum_ch4_year_sh * 25 * numsheep
rum_co2e_yeartot <- rum_co2e_yeartot_ct + rum_co2e_yeartot_sh

# spatially allocation of the impact

sumlivdist <- cellStats(livdist, stat = "sum", na.rm = TRUE)
fun <- function(x) {
  x * rum_co2e_yeartot_ct/sumlivdist
}
rum_co2e_map_ct <- overlay(livdist, fun = fun)
fun <- function(x) {
  x * rum_co2e_yeartot_sh/sumlivdist
}
rum_co2e_map_sh <- overlay(livdist, fun = fun)
rum_co2e_map <- rum_co2e_map_ct + rum_co2e_map_sh

############################ emissions from manure management ############################

# volatile solids ash = mineral content, this is an IPCC value (2006-)
# but can be customized ge is per cow
vs_ct <- (ge_ct * (1 - lcide_ct) + (0.04 * ge_ct)) * ((1 - ash)/18.45)  #IPCC 10.24
vs_sh <- (ge_sh * (1 - lcide_sh) + (0.04 * ge_sh)) * ((1 - ash)/18.45)  #IPCC 10.24

# 0.04=urinary energy default vs <- ((dmi) * (1.04 - lcide))*(1-ash)
# #LEAP guidelines gives the same answer

# the percentage comuptation are now it in the feedbasket

# differentiate between pasture = grazing land, solid storage, slurry
# and daily spread= cropland, other =uniform) this is for dairy

fun <- function(a, b, c, d, f, g, h, i) {
  vs_ct * 365 * B0 * 0.67 * ((a/100 * mms_lagoon_perc/100) + (b/100 * 
                                                                mms_liquidslurry_perc/100) + (c/100 * mms_solidstorage_perc/100) + 
                               (d/100 * mms_drylot_perc/100) + (f/100 * mms_dailyspread_perc/100) + 
                               (g/100 * mms_digester_perc/100) + (h/100 * mms_fuel_perc/100) + 
                               (i/100 * mms_other_perc/100))
}

efc1 <- overlay(mms_lagoon_mcf, mms_liquidslurry_mcf, mms_solidstorage_mcf, 
                mms_drylot_mcf, mms_dailyspread_mcf, mms_digester_mcf, mms_burned_mcf, 
                mms_other_mcf, fun = fun)


fun1 <- function(a, b) {
  a * (numcow_es + numcow_is)/ncellcrop * b
}
manure_CH4_year_herd_c1 <- overlay(efc1, cropland, fun = fun1)

# we assume that the proportion of cows on pastures is equivalent of
# the manure on pastures, so here we have all the others, so 1-percent
# pasture CH4 estimates per cow
fun <- function(e, d) {
  (vs_ct * 365 * B0 * 0.67 * (e/100 * mms_pasture_perc/100)) * d
}
efg1 <- overlay(mms_pasture_mcf, grazland, fun = fun)

# spatial allocation
fun <- function(a, c) {
  a * (numcow_es + numcow_is)/ncellgraz * c
}
manure_CH4_year_herd_g1 <- overlay(efg1, grazland, fun = fun)

# total
fun <- function(x, y) {
  x + y
}
manure_CH4_year_herd_tot1 <- overlay(manure_CH4_year_herd_g1, manure_CH4_year_herd_c1, 
                                     fun = fun)
# plot(manure_CH4_year_herd_tot)

# other cattle
fun <- function(a, b, c, d, f, g, h, i) {
  vs_ct * 365 * B02 * 0.67 * ((a/100 * mms_lagoon_perc2/100) + (b/100 * 
                                                                  mms_liquidslurry_perc2/100) + (c/100 * mms_solidstorage_perc2/100) + 
                                (d/100 * mms_drylot_perc2/100) + (f/100 * mms_dailyspread_perc2/100) + 
                                (g/100 * mms_digester_perc2/100) + (h/100 * mms_fuel_perc2/100) + 
                                (i/100 * mms_other_perc2/100))
}
efc2 <- overlay(mms_lagoon_mcf, mms_liquidslurry_mcf, mms_solidstorage_mcf, 
                mms_drylot_mcf, mms_dailyspread_mcf, mms_digester_mcf, mms_burned_mcf, 
                mms_other_mcf, fun = fun)


fun1 <- function(a, b) {
  a * (numcow_sis + numcow_da)/ncellcrop * b
}
manure_CH4_year_herd_c2 <- overlay(efc2, cropland, fun = fun1)


# sheep
fun <- function(a) {
  vs_sh * 365 * B03 * 0.67 * a
}
efsh <- overlay(mcf_sh, fun = fun)

fun1 <- function(a, b) {
  a * (numcow_sis + numcow_da)/ncellcrop * b
}
manure_CH4_year_herd_c2 <- overlay(efc2, cropland, fun = fun1)
fun <- function(e, d) {
  (vs_ct * 365 * B0 * 0.67 * (e/100 * mms_pasture_perc2/100)) * d
}
efg2 <- overlay(mms_pasture_mcf2, grazland, fun = fun)

# spatial allocation
fun <- function(a, c) {
  a * (numcow_sis + numcow_da)/ncellgraz * c
}
manure_CH4_year_herd_g2 <- overlay(efg2, grazland, fun = fun)
fun <- function(x, y) {
  x + y
}
manure_CH4_year_herd_tot2 <- overlay(manure_CH4_year_herd_g2, manure_CH4_year_herd_c2, 
                                     fun = fun)
# plot(manure_CH4_year_herd_tot)
manure_CH4_year_herd_sh <- efsh * (cropland + grazland)

# plot(manure_CH4_year_herd_tot)

## total
manure_CH4_year_herd_tot <- manure_CH4_year_herd_tot1 + manure_CH4_year_herd_tot2 + 
  manure_CH4_year_herd_sh
manure_CH4_year_herd_ct <- manure_CH4_year_herd_tot1 + manure_CH4_year_herd_tot2

# N2O emissions
NrateLook <- read.csv(paste0(path, "/1-input/parameter/MMS_Nrate.csv"))
MMS_N_EF <- read.csv(paste0(path, "/1-input/parameter/MMSn2oEF.csv"))
MMS_N_EFmean <- mean(MMS_N_EF$EF, na.rm = TRUE)

# for dairy
Nrate1 <- NrateLook$ExcretionRate[NrateLook$Species == species & NrateLook$Region == 
                                    region]
Nrate2 <- NrateLook$ExcretionRate[NrateLook$Species == species3 & NrateLook$Region == 
                                    region]
Nrate3 <- NrateLook$ExcretionRate[NrateLook$Species == species5 & NrateLook$Region == 
                                    region]

Nex1 <- Nrate1 * ((lwis * numcow_is + lwes * numcow_es)/1000) * 365  #for all dairy animals this is tier 1  equation 10.30
Nex2 <- Nrate2 * ((lwsis * numcow_sis + lwda * numcow_da)/1000) * 365  #for all dairy animals this is tier 1  equation 10.30
Nex3 <- Nrate3 * ((lwsh * numsheep)/1000) * 365  #for all dairy animals this is tier 1  equation 10.30

# Direct dairy
N2O_d_year_herd1 <- sum(Nex1 * mms_lagoon_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS == 
                                                                   "UncoveredAnerobic"], Nex1 * mms_liquidslurry_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS == 
                                                                                                                                          "Liquid"], Nex1 * mms_solidstorage_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS == 
                                                                                                                                                                                                      "SolidStorage"], Nex1 * mms_drylot_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS == 
                                                                                                                                                                                                                                                                  "DryLot"], Nex1 * mms_dailyspread_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS == 
                                                                                                                                                                                                                                                                                                                             "DailySpread"], Nex1 * mms_digester_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS == 
                                                                                                                                                                                                                                                                                                                                                                                          "AnerobicDigester"], Nex1 * mms_other_perc/100 * MMS_N_EFmean) * (44/28)
# 44/28 = conversion of (N2O-N)(mm) emissions to N2O(mm) emissions

# other cattle
N2O_d_year_herd2 <- sum(Nex2 * mms_lagoon_perc2/100 * MMS_N_EF$EF[MMS_N_EF$MMS == 
                                                                    "UncoveredAnerobic"], Nex2 * mms_liquidslurry_perc2/100 * MMS_N_EF$EF[MMS_N_EF$MMS == 
                                                                                                                                            "Liquid"], Nex2 * mms_solidstorage_perc2/100 * MMS_N_EF$EF[MMS_N_EF$MMS == 
                                                                                                                                                                                                         "SolidStorage"], Nex2 * mms_drylot_perc2/100 * MMS_N_EF$EF[MMS_N_EF$MMS == 
                                                                                                                                                                                                                                                                      "DryLot"], Nex2 * mms_dailyspread_perc2/100 * MMS_N_EF$EF[MMS_N_EF$MMS == 
                                                                                                                                                                                                                                                                                                                                  "DailySpread"], Nex2 * mms_digester_perc2/100 * MMS_N_EF$EF[MMS_N_EF$MMS == 
                                                                                                                                                                                                                                                                                                                                                                                                "AnerobicDigester"], Nex2 * mms_other_perc2/100 * MMS_N_EFmean) * (44/28)

# sheep
N2O_d_year_herd3 <- sum(Nex3 * (100 - mms_pasture_perc3)/100 * MMS_N_EF$EF[MMS_N_EF$MMS == 
                                                                             "SolidStorage"]) * (44/28)

# Pasture (grazed and deposited) calculated is now calculted here
# Direct and indirect N2O on pastures
Ndeposit1 <- (Nex1) * (mms_pasture_perc/100)  #Equation 11.5 IPCC
Ndeposit2 <- (Nex2) * (mms_pasture_perc2/100)  #Equation 11.5 IPCC
Ndeposit3 <- (Nex3) * (mms_pasture_perc3/100)  #Equation 11.5 IPCC

EF_CPP <- 0.02  #Default emission factor for cattle, pigs and poultry
EF_SH <- 0.01  #Default emission factor for sheep and others
N20deposit_direct1 <- ((Ndeposit1 + Ndeposit2) * EF_CPP) * (44/28)  #Equation 11.1 IPCC
N20deposit_direct2 <- (Ndeposit3 * EF_SH) * (44/28)

N20deposit_direct <- N20deposit_direct1 + N20deposit_direct2

FracGASM <- 0.2  #Default value for fraction of applied organic N that volatilises as NH3 and NOx
EF_indirectN2Opasture <- 0.01  # Default emission factor
N2Odeposit_indirect_ct <- ((Ndeposit1 + Ndeposit2) * FracGASM * EF_indirectN2Opasture) * 
  (44/28)  #Equation 11.9 IPCC
N2Odeposit_indirect_sh <- (Ndeposit3 * FracGASM * EF_indirectN2Opasture) * 
  (44/28)  #Equation 11.9 IPCC
N2Odeposit_indirect <- N2Odeposit_indirect_ct + N2Odeposit_indirect_sh

N_loss <- read.csv(paste0(path, "/1-input/parameter/MMS_Nloss.csv"))
N_loss1 <- N_loss[N_loss$Species == species, ]
N_loss1$N_loss <- N_loss1$N_loss/100
N_lossmean1 <- mean(N_loss1$N_loss, na.rm = TRUE)
N_loss2 <- N_loss[N_loss$Species == species4, ]
N_loss2$N_loss <- N_loss2$N_loss/100
N_lossmean2 <- mean(N_loss2$N_loss, na.rm = TRUE)
N_loss3 <- N_loss[N_loss$Species == "Other", ]
N_loss3$N_loss <- N_loss3$N_loss/100
N_lossmean3 <- mean(N_loss3$N_loss, na.rm = TRUE)
# Indirect (from ammonia)
N_vol_herd1 <- sum(Nex1 * mms_lagoon_perc/100 * N_loss1$N_loss[N_loss1$MMS == 
                                                                 "UncoveredAnerobic"], Nex1 * mms_liquidslurry_perc/100 * N_loss1$N_loss[N_loss1$MMS == 
                                                                                                                                           "Liquid"], Nex1 * mms_solidstorage_perc/100 * N_loss1$N_loss[N_loss1$MMS == 
                                                                                                                                                                                                          "SolidStorage"], Nex1 * mms_drylot_perc/100 * N_loss$N_loss1[N_loss1$MMS == 
                                                                                                                                                                                                                                                                         "DryLot"], Nex1 * mms_dailyspread_perc/100 * N_loss1$N_loss[N_loss1$MMS == 
                                                                                                                                                                                                                                                                                                                                       "DailySpread"], Nex1 * mms_other_perc/100 * N_lossmean1)
N2O_i_year_herd1 <- ((N_vol_herd1) * 0.01) * (44/28)


# 0.01 is the default EF for emission factor for N2O emissions from
# atmospheric deposition of nitrogen on soils and water surfaces 44/28
# density of N2O assigning to cropland only

N_vol_herd2 <- sum(Nex2 * mms_lagoon_perc2/100 * N_loss2$N_loss[N_loss2$MMS == 
                                                                  "UncoveredAnerobic"], Nex2 * mms_liquidslurry_perc2/100 * N_loss2$N_loss[N_loss2$MMS == 
                                                                                                                                             "Liquid"], Nex2 * mms_solidstorage_perc2/100 * N_loss2$N_loss[N_loss2$MMS == 
                                                                                                                                                                                                             "SolidStorage"], Nex2 * mms_drylot_perc2/100 * N_loss2$N_loss[N_loss2$MMS == 
                                                                                                                                                                                                                                                                             "DryLot"], Nex2 * mms_dailyspread_perc2/100 * N_loss2$N_loss[N_loss2$MMS == 
                                                                                                                                                                                                                                                                                                                                            "DailySpread"], Nex2 * mms_other_perc2/100 * N_lossmean2)
N2O_i_year_herd2 <- ((N_vol_herd2) * 0.01) * (44/28)

N_vol_herd3 <- sum(Nex2 * (100 - mms_pasture_perc3)/100 * N_loss3$N_loss[N_loss3$MMS == 
                                                                           "SolidStorage"])
N2O_i_year_herd3 <- ((N_vol_herd3) * 0.01) * (44/28)

fun <- function(x) {
  ((N2O_i_year_herd1 + N2O_d_year_herd1 + N2O_i_year_herd2 + N2O_d_year_herd2)/ncellcrop) * 
    (x)
}
N2O_t_year_herd_map_c_ct <- overlay(cropland, fun = fun)
fun <- function(x) {
  ((N2O_i_year_herd3 + N2O_d_year_herd3)/ncellcrop) * (x)
}
N2O_t_year_herd_map_c_sh <- overlay(cropland, fun = fun)
N2O_t_year_herd_map_c <- N2O_t_year_herd_map_c_sh + N2O_t_year_herd_map_c_ct


fun <- function(x) {
  ((N20deposit_direct1 + N2Odeposit_indirect_ct)/ncellgraz) * x
}
N2O_t_year_herd_map_g_ct <- overlay(grazland, fun = fun)
fun <- function(x) {
  ((N20deposit_direct2 + N2Odeposit_indirect_sh)/ncellgraz) * x
}
N2O_t_year_herd_map_g_sh <- overlay(grazland, fun = fun)
N2O_t_year_herd_map_g <- N2O_t_year_herd_map_g_sh + N2O_t_year_herd_map_g_ct
fun <- function(x, y) {
  x + y
}
N2O_t_year_herd_map <- overlay(N2O_t_year_herd_map_c, N2O_t_year_herd_map_g, 
                               fun = fun)
N2O_t_year_herd_map_ct <- overlay(N2O_t_year_herd_map_c_ct, N2O_t_year_herd_map_g_ct, 
                                  fun = fun)
N2O_t_year_herd_map_sh <- overlay(N2O_t_year_herd_map_c_sh, N2O_t_year_herd_map_g_sh, 
                                  fun = fun)


# CO2 equ from manure
fun <- function(x, y) {
  (x * 298) + (y * 23)
}
# 298 conversion factor to Co2 equ for 100 year horizon (gwp) for N2O,
# and 23 for CH4
co2e_manure <- overlay(N2O_t_year_herd_map, manure_CH4_year_herd_tot, fun = fun)
co2e_manure_ct <- overlay(N2O_t_year_herd_map_ct, manure_CH4_year_herd_ct, 
                          fun = fun)
co2e_manure_sh <- overlay(N2O_t_year_herd_map_sh, manure_CH4_year_herd_sh, 
                          fun = fun)


############################## emissions from feed and fodder production ########################

# ghg soil stock change
fun <- function(a, b) {
  a * b
}
Flu_c <- overlay(Flu_c, cropland, fun = fun)
ssc <- read.csv(paste0(path, "/1-input/parameter/ghg_soil_stock_change.csv"))

ssc$avgtil <- perc_til/100 * ssc$Full + perc_redtil/100 * ssc$Reduced + 
  perc_notil/100 * ssc$No.till
fun <- function(a, b, c, d, e, f, g, h) {
  ((a * ssc[6, col]) + (b * ssc[5, col]) + (c * ssc[4, col]) + (d * ssc[3, 
                                                                        col]) + (e * ssc[10, col]) + (f * ssc[9, col]) + (g * ssc[8, col]) + 
     (h * ssc[7, col]))
}
col <- 16
Fmg_c <- overlay(clim_wtmoist, clim_wtdry, clim_ctmoist, clim_ctdry, clim_tr_mont, 
                 clim_tr_wet, clim_tr_moist, clim_tr_dry, fun = fun)
fun2 <- function(x, y) {
  x * y
}
Fmg_c <- overlay(Fmg_c, cropland, fun = fun2)
# plot(Fmg_c)

# create a layer from soil carbon stock change factor related to inputs
# (Fi)
ssc$input <- perc_inlow/100 * ssc$Low + perc_inmedium/100 * ssc$Medium + 
  perc_inhighnoman/100 * ssc$High.without.manure + perc_inhighman/100 * 
  ssc$High.with.manure
col <- 17
Fi_c <- overlay(clim_wtmoist, clim_wtdry, clim_ctmoist, clim_ctdry, clim_tr_mont, 
                clim_tr_wet, clim_tr_moist, clim_tr_dry, fun = fun)
Fi_c <- overlay(Fi_c, cropland, fun = fun2)
# plot(Fi_c)

# make a total Flu map, adjust here with the new land uses xxwhere
# necessary

# fun3<-fun(x,y) {x+y} Flu_tot<-(Flu_c, Flu_rice, fun=fun3)
Flu_tot <- Flu_c

fun4 <- function(a, b, c, d) {
  a * b * c * d
}
SOCa <- overlay(soilref, Flu_tot, Fmg_c, Fi_c, fun = fun4)

fun <- function(x, y) {
  x * y
}
SOCr <- overlay(soilref, Flu_c, fun = fun)

# soil carbon balance on cropland
fun5 <- function(x, y) {
  (x - y) * 3.664 * 1/20
}
scb <- overlay(SOCr, SOCa, fun = fun5)
scb <- overlay(scb, cropland, fun = fun)
# plot(scb) emissions from land use change
csluc <- raster(ext = cropland@extent, res = res(cropland), vals = 0)
# if (add>0){
# gfrs<-read.csv('1-input/parameter/GlobalForestResourcesAssessment2010.csv')
# gfrdom<-gfrs$Dead.organic.matter.forests[gfrs$Country==country]
# fun3<-function(x,y) {(x*y)*(3.664*1/20)*gfrdom}
# rvc_forest<-overlay(forest_clim,lostforest, fun=fun3)
# fun3<-function(x,y) {x*y*3.664*0.05}
# rvc_gras<-overlay(graz_clim,lostgraz, fun=fun3)
# fun=function(x,y){x+y} csluc<-overlay(rvc_gras,rvc_forest,fun=fun )}


############ adding up the 4 indicators
fun <- function(w, x, y, z) {
  w + x + y + z
}
COe <- overlay(rum_co2e_map, co2e_manure, scb, csluc, fun = fun)

fun <- function(w, x, y, z) {
  w + x + (fw_pf_ct + fw_rc_ct + fw_rl_ct)/(fw_pf + fw_rc + fw_rl) * 
    y + z
}  #cslucs is not assigned, because it is always zero for this case
COe_ct <- overlay(rum_co2e_map_ct, co2e_manure_ct, scb, csluc, fun = fun)

fun <- function(w, x, y, z) {
  w + x + (fw_pf_sh + fw_rc_sh + fw_rl_sh)/(fw_pf + fw_rc + fw_rl) * 
    y + z
}
COe_sh <- overlay(rum_co2e_map_sh, co2e_manure_sh, scb, csluc, fun = fun)
# plot(COe)

# landscape level indicator
COe_l <- round(cellStats(COe, stat = "sum"), 0)  # total CO2 producted 
COe_l_ct <- round(cellStats(COe_ct, stat = "sum"), 0)  # total CO2 produced in cattle
COe_l_sh <- round(cellStats(COe_sh, stat = "sum"), 0)  # total CO2 in sheep 

co2e_manure_l <- round(cellStats(co2e_manure, stat = "sum"), 0)  # total CO2 producted from manure 
co2cow <- round(COe_l_ct/numcow, 0)
co2sh <- round(COe_l_sh/numsheep, 0)
co2milk <- round(COe_l_ct * (numcow_es + numcow_is)/numcow/(milk), 0)
co2meat <- round((COe_l_ct * (numcow_sis + numcow_da)/numcow + COe_l_sh)/(meat), 
                 0)

co2meat_ct <- round((COe_l_ct * (numcow_sis + numcow_da)/numcow)/(cattle_meat), 
                    0)
co2meat_sh <- round(COe_l_sh/sheep_meat, 0)


rum_co2e_yeartot <- round(rum_co2e_yeartot, 0)
Coe_ind <- data.frame(COe_l, COe_l_ct, COe_l_sh, co2cow, co2sh, co2milk, 
                      co2meat)


# indifference computation
Coe_ind_diff <- Coe_ind - Coe_base
Coe_ind_perc <- round(Coe_ind_diff/Coe_base * 100, digits = 1)
Coe_ind2 <- rbind(Coe_ind, Coe_ind_diff, Coe_ind_perc)
names(Coe_ind2) <- c("CO2 emmissions", "CO2 emmissions Cattle", "CO2 emmissions Sheep", 
                     "CO2 per cow", "CO2 per sheep", "CO2 per tonnes of milk", "CO2 per tonnes of meat")
Coe_ind2 <- data.frame(t(Coe_ind2))
colnames(Coe_ind2) <- c("result", "difference", "percent")

## steve edits
Coe_ind_val <- read.csv(paste0(path, "/1-input/parameter/ghg_val.csv"))
Coe_ind2$evaluation <- ifelse(abs(Coe_ind2$percent) <= Coe_ind_val$val, 
                              "low", ifelse((abs(Coe_ind2$percent) >= Coe_ind_val$val) & (abs(Coe_ind2$percent) <= 
                                                                                            2 * Coe_ind_val$val), "medium", "high"))

Coe_map_diff <- round(COe - Coe_map, digit = 4)

title1 <- paste0("GHG : CO2 equivalent", name, sep = " ")
title2 <- paste0("GHG : CO2 equ from manure only", name, sep = " ")
title3 <- paste0("GHG : CO2 difference from baseline", name, sep = " ")

col <- colorRampPalette(brewer.pal(9, "YlOrRd"))(100)
col2 <- colorRampPalette((brewer.pal(9, "Reds")))(100)

par(mfrow = c(1, 3), mar = c(2, 4.5, 2, 6))
plot(COe, legend.width = 1, legend.shrink = 0.45, col = col)
plot(sarea, add = TRUE)
title(title1)

plot(co2e_manure, legend.width = 1, legend.shrink = 0.45, col = col)
plot(sarea, add = TRUE)
title(title2)

plot(Coe_map_diff, legend.width = 1, legend.shrink = 0.45, col = col2)
plot(sarea, add = TRUE)
title(title3)

################################# extract the maps for final user #################################
title1 <- paste("Greenhouse gas pathway : CO2 equivalent", name, sep = " ")
title2 <- paste("Greenhouse gas pathway : CO2 equ from manure only", name, 
                sep = " ")
title3 <- paste("Greenhouse gas pathway : CO2 difference from baseline", 
                name, sep = " ")

col <- colorRampPalette(brewer.pal(9, "YlOrRd"))(100)
col2 <- colorRampPalette((brewer.pal(9, "Reds")))(100)

pdf(paste0(path, "/4-output/", name, "-ghg_pathway.pdf", sep = ""))
plot(COe, legend.width = 1, legend.shrink = 0.75, col = col)
plot(sarea, add = TRUE)
title(title1)
plot(co2e_manure, legend.width = 1, legend.shrink = 0.75, col = col)
plot(sarea, add = TRUE)
title(title2)
plot(co2e_manure, legend.width = 1, legend.shrink = 0.75, col = col)
plot(sarea, add = TRUE)
title(title2)
plot(Coe_map_diff, legend.width = 1, legend.shrink = 0.75, col = col2)
plot(sarea, add = TRUE)
title(title3)
plot(NA, xlim = c(0, 100), ylim = c(0, 10), bty = "n", xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "")
grid.table(Coe_ind2)

dev.off()

writeRaster(COe, paste0(path, "/4-output/", name, "-ghg_map.tif", sep = ""), overwrite = TRUE)
# writeRaster(co2e_manure,paste(name,'ghg_co2e_manure.tif',
# sep='-'),overwrite=TRUE)
write.csv(Coe_ind, paste(path, "/4-output/", name, "-ghg_ind.csv", sep = ""), row.names = F)