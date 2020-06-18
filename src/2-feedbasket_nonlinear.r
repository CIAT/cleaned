library(pacman)
pacman::p_load(raster, gridExtra)

# read parameter
para <- read.csv(paste0(path, "/1-input/parameter/para.csv"))
para$value <- as.numeric(para$value)
dim <- dim(para)
obs <- dim[1]
for (i in seq_along(para$name)) {
  assign(as.character(para$name[i]), para$value[i])
}

# do we run a land use change driven scenario? then we need to run the
# land use change module first and read here the file indicating the
# pixels that have changed

# add the path to the changes in land use rasters path to changing
# cropland i.e. cropland change layer


# now overwrite variables with preset
if (preset == 1) {
  # get the parameter of the dual purpose dairy
  temp <- scenario[, c("DDvar", DD)]
  names(temp)[2] <- "ppara"
  for (i in which(!temp$DDvar == "")) {
    assign(as.character(temp$DDvar[i]), temp$ppara[i])
  }
}

if (preset == 1) {
  # get the parameter of dual purpose meat (fattening and rearing)
  temp <- scenario[, c("DFvar", DF)]
  names(temp)[2] <- "ppara"
  for (i in which(!temp$DFvar == "")) {
    assign(as.character(temp$DFvar[i]), temp$ppara[i])
  }
}

if (preset == 1) {
  # get the parameter of the draft animals
  temp <- scenario[, c("DAvar", DA)]
  names(temp)[2] <- "ppara"
  for (i in which(!temp$DAvar == "")) {
    assign(as.character(temp$DAvar[i]), temp$ppara[i])
  }
}

if (preset == 1) {
  # get the parameter of the draft animals
  temp <- scenario[, c("SDvar", SD)]
  names(temp)[2] <- "ppara"
  for (i in which(!temp$SDvar == "")) {
    assign(as.character(temp$SDvar[i]), temp$ppara[i])
  }
}

if (preset == 1) {
  # get the parameter of the draft animals
  temp <- scenario[, c("SHvar", SH)]
  names(temp)[2] <- "ppara"
  for (i in which(!temp$SHvar == "")) {
    assign(as.character(temp$SHvar[i]), temp$ppara[i])
  }
}


if (preset == 1) {
  # get the parameter of the cropping system
  temp <- scenario[, c("Crvar", Cr)]
  names(temp)[2] <- "ppara"
  for (i in which(!temp$Crvar == "")) {
    assign(as.character(temp$Crvar[i]), temp$ppara[i])
  }
}

# computation of the number of animals in the landscape/area
numcow <- numcow_es + numcow_sis + numcow_is + numcow_da

# these are the computation for cattle, for any other animal, this has to be adjusted based on IPCC guidlines


# total energy requirement per average cow per year per system
# maintenance energy eq 10.3, and table 10.4 energy per day
er_mes <- 0.386 * lwes^0.75  # lactating animals 
er_msis <- 0.322 * lwsis^0.75  # non-lactating animals
er_mis <- 0.386 * lwis^0.75  # lacatating animals
er_mda <- 0.37 * lwda^0.75  # bulls
er_msh <- 0.217 * lwsh^0.75  # sheep older than 1 yeasr

# activity energy equ 10.4 table 10.5 # per day
er_ames <- 0.17 * er_mes
er_amsis <- 0.3 * er_mis  # reduced large gazing area coefficient
er_amis <- 0 * er_mis  # assumtion these animal still graze a bit  so the value is between 0 and 0.17
er_amda <- 0.15 * er_mda
er_amsh <- 0.0107 * lwsh  # grazing flat pastures 

# lactation energy eq 10.8 assuming milk fat content of 3.5%, this
# equation is per year because milk production is per year
er_les <- myes * (1.47 + 0.4 * 3.5)
er_lsis <- mysis * (1.47 + 0.4 * 3.5)
er_lis <- myis * (1.47 + 0.4 * 3.5)
er_lda <- 0
# wool production energy eq 10.12
er_wool <- 24 * 0.6/365

# net energy requirement per animal per day
erc_es <- (er_mes + er_ames) + er_les/365
erc_sis <- (er_msis + er_amsis) + er_lsis/365
erc_is <- (er_mis + er_amis) + er_lis/365
erc_da <- (er_mda + er_amda) + er_lda/365 + er_mda * 0.1 * 1  #draft energy  equ10.11
erc_sh <- er_msh + er_amsh + er_wool
# total energy needed for maintaining ruminants in the study area
erc_ct <- (erc_es * numcow_es + numcow_sis * erc_sis + numcow_is * erc_is + 
             erc_da * numcow_da)

# calculate energy requirement in by season from each feed source dry season
ng_es <- (efng1/100 * numcow_es * erc_es) * ds/12 * 365 + (efng2/100 * 
                                                             numcow_es * erc_es) * ws/12 * 365
ng_sis <- (sfng1/100 * numcow_sis * erc_sis) * ds/12 * 365 + (sfng2/100 * 
                                                                numcow_sis * erc_sis) * ws/12 * 365
ng_is <- (ifng1/100 * numcow_is * erc_is) * ds/12 * 365 + (ifng2/100 * 
                                                             numcow_is * erc_is) * ws/12 * 365
ng_da <- (dafng1/100 * numcow_da * erc_da) * ds/12 * 365 + (dafng2/100 * 
                                                              numcow_da * erc_da) * ws/12 * 365
ng_sh <- (shfng1/100 * numsheep * erc_sh) * ds/12 * 365 + (shfng2/100 * 
                                                             numsheep * erc_sh) * ws/12 * 365

rc_es <- (efrc1/100 * numcow_es * erc_es) * ds/12 * 365 + (efrc2/100 * 
                                                             numcow_es * erc_es) * ws/12 * 365
rc_sis <- (sfrc1/100 * numcow_sis * erc_sis) * ds/12 * 365 + (sfrc2/100 * 
                                                                numcow_sis * erc_sis) * ws/12 * 365
rc_is <- (ifrc1/100 * numcow_is * erc_is) * ds/12 * 365 + (ifrc2/100 * 
                                                             numcow_is * erc_is) * ws/12 * 365
rc_da <- (dafrc1/100 * numcow_da * erc_da) * ds/12 * 365 + (dafrc2/100 * 
                                                              numcow_da * erc_da) * ws/12 * 365
rc_sh <- (shfrc1/100 * numsheep * erc_sh) * ds/12 * 365 + (shfrc2/100 * 
                                                             numsheep * erc_sh) * ws/12 * 365

rr_es <- (efrr1/100 * numcow_es * erc_es) * ds/12 * 365 + (efrr2/100 * 
                                                             numcow_es * erc_es) * ws/12 * 365
rr_sis <- (sfrr1/100 * numcow_sis * erc_sis) * ds/12 * 365 + (sfrr2/100 * 
                                                                numcow_sis * erc_sis) * ws/12 * 365
rr_is <- (ifrr1/100 * numcow_is * erc_is) * ds/12 * 365 + (ifrr2/100 * 
                                                             numcow_is * erc_is) * ws/12 * 365
rr_da <- (dafrr1/100 * numcow_da * erc_da) * ds/12 * 365 + (dafrr2/100 * 
                                                              numcow_da * erc_da) * ws/12 * 365
rr_sh <- (shfrr1/100 * numsheep * erc_sh) * ds/12 * 365 + (shfrr2/100 * 
                                                             numsheep * erc_sh) * ws/12 * 365

rl_es <- (efrl1/100 * numcow_es * erc_es) * ds/12 * 365 + (efrl2/100 * 
                                                             numcow_es * erc_es) * ws/12 * 365
rl_sis <- (sfrl1/100 * numcow_sis * erc_sis) * ds/12 * 365 + (sfrl2/100 * 
                                                                numcow_sis * erc_sis) * ws/12 * 365
rl_is <- (ifrl1/100 * numcow_is * erc_is) * ds/12 * 365 + (ifrl2/100 * 
                                                             numcow_is * erc_is) * ws/12 * 365
rl_da <- (dafrl1/100 * numcow_da * erc_da) * ds/12 * 365 + (dafrl2/100 * 
                                                              numcow_da * erc_da) * ws/12 * 365
rl_sh <- (shfrl1/100 * numsheep * erc_sh) * ds/12 * 365 + (shfrl2/100 * 
                                                             numsheep * erc_sh) * ws/12 * 365

pf_es <- (efpf1/100 * numcow_es * erc_es) * ds/12 * 365 + (efpf2/100 * 
                                                             numcow_es * erc_es) * ws/12 * 365
pf_sis <- (sfpf1/100 * numcow_sis * erc_sis) * ds/12 * 365 + (sfpf2/100 * 
                                                                numcow_sis * erc_sis) * ws/12 * 365
pf_is <- (ifpf1/100 * numcow_is * erc_is) * ds/12 * 365 + (ifpf2/100 * 
                                                             numcow_is * erc_is) * ws/12 * 365
pf_da <- (dafpf1/100 * numcow_da * erc_da) * ds/12 * 365 + (dafpf2/100 * 
                                                              numcow_da * erc_da) * ws/12 * 365
pf_sh <- (shfpf1/100 * numsheep * erc_sh) * ds/12 * 365 + (shfpf2/100 * 
                                                             numsheep * erc_sh) * ws/12 * 365

conc_es <- (efconc1/100 * numcow_es * erc_es) * ds/12 * 365 + (efconc2/100 * 
                                                                 numcow_es * erc_es) * ws/12 * 365
conc_sis <- (sfconc1/100 * numcow_sis * erc_sis) * ds/12 * 365 + (sfconc2/100 * 
                                                                    numcow_sis * erc_sis) * ws/12 * 365
conc_is <- (ifconc1/100 * numcow_is * erc_is) * ds/12 * 365 + (ifconc2/100 * 
                                                                 numcow_is * erc_is) * ws/12 * 365
conc_da <- (dafconc1/100 * numcow_da * erc_da) * ds/12 * 365 + (dafconc2/100 * 
                                                                  numcow_da * erc_da) * ws/12 * 365
conc_sh <- (shfconc1/100 * numsheep * erc_sh) * ds/12 * 365 + (shfconc2/100 * 
                                                                 numsheep * erc_sh) * ws/12 * 365

conos_es <- (efconos1/100 * numcow_es * erc_es) * ds/12 * 365 + (efconos2/100 * 
                                                                   numcow_es * erc_es) * ws/12 * 365
conos_sis <- (sfconos1/100 * numcow_sis * erc_sis) * ds/12 * 365 + (sfconos2/100 * 
                                                                      numcow_sis * erc_sis) * ws/12 * 365
conos_is <- (ifconos1/100 * numcow_is * erc_is) * ds/12 * 365 + (ifconos2/100 * 
                                                                   numcow_is * erc_is) * ws/12 * 365
conos_da <- (dafconos1/100 * numcow_da * erc_da) * ds/12 * 365 + (dafconos2/100 * 
                                                                    numcow_da * erc_da) * ws/12 * 365
conos_sh <- (shfconos1/100 * numsheep * erc_sh) * ds/12 * 365 + (shfconos2/100 * 
                                                                   numsheep * erc_sh) * ws/12 * 365

hay_es <- (efhay1/100 * numcow_es * erc_es) * ds/12 * 365 + (efhay2/100 * 
                                                               numcow_es * erc_es) * ws/12 * 365
hay_sis <- (sfhay1/100 * numcow_sis * erc_sis) * ds/12 * 365 + (sfhay2/100 * 
                                                                  numcow_sis * erc_sis) * ws/12 * 365
hay_is <- (ifhay1/100 * numcow_is * erc_is) * ds/12 * 365 + (ifhay2/100 * 
                                                               numcow_is * erc_is) * ws/12 * 365
hay_da <- (dafhay1/100 * numcow_da * erc_da) * ds/12 * 365 + (dafhay2/100 * 
                                                                numcow_da * erc_da) * ws/12 * 365
hay_sh <- (shfhay1/100 * numsheep * erc_sh) * ds/12 * 365 + (shfhay2/100 * 
                                                               numsheep * erc_sh) * ws/12 * 365

sil_es <- (efsil1/100 * numcow_es * erc_es) * ds/12 * 365 + (efsil2/100 * 
                                                               numcow_es * erc_es) * ws/12 * 365
sil_sis <- (sfsil1/100 * numcow_sis * erc_sis) * ds/12 * 365 + (sfsil2/100 * 
                                                                  numcow_sis * erc_sis) * ws/12 * 365
sil_is <- (ifsil1/100 * numcow_is * erc_is) * ds/12 * 365 + (ifsil2/100 * 
                                                               numcow_is * erc_is) * ws/12 * 365
sil_da <- (dafsil1/100 * numcow_da * erc_da) * ds/12 * 365 + (dafsil2/100 * 
                                                                numcow_da * erc_da) * ws/12 * 365
sil_sh <- (shfsil1/100 * numsheep * erc_sh) * ds/12 * 365 + (shfsil2/100 * 
                                                               numsheep * erc_sh) * ws/12 * 365
# calculating the fresh weight of feed in basket percent were in fresh
# weigh already so we need to correct only for dried stuff corrected
# for the fact that meg (metabolizing energy)
fw_g_es <- ng_es/(meg)
fw_g_sis <- ng_sis/(meg)
fw_g_is <- ng_is/(meg)
fw_g_da <- ng_da/(meg)
fw_g_sh <- ng_sh/(meg)
fw_g <- fw_g_es + fw_g_sis + fw_g_is + fw_g_da + fw_g_sh
fw_g_ct <- fw_g_es + fw_g_sis + fw_g_is + fw_g_da

fw_rc_es <- rc_es/(merc)
fw_rc_sis <- rc_sis/(merc)
fw_rc_is <- rc_is/(merc)
fw_rc_da <- rc_da/(merc)
fw_rc_sh <- rc_sh/(merc)
fw_rc <- fw_rc_es + fw_rc_sis + fw_rc_is + fw_rc_da + fw_rc_sh
fw_rc_ct <- fw_rc_es + fw_rc_sis + fw_rc_is + fw_rc_da

fw_rl_es <- rl_es/(merl)
fw_rl_sis <- rl_sis/(merl)
fw_rl_is <- rl_is/(merl)
fw_rl_da <- rl_da/(merl)
fw_rl_sh <- rl_sh/(merl)
fw_rl <- fw_rl_es + fw_rl_sis + fw_rl_is + fw_rl_da + fw_rl_sh
fw_rl_ct <- fw_rl_es + fw_rl_sis + fw_rl_is + fw_rl_da

fw_pf_es <- pf_es/(mepf)
fw_pf_sis <- pf_sis/(mepf)
fw_pf_is <- pf_is/(mepf)
fw_pf_da <- pf_da/(mepf)
fw_pf_sh <- pf_sh/(mepf)
fw_pf <- fw_pf_es + fw_pf_sis + fw_pf_is + fw_pf_da + fw_pf_sh
fw_pf_ct <- fw_pf_es + fw_pf_sis + fw_pf_is + fw_pf_da

# fw_rr_es<-rr_es/(merr) fw_rr_sis<-rr_sis/(merr)
# fw_rr_is<-rr_is/(merr) fw_rr_da<-rr_da/(merr) fw_rr_sh<-rr_sh/(merr)
# fw_rr<-fw_rr_es+fw_rr_sis+fw_rr_is+fw_rr_da+fw_rr_sh

fw_conc_es <- conc_es/(meconc)
fw_conc_sis <- conc_sis/(meconc)
fw_conc_is <- conc_is/(meconc)
fw_conc_da <- conc_da/(meconc)
fw_conc_sh <- conc_sh/(meconc)
fw_conc <- fw_conc_es + fw_conc_sis + fw_conc_is + fw_conc_da + fw_conc_sh
fw_conc_ct <- fw_conc_es + fw_conc_sis + fw_conc_is + fw_conc_da

fw_conos_es <- conos_es/(meconos)
fw_conos_sis <- conos_sis/(meconos)
fw_conos_is <- conos_is/(meconos)
fw_conos_da <- conos_da/(meconos)
fw_conos_sh <- conos_sh/(meconos)
fw_conos <- fw_conos_es + fw_conos_sis + fw_conos_is + fw_conos_da + fw_conos_sh
fw_conos_ct <- fw_conos_es + fw_conos_sis + fw_conos_is + fw_conos_da

fwg_hay_es <- hay_es/meh * (1 + ((1 - dm_g) - (1 - dm_h)))
fwg_hay_sis <- hay_sis/meh * (1 + ((1 - dm_g) - (1 - dm_h)))
fwg_hay_is <- hay_is/meh * (1 + ((1 - dm_g) - (1 - dm_h)))
fwg_hay_da <- hay_da/meh * (1 + ((1 - dm_g) - (1 - dm_h)))
fwg_hay_sh <- hay_sh/meh * (1 + ((1 - dm_g) - (1 - dm_h)))
fwg_hay <- fwg_hay_es + fwg_hay_sis + fwg_hay_is + fwg_hay_da + fwg_hay_sh
fwg_hay_ct <- fwg_hay_es + fwg_hay_sis + fwg_hay_is + fwg_hay_da

fwg_sil_es <- sil_es/mes
fwg_sil_sis <- sil_sis/mes
fwg_sil_is <- sil_is/mes
fwg_sil_da <- sil_da/mes
fwg_sil_sh <- sil_sh/mes
fwg_sil <- fwg_sil_es + fwg_sil_sis + fwg_sil_is + fwg_sil_da + fwg_sil_sh
fwg_sil_ct <- fwg_sil_es + fwg_sil_sis + fwg_sil_is + fwg_sil_da

# total energy required from each source of food per species big
# ruminant
ng_ct <- ng_es + ng_sis + ng_is + ng_da
rc_ct <- rc_es + rc_sis + rc_is + rc_da
rr_ct <- rr_es + rr_sis + rr_is + rr_da
rl_ct <- rl_es + rl_sis + rl_is + rl_da
pf_ct <- pf_es + pf_sis + pf_is + pf_da
conc_ct <- conc_es + conc_sis + conc_is + conc_da
conos_ct <- conos_es + conos_sis + conos_is + conos_da
hay_ct <- hay_es + hay_sis + hay_is + hay_da
sil_ct <- sil_es + sil_sis + sil_is + sil_da

# fraction of each fodder at landscape scale over the year step for
# cattle.
fng_ct <- ng_ct/(ng_ct + rc_ct + rl_ct + pf_ct + conc_ct + conos_ct + hay_ct + 
                   sil_ct)
frc_ct <- rc_ct/(ng_ct + rc_ct + rl_ct + pf_ct + conc_ct + conos_ct + hay_ct + 
                   sil_ct)
frr_ct <- rr_ct/(ng_ct + rc_ct + rl_ct + pf_ct + conc_ct + conos_ct + hay_ct + 
                   sil_ct)
frl_ct <- rl_ct/(ng_ct + rc_ct + rl_ct + pf_ct + conc_ct + conos_ct + hay_ct + 
                   sil_ct)
fpf_ct <- pf_ct/(ng_ct + rc_ct + rl_ct + pf_ct + conc_ct + conos_ct + hay_ct + 
                   sil_ct)
fconc_ct <- conc_ct/(ng_ct + rc_ct + rl_ct + pf_ct + conc_ct + conos_ct + 
                       hay_ct + sil_ct)
fconos_ct <- conos_ct/(ng_ct + rc_ct + rl_ct + pf_ct + conc_ct + conos_ct + 
                         hay_ct + sil_ct)
fhay_ct <- hay_ct/(ng_ct + rc_ct + rl_ct + pf_ct + conc_ct + conos_ct + 
                     hay_ct + sil_ct)
fsil_ct <- sil_ct/(ng_ct + rc_ct + rl_ct + pf_ct + conc_ct + conos_ct + 
                     hay_ct + sil_ct)

# fraction of each fodder at landscape scale over the year step for
# sheep
fng_sh <- ng_sh/(ng_sh + rc_sh + rl_sh + pf_sh + conc_sh + conos_sh + hay_sh + 
                   sil_sh)
frc_sh <- rc_sh/(ng_sh + rc_sh + rl_sh + pf_sh + conc_sh + conos_sh + hay_sh + 
                   sil_sh)
frr_sh <- rr_sh/(ng_sh + rc_sh + rl_sh + pf_sh + conc_sh + conos_sh + hay_sh + 
                   sil_sh)
frl_sh <- rl_sh/(ng_sh + rc_sh + rl_sh + pf_sh + conc_sh + conos_sh + hay_sh + 
                   sil_sh)
fpf_sh <- pf_sh/(ng_sh + rc_sh + rl_sh + pf_sh + conc_sh + conos_sh + hay_sh + 
                   sil_sh)
fconc_sh <- conc_sh/(ng_sh + rc_sh + rl_sh + pf_sh + conc_sh + conos_sh + 
                       hay_sh + sil_sh)
fconos_sh <- conos_sh/(ng_sh + rc_sh + rl_sh + pf_sh + conc_sh + conos_sh + 
                         hay_sh + sil_sh)
fhay_sh <- hay_sh/(ng_sh + rc_sh + rl_sh + pf_sh + conc_sh + conos_sh + 
                     hay_sh + sil_sh)
fsil_sh <- sil_sh/(ng_sh + rc_sh + rl_sh + pf_sh + conc_sh + conos_sh + 
                     hay_sh + sil_sh)

# total feedbasket at landscape scale
fng <- fng_ct + fng_sh
frc <- frc_ct + frc_sh
frr <- frr_ct + frr_sh
frl <- frl_ct + frl_sh
fpf <- fpf_ct + fpf_sh
fconc <- fconc_ct + fconc_sh
fconos <- fconos_ct + fconos_sh
fhay <- fhay_ct + fhay_sh
fsil <- fsil_ct + fsil_sh
# # create output for vignettes
# names_feed<-c('fw_g_es','fw_g_sis','fw_g_is','fw_g_da','fw_g_sh','fw_rc_es','fw_rc_sis','fw_rc_is','fw_rc_da','fw_rc_sh','fw_rl_es','fw_rl_sis','fw_rl_is','fw_rl_da','fw_rl_sh','fw_pf_es','fw_pf_sis','fw_pf_is','fw_pf_da','fw_pf_sh','fw_conc_es','fw_conc_sis','fw_conc_is','fw_conc_da','fw_conc_sh','fw_conos_es','fw_conos_sis','fw_conos_is','fw_conos_da','fw_conos_sh','fwg_hay_es','fwg_hay_sis','fwg_hay_is','fwg_hay_da','fwg_hay_sh','fwg_sil_es','fwg_sil_sis','fwg_sil_is','fwg_sil_da','fwg_sil_sh')
# values_feed
# <-c(fw_g_es/numcow_es/365,fw_g_sis/numcow_sis/365,fw_g_is/numcow_is/365,fw_g_da/numcow_da/365,fw_g_sh/numsheep/365,fw_rc_es/numcow_es/365,fw_rc_sis/numcow_sis/365,fw_rc_is/numcow_is/365,fw_rc_da/numcow_da/365,fw_rc_sh/numsheep/365,fw_rl_es/numcow_es/365,fw_rl_sis/numcow_sis/365,fw_rl_is/numcow_is/365,fw_rl_da/numcow_da/365,fw_rl_sh/numsheep/365,fw_pf_es/numcow_es/365,fw_pf_sis/numcow_sis/365,fw_pf_is/numcow_is*365,fw_pf_da/numcow_da/365,fw_pf_sh/numsheep/365,fw_conc_es/numcow_es/365,fw_conc_sis/numcow_sis/365,fw_conc_is/numcow_is/365,fw_conc_da/numcow_da/365,fw_conc_sh/numsheep/365,fw_conos_es/numcow_es/365,fw_conos_sis/numcow_sis/365,fw_conos_is/numcow_is/365,fw_conos_da/numcow_da/365,fw_conos_sh/numsheep/365,fwg_hay_es/numcow_es/365,fwg_hay_sis/numcow_sis/365,fwg_hay_is/numcow_is/365,fwg_hay_da/numcow_da/365,fwg_hay_sh/numsheep/365,fwg_sil_es/numcow_es/365,fwg_sil_sis/numcow_sis/365,fwg_sil_is/numcow_is/365,fwg_sil_da/numcow_da/365,fwg_sil_sh/numsheep/365)
# vignette<-data.frame(names=names_feed,values=values_feed)

# ratio of net energy available in the diet for maintenance to
# digestible energy consumed (REM) first compute digestibiliy of the
# landscape level feed
# de<-(ng*d_g+rc*d_rc+rl*d_rl+pf*d_pf+conc*d_conc+conos*d_conos+rr*d_rr)*100
# # this seems to be wrong 11.3.2017
de_ct <- (fng_ct * d_g + frc_ct * d_rc + frl_ct * d_rl + fpf_ct * d_pf + 
            fconc_ct * d_conc + fconos_ct * d_conos + fhay_ct * d_h + fsil_ct * 
            d_s) * 100
de_sh <- (fng_sh * d_g + frc_sh * d_rc + frl_sh * d_rl + fpf_sh * d_pf + 
            fconc_sh * d_conc + fconos_sh * d_conos + fhay_sh * d_h + fsil_sh * 
            d_s) * 100


# REM = ratio of net energy available in a diet for maintenance to
# digestible energy consumed
rem_ct <- (1.123 - (4.092 * 10^-3 * de_ct) + (1.126 * 10^-5 * de_ct^2) - 
             (25.4/de_ct))
rem_sh <- (1.123 - (4.092 * 10^-3 * de_sh) + (1.126 * 10^-5 * de_sh^2) - 
             (25.4/de_sh))

# yearly gross energy requirement at landscape scale 23.01.2018 seems
# to be daily
gerc_ct <- (erc_ct/rem_ct)/(de_ct/100)  #equation 10.16 this is therefore GE at landscape scale per day
gerc_sh <- (erc_sh/rem_sh)/(de_sh/100)  #equation 10.16 this is therefore GE at landscape scale per day


# calculate production milk production in tons of litre
milk_es <- numcow_es * myes * 0.001
milk_sis <- numcow_sis * mysis * 0.001
milk_is <- numcow_is * myis * 0.001
milk <- milk_es + milk_sis + milk_is


# meaat production milk production in tons of litre
meat_es <- numcow_es * des * lwes * 0.001
meat_sis <- numcow_sis * dsis * lwsis * 0.001
meat_is <- numcow_is * dis * lwis * 0.001
meat_da <- numcow_da * dda * lwda * 0.001

cattle_meat <- meat_es + meat_sis + meat_is + meat_da
sheep_meat <- numsheep * lwsh * dsh * 0.001
meat <- cattle_meat + sheep_meat

# area needed for the feed production (with adjustement to pass from ha
# to km2, and tons to kg) first we need to compute the fodder yield.
# This is done by multiplying the crop yield with the residue factor,
# which is the part that is used as feed. This is computed in the water
# pararmeter excel sheet and accounts for post harvest loss

fun <- function(x) {
  x * (1 + pgc) * rfm
}  # residue factor to account for what is consummed by livestock only
fy_rc <- overlay(y_maize, fun = fun)  # ymaize is in kg/pixel 

fun <- function(x) {
  x * (1 + pgl) * rfl
}
fy_rl <- overlay(y_pulse, fun = fun)

fun <- function(x, y) {
  x * (1 + pgc) * y
}
y_maizec <- overlay(y_maize, cropland, fun = fun)
# max_c<-cellStats(y_maizec,stat='sum')
fun <- function(x, y) {
  x * (1 + pgl) * y
}
y_pulsec <- overlay(y_pulse, cropland, fun = fun)
# max_l<-cellStats(y_pulsec,stat='sum')
# y_ricec<-overlay(y_rice,riceland,fun=fun)
# max_r<-cellStats(y_ricec,stat='sum')
fun <- function(x, y) {
  x^y
}
fy_rcc <- overlay(fy_rc, cropland, fun = fun)
# max_rc<-cellStats(fy_rcc,stat='sum')
fy_rlc <- overlay(fy_rl, cropland, fun = fun)
# max_rl<-cellStats(fy_rlc,stat='sum')
# max_rl<-cellStats(fy_rlc,stat='sum') check from here in principle one
# should not work with the area but the produce...


# we assume that every cropland pixel produces the basket so we need to
# correct the total production from the area to avoid double planting

# compute the percentage of each feed on cropland so we need to compute
# the area used for each crop (in term of pixels, so in a 900m2)
fy_rcc[fy_rcc == 0] <- NaN
avg_rc <- cellStats(fy_rcc, stat = "mean")  # per pixel
# area required
ar_rc <- fw_rc/avg_rc  # number pixel, the data is already adjusted in the spatial extraction 
fy_rlc[fy_rlc == 0] <- NaN

avg_rl <- cellStats(fy_rlc, stat = "mean")
ar_rl <- fw_rl/avg_rl
ar_pf <- (fw_pf + fwg_sil)/(fy_pf * (1 + pgpf) * 1000/10000 * pixel[[1]]^2 * 
                              rfpf)  # we consider silage  as planted forrage hay is considered to be from natural grass 3.5.2018
# fy_pf is in tons per ha so to get kg we 1000 and from ha to m is
# /10000, so the denominator is in kg/pixel and the result is a number
# of pixel total cropland area required
ar <- (ar_rc + ar_rl + ar_pf) * pixel[[1]]^2/1e+06  # all ar are in numbers of pixel so we need transform in km2
# for indicator purpose let us compute area of planted fodder in km2
# 12.10.2018
ar_pf_ha <- ar_pf * pixel[[1]]^2/1e+06
# # check if ther e is a land use change required and run the code that
# will yield at add crop if (add > 0){ source('3-cleaned/2-luc.r')} #
# compute the new land cover layers if (add>0) {
# source('3-cleaned/2-luccomp.r') }

# calculate the production of planted fodder under cropland
croparea <- cellStats(cropland, stat = "sum") * pixel[[1]]^2/1e+06  #area in km
# computing needs for the crossing animals assuming they are staying 1
# month in the area percentage of cropland under planted fodder
pf_crop_ratio <- ar_pf_ha/croparea



diff_ar <- croparea - ar  # ar is in pixel so need t be transformed in km
if (diff_ar > 0) {
  import_c <- 0
  totmaize <- cellStats(y_maizec, stat = "mean") * ar_rc * pixel[[1]]^2/1e+06 + 
    cellStats(y_maizec, stat = "mean") * (diff_ar)
  
} else {
  import_c <- -diff_ar
  
  totmaize <- cellStats(y_maizec, stat = "mean") * ar_rc * pixel[[1]]^2/1e+06 + 
    cellStats(y_maizec, stat = "mean") * (diff_ar)
}  # now imports are in area in km2 
diff_ar <- round(diff_ar, digit = 0)
# calculate the production under grazing land

# lets compute the maize produced in the area original yield is
# kg/pixel and ar_rc is in pixel so *0.001 it to get tons we also
# assume that all the cropland that is not used for livestock is
# actually maize then we can add this to the maize produced in the area
# it can be that there is less maize as by product to livestock, yet
# the total land used for livestock is less, so there is no food
# security issue





grazarea <- cellStats(grazland, stat = "sum") * pixel[[1]]^2/1e+06  #area in km2
ar_g <- (fw_g + fwg_hay)/(fy_g * (1 + pgg)) * 0.01/1000  #*0.01 from ha to km2 and 1000 for passing from tons to kg,  we are alredy in tons, so we need to go into kg
# now hay is from natural grass! 3.5.2018 reports from stakeholders
# (district representative) that hey is from grassland and sillage from
# crops and planted fodder.

diff_g <- grazarea - ar_g
if (diff_g > 0) {
  import_g <- 0
} else {
  import_g <- -diff_g
}
diff_g <- round(diff_g, digit = 0)

#################################### moving the manure management computation here as they are used in
#################################### soil and ghg
species <- "dairyCows"
species2 <- "Othercattle"
species3 <- "OtherCattle"
species4 <- "Cattle"
species5 <- "Sheep"
# Africa, Asia, Latin America
region <- "Africa"
region2 <- "developing"

country <- "Ethiopia"  # for global forest ressource
# for dairy extracting the IPCC parameter for each management type
# depending on temperature
mms_lagoon <- (es_lagoon_perc * numcow_es + is_lagoon_perc * numcow_is)
mms_liquidslurry <- (es_liquidslurry_perc * numcow_es + is_liquidslurry_perc * 
                       numcow_is)
mms_solidstorage <- (es_solidstorage_perc * numcow_es + is_solidstorage_perc * 
                       numcow_is)
mms_drylot <- (es_drylot_perc * numcow_es + is_drylot_perc * numcow_is)
mms_pasture <- (es_pasture_perc * numcow_es + is_pasture_perc * numcow_is)
mms_dailyspread <- (es_dailyspread_perc * numcow_es + is_dailyspread_perc * 
                      numcow_is)
mms_digester <- (es_digester_perc * numcow_es + is_digester_perc * numcow_is)
mms_fuel <- (es_fuel_perc * numcow_es + is_fuel_perc * numcow_is)
mms_other <- (es_other_perc * numcow_es + is_other_perc * numcow_is)
mms_total <- mms_lagoon + mms_liquidslurry + mms_solidstorage + mms_drylot + 
  mms_pasture + mms_dailyspread + mms_fuel + mms_other

mms_lagoon_perc <- mms_lagoon/mms_total * 100
mms_liquidslurry_perc <- mms_liquidslurry/mms_total * 100
mms_solidstorage_perc <- mms_solidstorage/mms_total * 100
mms_drylot_perc <- mms_drylot/mms_total * 100
mms_pasture_perc <- mms_pasture/mms_total * 100
mms_dailyspread_perc <- mms_dailyspread/mms_total * 100
mms_digester_perc <- mms_digester/mms_total * 100
mms_fuel_perc <- mms_fuel/mms_total * 100
mms_other_perc <- mms_other/mms_total * 100

mms_params1 <- read.csv(paste0(path, "/1-input/parameter/MMSparams.csv"))
mms_params <- subset(mms_params1, mms_params1$Species == species & mms_params1$Region == 
                       region)
B0 <- mms_params$B0

if (ipcc == 1) {
  mms_lagoon_perc <- mms_params$lagoon_perc
  mms_liquidslurry_perc <- mms_params$Liquid.slurry_perc
  mms_solidstorage_perc <- mms_params$Solid.storage_perc
  mms_drylot_perc <- mms_params$Dry.lot_perc
  mms_pasture_perc <- mms_params$Pasture_perc
  mms_dailyspread_perc <- mms_params$Daily.spread_perc
  mms_digester_perc <- mms_params$Digester_perc
  mms_fuel_perc <- mms_params$Burned.for.fuel_perc
  mms_other_perc <- mms_params$Other_perc
}

# for non dairy
mms_params <- subset(mms_params1, mms_params1$Species == species2 & mms_params1$Region == 
                       region)
B02 <- mms_params$B0

mms_lagoon2 <- (sis_lagoon_perc * numcow_sis + da_lagoon_perc * numcow_da)
mms_liquidslurry2 <- (sis_liquidslurry_perc * numcow_sis + da_liquidslurry_perc * 
                        numcow_da)
mms_solidstorage2 <- (sis_solidstorage_perc * numcow_sis + da_solidstorage_perc * 
                        numcow_da)
mms_drylot2 <- (sis_drylot_perc * numcow_sis + da_drylot_perc * numcow_da)
mms_pasture2 <- (sis_pasture_perc * numcow_sis + da_pasture_perc * numcow_da)
mms_dailyspread2 <- (sis_dailyspread_perc * numcow_sis + da_dailyspread_perc * 
                       numcow_da)
mms_digester2 <- (sis_digester_perc * numcow_sis + da_digester_perc * numcow_da)
mms_fuel2 <- (sis_fuel_perc * numcow_sis + da_fuel_perc * numcow_da)
mms_other2 <- (sis_other_perc * numcow_sis + da_other_perc * numcow_da)
mms_total2 <- mms_lagoon2 + mms_liquidslurry2 + mms_solidstorage2 + mms_drylot2 + 
  mms_pasture2 + mms_dailyspread2 + mms_fuel2 + mms_other2

mms_lagoon_perc2 <- mms_lagoon2/mms_total2 * 100
mms_liquidslurry_perc2 <- mms_liquidslurry2/mms_total2 * 100
mms_solidstorage_perc2 <- mms_solidstorage2/mms_total2 * 100
mms_drylot_perc2 <- mms_drylot2/mms_total2 * 100
mms_pasture_perc2 <- mms_pasture2/mms_total2 * 100
mms_dailyspread_perc2 <- mms_dailyspread2/mms_total2 * 100
mms_digester_perc2 <- mms_digester2/mms_total2 * 100
mms_fuel_perc2 <- mms_fuel2/mms_total2 * 100
mms_other_perc2 <- mms_other2/mms_total2 * 100

mms_pasture_perc3 <- sh_pasture_perc

if (ipcc == 1) {
  mms_lagoon_perc2 <- mms_params$lagoon_perc
  mms_liquidslurry_perc2 <- mms_params$Liquid.slurry_perc
  mms_solidstorage_perc2 <- mms_params$Solid.storage_perc
  mms_drylot_perc2 <- mms_params$Dry.lot_perc
  mms_pasture_perc2 <- mms_params$Pasture_perc
  mms_dailyspread_perc2 <- mms_params$Daily.spread_perc
  mms_digester_perc2 <- mms_params$Digester_perc
  mms_fuel_perc2 <- mms_params$Burned.for.fuel_perc
  mms_other_perc2 <- mms_params$Other_perc
}

# for sheep
mms_params1 <- read.csv(paste0(path, "/1-input/parameter/MMSparams.csv"))
mms_params <- subset(mms_params1, mms_params1$Species == species5 & mms_params1$Region == 
                       region2)
B03 <- mms_params$B0

# check what is needed here for sheep in staead of the mms

# if (add > 0) { source('3-cleaned/2-luccomp.r') taken out on 28.3.2018
# check if it still works, there is this code at the begining of the
# code }

prod_base <- read.csv(paste0(path, "/1-input/base_run/prod_ind.csv"), row.names = NULL)
# prod_base<-prod_base[,2:length(prod_base)]

import_c <- round(import_c, digits = 0)
import_g <- round(import_g, digits = 0)
croparea <- round(croparea, digits = 0)
grazarea <- round(grazarea, digits = 0)
fw_conc2 <- round(fw_conc * 0.001, digits = 0)
fw_conos2 <- round(fw_conos * 0.001, digits = 0)
fw_contot <- fw_conc2 + fw_conos2

# ricearea<-round(ricearea,digits=0)
totmaize <- round(totmaize, digits = 0)
ar_g <- round(ar_g, digits = 1)
ar <- round(ar, digits = 1)
ar_pf_ha <- round(ar_pf_ha, digits = 1)

pf_crop_ratio <- round(pf_crop_ratio, digits = 3)
# ar_rr<-round(ar_rr,digits=1)

prod_ind <- data.frame(milk, cattle_meat, sheep_meat, totmaize, croparea, 
                       grazarea, ar, ar_g, import_c, import_g, fw_contot, fw_conc2, fw_conos2, 
                       ar_pf_ha, pf_crop_ratio)
prod_ind_diff <- prod_ind - prod_base
prod_ind_per <- round(prod_ind_diff/prod_base * 100, digit = 1)
# prod_ind_per<-ifelse(is.na(prod_ind_per) ,0,prod_ind_per)
prod_ind2 <- rbind(prod_ind, prod_ind_diff)
prod_ind2 <- rbind(prod_ind2, prod_ind_per)
names(prod_ind2) <- c("milk produced", "cattlemeat produced", "sheep meat", 
                      "cereal produced", "total area available for crop", "total area available for pasture", 
                      "crop area used", "pasture area used", "import crop", "import pasture", 
                      "concentrate total", "concentrate-bran", "concentrate - oil seed cake", 
                      "planted fodder", "planted fodder crop ratio")

prod_ind2 <- data.frame(t(prod_ind2))
colnames(prod_ind2) <- c("result", "diff", "percent")
## steve edits
## prod_ind_val<-read.csv('1-input/parameter/productivity_val.csv')
## prod_ind2$evaluation <- ifelse(abs(prod_ind2$percent) <=
## prod_ind_val$val, 'low', ifelse((abs(prod_ind2$percent) >=
## prod_ind_val$val) & (abs(prod_ind2$percent) <=
## 2*prod_ind_val$val),'medium','high'))

pdf(paste0(path, "/4-output/", name, "-productivity.pdf", sep = ""))
# par(mfrow=(c(2,1)))

plot(NA, xlim = c(0, 100), ylim = c(0, 10), bty = "n", xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "")
grid.table(prod_ind2)

dev.off()

write.csv(prod_ind, paste0(path, "/4-output/", name, "-prod_ind.csv", sep = ""), row.names = F)