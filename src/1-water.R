# water pathway for Tanzania
# by Catherine Pfeifer
# last modified 22.1.2018

# read libraries
library(pacman)
pacman::p_load(raster, maptools, RColorBrewer, gridExtra)

# read data
water_base <- read.csv(paste0(path, "/1-input/base_run/water_ind.csv", row.names = NULL))
# water_base<-water_base[,2:7]
water_map <- raster(paste0(path, "/1-input/base_run/water_map.tif"))

############################ water requirement ############################

# computing water requirment for the crops adjusting et in mm (it is now a high)
fun <- function(x, y) {
  x * y
}

ET_grasl <- overlay(ET_grasl, cropland, fun = fun)
ET_maize <- overlay(ET_maize, cropland, fun = fun)
ET_pulse <- overlay(ET_pulse, cropland, fun = fun)
ET_gras <- overlay(ET_gras, grazland, fun = fun)
# ET_rice<-overlay(ET_rice,riceland,fun=fun)

# plot(ET_grasl)

# now we need to adjust the ET by the proportion in the feed basket
# these proportions are based on area used for each crop given the total crop area
fun <- function(x, y, z, v) {
  (ar_rc/(ar_rc + ar_rl + ar_pf)) * x + (ar_rl/(ar_rc + ar_rl + ar_pf)) * 
    y + (ar_pf/(ar_rc + ar_rl + ar_pf)) * z + v
}
wr <- overlay(ET_maize, ET_pulse, ET_grasl, ET_gras, fun = fun)
# plot(wr)

############################ create the indicator spatial indicator : water use intensity ############################
fun <- function(x, y) {
  x/y
}
wui <- overlay(wr, rain, fun = fun)
# plot(wui) plot(sarea,add=TRUE)

# the lanscape level indicator total water consumed by feed
# wui_avg<-round(cellStats(wr,stat='mean'),1)
wr_sum <- round(cellStats(wr, stat = "sum"), 0)
wu_cow <- round(wr_sum * (fw_pf_ct + fw_rc_ct + fw_rl_ct + fw_g_ct)/(fw_pf + 
                                                                       fw_rc + fw_rl + fw_g)/numcow, 0)  #num cow here takes the average of the pastoral cows over the year
wu_sheep <- round(wr_sum * (fw_pf_sh + fw_rc_sh + fw_rl_sh + fw_g_sh)/(fw_pf + 
                                                                         fw_rc + fw_rl + fw_g)/numsheep, 0)  #num cow here takes the average of the pastoral cows over the year

wratio <- round((wr_sum/(cellStats(rain, stat = "sum"))), 2)  # in cubic meters

# why not per produce
wu_milk <- round(wu_cow * (numcow_es + numcow_is/numcow)/milk, 0)
wu_meat <- round((wu_cow * (numcow_sis + numcow_da/numcow) + wu_sheep)/meat, 
                 0)

# average water intensity
wui_avg <- round(cellStats(wui, stat = "mean"), 3)

# indifference computation
water_ind <- data.frame(wr_sum, wratio, wu_cow, wu_sheep, wu_milk, wu_meat, 
                        wui_avg)
water_ind_diff <- water_ind - water_base
water_map_diff <- round(wui - water_map, digit = 4)
water_ind_diff <- water_ind - water_base
water_ind2 <- rbind(water_ind, water_ind_diff)
water_ind_perc <- round(water_ind_diff/water_base * 100, digit = 1)
water_ind2 <- rbind(water_ind2, water_ind_perc)
names(water_ind2) <- c("total water consumption", " water consumption rainfall ratio", 
                       "water consumption per cow", "water consumption per sheep", "water consumption per ton of milk", 
                       "water consumption per ton of meat", "average water consumption intensity")
water_ind2 <- data.frame(t(water_ind2))
colnames(water_ind2) <- c("result", "difference", "percent")

## steve edits
water_ind_val <- read.csv(paste0(path, "/1-input/parameter/water_val.csv"))
water_ind2$evaluation <- ifelse(abs(water_ind2$percent) <= water_ind_val$val, 
                                "low", ifelse((abs(water_ind2$percent) >= water_ind_val$val) & (abs(water_ind2$percent) <= 
                                                                                                  2 * water_ind_val$val), "medium", "high"))

title <- paste("Water pathway", name, sep = " ")
col <- colorRampPalette(brewer.pal(9, "Blues"))(100)
col2 <- colorRampPalette((brewer.pal(9, "Reds")))(100)

title2 <- paste("Water pathway difference to baseline", name, sep = " ")

par(mfrow = c(1, 2), mar = c(2, 4.5, 2, 6))

# pdf(paste(name,'water_pathway.pdf', sep='-')) par(mfrow=(c(2,1)))
plot(wui * 100, legend.width = 1, legend.shrink = 0.45, col = col)
plot(sarea, add = TRUE)
title(title)
plot(water_map_diff, legend.width = 1, legend.shrink = 0.45, col = col2)
plot(sarea, add = TRUE)
title(title2)
# plot(NA, xlim=c(0,100), ylim=c(0,10), bty='n', xaxt='n', yaxt='n',
# xlab='', ylab='')



############################ visualisation extract the maps for final user
title <- paste("Water pathway", name, sep = " ")
col <- colorRampPalette(brewer.pal(9, "Blues"))(100)
col2 <- colorRampPalette((brewer.pal(9, "Reds")))(100)

title2 <- paste("Water pathway difference to baseline", name, sep = " ")

pdf(paste0(path, "/4-output/",name, "-water_pathway.pdf", sep = ""))
# par(mfrow=(c(2,1)))
plot(wui * 100, legend.width = 1, legend.shrink = 0.75, col = col)
plot(sarea, add = TRUE)
title(title)
plot(water_map_diff, legend.width = 1, legend.shrink = 0.75, col = col2)
plot(sarea, add = TRUE)
title(title2)
plot(NA, xlim = c(0, 100), ylim = c(0, 10), bty = "n", xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "")
grid.table(water_ind2)

dev.off()

# tiff(paste(name,'water_pathway.tiff', sep ='-')) plot(wui*100,
# legend.width=1, legend.shrink=0.75,col=col) plot(sarea, add=TRUE)
# title(title) dev.off()

writeRaster(wui, paste0(path, "/4-output/", name, "-water_map.tif", sep = ""), overwrite = T)
write.csv(water_ind, paste(path, "/4-output/", name, "-water_ind.csv", sep = ""), row.names = F)