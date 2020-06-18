# CLEANED R code for Atsbi, Tigay, Ethiopia (ReSLess project) 

This folder contains all codes and data necessary to run CLEANED as a stand alone on a computer. The code corresponds to the two shiny tools : 
* The simplified version used in the workshop (https://ilri.shinyapps.io/cleaned-r-resless-atsbi_eth/)
* the expert interface (https://ilri.shinyapps.io/cleaned-r-resless-atsbi_eth_ex/). 

## Structure of the tool 
The tool consistens of 3 folders that need to be created on the computer 
* core code of CLEANED direcly available
* 1-input : this folder contains all the data to be downloaded from https://drive.google.com/open?id=1aDR69hFNhCla7XKAqPfGZUOgjjifbCEH
* 4-output : this folder is used to save the output of the CLEANED tool 
* www : contains the images that are loaded into the R-shiny tool 

## Core codes 
* interface in shiny: click on run app in R to start the interface comes in 3 versions 
  + based on presets for the transformation game (preset/vignettes) in English
  + expert version
* 0-user definition (manual interface)
* 2-feedbasket_nonlinear2 : productivity computations
* 2-load data : loading data from 1-input
* 2-luccomp : the land use module
* 1-water : water impact
* 1-ghg : greenhouse gas emmissions
* 1-biodiv : biodiversity impact
* 1-soil : soil impact

To run the r-shiny stand alone version on your computer make sure that you have the following packages downloaded : 
shiny, shinydashoared, shinyjs, graphics, rgeos, sp,raster, maptools rgdal, gridExtrea, grid, RColorBrewer.

This code was developped by Catherine Pfeifer (c.pfeifer@cgiar.org) at the International Livestock Research Institute  (www.ilri.org) and Joanne Morris Stockholm Environmental Institute (www.sei.org) based in York. 
The shiny interface was developed by Victor Moses from LocateIT Kenya (http://www.locateit.co.ke/) 

CLEANED parametrization for Atsbi, Tigray, Ethiopia has been funded by the SAIRLA program of DFID as well as the livestock CRP (and formely by the livestock and fish CRP).The program thanks all donors and organizations which globally supported its work through their contributions to the CGIAR system.


Related publications 
up-coming
