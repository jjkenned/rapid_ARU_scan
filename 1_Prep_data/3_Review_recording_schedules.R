###########################################################
########### Visualize recording schedules  ################
###########################################################

## This script is for:
## - Determining/describing recording schedule
## - Exploring recording schedules within a set of recordings to find inconsistencies  
##  - in time of year (date)
##  - between Stations within a transect (or other deployment type)
##  - between actual and expected recording schedule 
##  

# Re-set  your script when needed
dev.off()
rm(list=ls())

# Base folder with recordings present 
prnt.source = c("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/recordings/2022_Nawhitti")

#### First trial of this component of script ######

# set intermediate directories 
all.dirs = list.dirs(prnt.source,recursive = T,full.names = T)

