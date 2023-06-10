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

# library what is needed
library(stringr)

# Base folder with recordings present 
prnt.source = c("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/recordings/2022_Nawhitti")

#### First trial of this component of script ######

# set intermediate directories
## IMPORTANT NOTE: This won't work if you don't at least have all recordigns kept in the same depth of directories throughout this parent directory 

all.dirs = data.frame(Full = list.dirs(prnt.source,recursive = T,full.names = T)) # full names of directories present
all.dirs$depth <- lengths(strsplit(all.dirs$Full, "/")) # get directory depth 
all.dirs = all.dirs[all.dirs$depth == max(all.dirs$depth),]# keep only the deepest directories  
all.dirs$int.dirs = gsub(prnt.source,"",all.dirs$Full)
all.dirs$int.length = str_count(all.dirs$int.dirs,"/")


### C




