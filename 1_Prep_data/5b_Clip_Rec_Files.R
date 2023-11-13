
#######################################################################################
########### Clip Files from new names and proposed sub-schedule w sox  ################
#######################################################################################

#### Pre-requisites for this script
## - 5a_clip_longer_rec_files



## This script is for:
## - Clipping recordings to new schedule from base schedule and save in new location for use


# Re-set  your script when needed
dev.off()
rm(list=ls())

# set your timezone 
Sys.setenv(TZ = "Etc/GMT+8") # "https://www.ibm.com/docs/en/cloudpakw3700/2.3.0.0?topic=SS6PD2_2.3.0/doc/psapsys_restapi/time_zone_list.htm"
Sys.timezone() # check
options(stringsAsFactors = FALSE)
time_format = "%Y-%m-%d %H:%M:%S%z"


# library what is needed
library(stringr)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(suncalc)
# detach("package:terra",unload = T)





####  working spaces
# set database


####  working spaces
# csv file locations
meta.path = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/MetaData"
meta.files = list.files(meta.path,full.names = F,recursive = T,pattern = ".csv")
meta.base = "20231111_New_rec_Names_only_keep.csv"

# output locations
out.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/clipped_recordings"

# set database
db.path = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/MetaData"
db.name = "20231106_WLRS_Meta_Database.sqlite"
jpg.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/recording_sessions/"



# Read data in
meta = read.csv(file.path(meta.path,meta.base),stringsAsFactors = F)


### Prep data for loop
## new name is missing underscore
meta$use.name = gsub(" ","_",meta$new_name)
meta$use.name = gsub(":","",meta$use.name)
meta$use.name = sub("-","",meta$use.name)
meta$use.name = sub("-","",meta$use.name)


meta$orig.base = basename(meta$filename)
meta$full.use = file.path(out.dir,basename(dirname(meta$filename)),meta$use.name)

### make new metadata file to keep only the data to use, should speed up the script
meta.use = meta # [c("station","filename","use.name","start_time","full.use")]

## testing settins


i = 1

for (i in 1:nrow(meta.use)){
  
  
  # define recordings
  rec.old = meta.use$filename[i]
  rec.new = meta.use$full.use[i]
  
  if(!file.exists(dirname(rec.new))){dir.create(dirname(rec.new),recursive = T)}
  
  total.offset = as.numeric(strptime(meta$new_start_time[i],format = time_format, tz = meta$tzone_R[i]) -
                                   strptime(meta$full_rec_start_time[i],format = time_format,tz = meta$tzone_R[i]),units = "secs")
  
  
  # create command
  command.sox = paste0(rec.old," ",rec.new," trim ",total.offset," ",meta.use$new_length[i])
  
  
  system2("sox",
          args = command.sox)
  
  
  
  
}





























