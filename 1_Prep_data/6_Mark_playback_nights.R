##################################################################
########### Playback night assessment and removal ################
##################################################################


## This is basically the first script dealing with recordings in their final storage/processing locations

## This script is for:
# ~ importing and comparing nights with point counts (playback) to those chosen for processing
# ~ removing these nights from the processing options in RTS formation

## SQLite in R
# download sqlite for appropriate OS (https://sqlite.org/download.html) 
# For windows, find "A bundle of command-line tools for managing SQLite database files"


## Resets ## 
# Reset your script when needed
dev.off()
rm(list=ls())

# set Timezone
Sys.setenv(TZ = "Etc/GMT+7") # "https://www.ibm.com/docs/en/cloudpakw3700/2.3.0.0?topic=SS6PD2_2.3.0/doc/psapsys_restapi/time_zone_list.htm"
Sys.timezone() # check
options(stringsAsFactors = FALSE)
time_format = "%Y-%m-%d %H:%M:%S%z"

## packages 
library(readxl)



## set directories

# playback data
pb.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/MetaData/playback_visits"
pb.file = "2022.FLNR.WESO.ARU.CPB.Data.xlsx"


# ldfcs data
ldfcs.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/Timelapse_files/LDFCS"
ldfcs.file = "IndicesProcessing4.ddb"



############################
##### ~~~~ PART 1 ~~~~ #####
##### format pb data #######
############################

## Read in playback timing data
dat.pb = readxl::read_excel(file.path(pb.dir,pb.file),sheet = "2022 CPB Data")


## group by date and station 
dat.visits = dat.pb %>% group_by(TRANSECT,`SURVEY VISIT`) %>% summarize (visit.date = min(DATE))

















system.time(
  {files.new = list.files("//auscorp/dfs/CA/Burnaby/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/clipped_recordings/SMA00101")
 }
)



system.time(
  {files.old = list.files("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/clipped_recordings/SMA00101")
  }
)













