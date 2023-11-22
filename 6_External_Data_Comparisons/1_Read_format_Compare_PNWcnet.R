##########################################################################
########### Read Results from Running PNWcnet and review  ################
##########################################################################


## This script is for:
## - reading in data from PNWcnet output


# Re-set  your script when needed
dev.off()
rm(list=ls())

# set your timezone 
Sys.setenv(TZ = "Etc/GMT+8") # "https://www.ibm.com/docs/en/cloudpakw3700/2.3.0.0?topic=SS6PD2_2.3.0/doc/psapsys_restapi/time_zone_list.htm"
Sys.timezone() # check
options(stringsAsFactors = FALSE)
time_format = "%Y-%m-%d %H:%M:%S%z"


# library what is needed
library(tidyverse)
library(lubridate)



####  working spaces
# set results location

pnw.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/PNWcnet_results/CNN_v3_review_kscope_files_for_NVI2022"
pnw.files = data.frame(Full = list.files(pnw.dir,full.names = T,recursive = F,pattern = ".csv"))

#### loop through files to get data
pnw.res = list()
for(i in 1:nrow(pnw.files)){
  
  # read in data
  pnw.res[[i]] = data.frame(read.csv(pnw.files$Full[i],stringsAsFactors = F))
  
}


# group together into one dataframe
pnw.dat = do.call("rbind",pnw.res)


### filter by results
conf.weso = pnw.dat[grep("WESO",pnw.dat$MANUAL.ID,fixed = T),]

detect.weso = pnw.dat[grep("MEKE",pnw.dat$TOP1MATCH,fixed = T),]

# let's see what we have for playback only 

















