################################################################
############ Deployment and Retrieval Tracking ################
################################################################


# Re-set  your script when needed
dev.off()
rm(list=ls())

# set Timezone
Sys.setenv(TZ = "Etc/GMT+8") # "https://www.ibm.com/docs/en/cloudpakw3700/2.3.0.0?topic=SS6PD2_2.3.0/doc/psapsys_restapi/time_zone_list.htm"
Sys.timezone() # check

# library what is needed
library(stringr)
library(av)
library(seewave)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(exifr)
library(suncalc)
library(terra)
library(geosphere)

# set some directories
prnt.source = c("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Recordings/BIRD/2023/MKSC") # Base folder with recordings present 
project.dir = c("C:/Users/jeremiah.kennedy/Documents/PMRA/Code/rapid_ARU_scan") # directory for your R project 
dep.dir = "S:/Projects/107182-01/06 Data/ARU processing/Deployments/ARU Deploy_Retrieval Forms/JotForm"
csv_name = "ARU_Deployment_Retrieval2023-08-31_13_07_18.csv"

# and time settings
time_format = "%Y-%m-%d %H:%M:%S%z"


# get functions
# or.night = function(date_time,cutoff_hour,timezone){
#   
#   # get lubridate
#   require(lubridate)
#   date_time = as.POSIXct(date_time,format = time_format, tz = timezone)
#   
#   # part 1 makes datetime into yday
#   or_date = as.numeric(yday(date_time))
#   
#   # and hour
#   hour = hour(date_time)
#   
#   # part 2 compares these values
#   if (hour<cutoff_hour){or_night = or_date
#   } else {or_night = or_date + 1}
#   
#   return(or_night)
#   
#   
# }

source(paste0(project.dir,"/Sourced_Functions/Get_wamd_GUANO_4.R"))

#### Skip to ~~ Time Manipulation ~~ (approx. line 134) on subsequent runs, once completed first time

#### ~~ Extract some metadata from recordings ~~ ##### 

# Replacement for songmeter function in seewave


# List audio files
all.recs = data.frame(Full = list.files(prnt.source,recursive = T,full.names = T, pattern = ".wav")) # full names of directories present

### get meta data for recordings 
# Step 1 - extract WAMD and GUANO encoded meta data
meta_alt = plyr::mdply(all.recs$Full, # 
                       Get_wamd_guan,
                       .progress=plyr::progress_text(style = 3))


# before moving on, you may want to see what files, didnt work 
meta_alt$filename[!is.na(meta_alt$notes)]

dir.create(dirname("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Results/Tracking/BIRD/2023/MKSC/Meta_Data_Recordings.csv"),recursive = T)
# write.csv(meta_alt,file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Results/Tracking/BIRD/2023/MKSC/Meta_Data_Recordings.csv")

meta_alt = read.csv("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Results/Tracking/BIRD/2023/MKSC/Meta_Data_Recordings.csv")

## Deployment/retrieval data

# read in some deployment,retrieval data
dep_ret = read.csv(paste0(dep.dir,"/",csv_name))

# deploy/retrieve check
jot.check = dep_ret[c("Full.Station.ID","Deployment.or.Retrieval","ARU.ID")]
colnames(jot.check) = c("Dep.Station.ID","Deployment.or.Retrieval","Dep.ARU.ID")
jot.check$dep.ID = paste0(jot.check$Dep.ARU.ID,"_",jot.check$Dep.Station.ID)


meta.chk = meta_alt[c("filename","original_filename","prefix","model","serial_number")]

colnames(meta.chk) = c("Full.Rec","original.filename","meta.prefix","model","true.ARU.ID")

meta.chk$dep.ID = paste0(meta.chk$true.ARU.ID,"_",meta.chk$meta.prefix)

meta.out = meta.chk %>% group_by(meta.prefix,true.ARU.ID,model) %>% summarise(recs = n())

# Combine

crosscheck = merge(meta.chk,jot.check,by = 'dep.ID')

write.csv(crosscheck,file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Results/Tracking/BIRD/2023/MKSC/Crosscheck_ARU_Deployments.csv")
write.csv(meta.out,file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Results/Tracking/BIRD/2023/MKSC/station_rec_meta.csv")
write.csv(jot.check,file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Results/Tracking/BIRD/2023/MKSC/Dep_retr_summary.csv")



















# 
# 
# # Now let's see what we got 
# depl = dep_ret[dep_ret$Deployment.or.Retrieval=="Deployment",] %>% group_by(Full.Station.ID,ARU.ID) %>% count(Full.Station.ID,ARU.ID)
# 
# #### Check specific transect for Full retrieval 
# 
# transects = c("VI-24 (Tsitika Lower)","VI-21 (Schoen Lake)") # set transect
# 
# trans = dep_ret[dep_ret$Transect %in% transects,] %>% 
#   group_by(Deployment.or.Retrieval,Full.Station.ID,ARU.ID) %>% 
#   count(Deployment.or.Retrieval,Full.Station.ID,ARU.ID)
# 
# # Save the data for tracking
# write.csv(trans,paste0(direct,"/","Tsitika_Schoen_Check.csv"),row.names = F)
# 
# 
# 



























