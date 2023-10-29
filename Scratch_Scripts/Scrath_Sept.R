##########################################################
########### Scratch Script and Workspace# ################
##########################################################





## Brainstorm for Training validation and comparisons

time = full.meta$start_time[1]
start_time = "2000-01-01 00:00:00"
tzone = full.meta[1,"tzone"]
tzone_R = full.meta[1,"tzone_R"]


or.seconds = function(time,start_time,tzone,tzone_R){
  
  # get lubridate
  require(lubridate)
  
  # add timezone into time
  start_time = paste0(start_time,tzone)
  
  # convert time into time format
  start_time = as.POSIXct(start_time,format = "%Y-%m-%d %H:%M:%S%z",tz = tzone_R)
  time = as.POSIXct(time,format = "%Y-%m-%d %H:%M:%S%z",tz = tzone_R)
  
  # days since that start date
  or_secs = as.numeric(difftime(time, start_time,units = "secs"))
  
  
  return(or_secs)
  
  
}







<<<<<<< HEAD
ct8 = function(time){
  
  
  
  
} # Converts character to time using iso8601 format in -0700 timezone




(old_rec$start_time, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")


str(meta.extract$timestamp)


old_rec$kept = "keep"
old_rec$start_time_offset = NA
old_rec$end_time_offset = NA
old_rec$prior_rec_offset = NA



## For i in 1:nrow(meta_date) followed by if(i>1) is confusing 
# why?















# check differences between directories
install.packages("sonicscrewdriver")
library(sonicscrewdriver)

"E:/PMRA_SAR/Recordings/BIRD/2022/MKBI/MKBI-01/MKBI-01-S04/CONFIG.TXT"

summaryfile = read.delim("E:/PMRA_SAR/Recordings/BIRD/2022/MKBI/MKBI-01/MKBI-01-S04/CONFIG.TXT")


configs = audiomoth_config("E:/PMRA_SAR/Recordings/BIRD/2022/MKBI/MKBI-01/MKBI-01-S04/CONFIG.TXT")
















=======
>>>>>>> 9ed1069610e7a439fbaee17ca97654ac8202904f
