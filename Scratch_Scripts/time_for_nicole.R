##############################################################
########### Managing time and timezones in R  ################
##############################################################



# Re-set  your script when needed
dev.off()
rm(list=ls())



# set your timezone 
Sys.setenv(TZ = "Etc/GMT+7") # "https://www.ibm.com/docs/en/cloudpakw3700/2.3.0.0?topic=SS6PD2_2.3.0/doc/psapsys_restapi/time_zone_list.htm"
Sys.timezone() # check
# options(stringsAsFactors = FALSE)
time_format = "%Y-%m-%d %H:%M:%S%z"


# install.packages(c("stringr","tidyverse","lubridate"))

# library what is needed
library(stringr)
library(tidyverse)
library(lubridate)

lubridate::seconds()
chron::seconds()


####  working spaces
# Set locations for source folder and destination folder 
# 

source.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Results/Nicole"
db.name = "2022_MKVI_Results.csv"

## Read in data

meta = read.csv(file.path(source.dir,db.name),stringsAsFactors = F)


# timezone extract
pos.time = as.POSIXct(meta$date_time[1],format = time_format,tz = "Etc/GMT+7")

sttime = strptime(meta$date_time[1],format = time_format,tz = "Etc/GMT+7")


### ~~~~~~~~~~~~~~ ###

time = meta$date_time[1]
get = "timezone"


# Reformat Timezone and timestamp values 
### Only if needed
reform_tz = function (time,get){
  
  if(grepl("+",time,fixed = T)){
    
    zone_pre = getElement(unlist(strsplit(time,"+")),
                          length(unlist(strsplit(time,"+"))))
    sign = "+"
    
    
  } else {zone_pre = getElement(unlist(strsplit(time,"-")),
                                length(unlist(strsplit(time,"-"))))
  
  sign = "-"
  
  }
  
  
  
  zone_true = as.character(paste0(sign,sprintf("%04d",as.numeric(zone_pre))))
  
  
    
  
    return(zone_true)
    
  
  
  
}


meta$timezone = lapply(meta$date_time,FUN = reform_tz, get = "timezone")



## Create the R-recognized timezone value
# only deals with whole hour timezones, unsure how to format half hour timezones

R_timezone = function(timezone){
  
  if(substr(timezone,1,1)=="-"){
    
    sign = "+"
    
    
  } else if(substr(timezone,1,1)=="+"){
    
    sign = "-"
    
  } else {
    
    stop ("No timezone offset +/- sign")
    
  }
  
  
  r_form = paste0("Etc/GMT",sign,as.numeric(substr(timezone,2,3)))
  
  return(r_form)
  
  
  
}

meta$tzone_R = lapply(meta$timezone,FUN = R_timezone)





test = strptime(meta$date_time[1],format = time_format,tz = meta$tzone_R[1])


test.character = strftime(test,format = time_format,tz = meta$tzone_R[1])















