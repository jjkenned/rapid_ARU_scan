#####################################################################################
########### Manipulate audio files into shorter processing schedule  ################
#####################################################################################

#### Pre-requisites for this script
## - 3_Review_recording_schedules.R
## - 


## This script is for:
## - Use sunrise sunset times to decide start and end times for new schedule
## - Use Output from processed jpgs of rec schedule review to choose which nights to convert into ldfcs





# Re-set  your script when needed
dev.off()
rm(list=ls())

# set your timezone 
Sys.setenv(TZ = "Etc/GMT+8") # "https://www.ibm.com/docs/en/cloudpakw3700/2.3.0.0?topic=SS6PD2_2.3.0/doc/psapsys_restapi/time_zone_list.htm"

# library what is needed
library(stringr)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(suncalc)
library(RODBC)
library(RSQLite)
library(terra)
library(bioacoustics)
library(exifr)

###  working spaces
# Set locations for source folder and destination folder 
# you may also want to have a temporary folder 
SourceFolder = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/recordings/2022_Nawhitti" #where your recording files are kept
OutputFolder =  "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/indices/shorter_recordings" # where saving images
project.dir = c("C:/Users/jeremiah.kennedy/Documents/PMRA/Code/rapid_ARU_scan") # directory for your R project 
schedule.timelapse.db = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/nightly_recording_schedule/Recording_Schedule_Review.ddb"
GPS.Locs = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/recording_tracking/ARU_Locations.csv"

# meta data is kept in two main files 
meta.w.pause = file.path("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/recording_tracking",
                          "2022_Nawhitti_recording_data_recs_pauses_final.csv") # meta with start and end time of recordings and pauses

meta.extract = file.path("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/recording_tracking",
                         "Full_WAMD_GUANO_EXIF.csv") # metadata extracted from recordings 


# and time settings
time_format = "%Y-%m-%d %H:%M:%S%z"


#### Prep data ####
# read in data from
meta = read.csv(file = meta.w.pause) # last script (start and end time recording data)
night.status =  DBI::dbConnect(RSQLite::SQLite(), schedule.timelapse.db)# timelapse database 



### Clean the 'nights to process data' df 
night.use = tbl(night.status,"DataTable") # specify table
night.use = data.frame(night.use %>% select(File, Status, Issues, Description)) # specify columns
 
DBI::dbDisconnect(night.status)
# Combine to filter out 'bad' recording nights
# quick prep

meta$File = basename(meta$jpg_name)# get basename jpg for merging dataframes
meta$File = gsub("Sessions_","",meta$File) # only if you have inaccurately named the JPG files like I did 

meta_2 = merge(meta,night.use, by = "File") # combine by jpg file basename
colnames(meta_2)[colnames(meta_2)=="File"] = "jpg_base" # rename vague column name

meta = meta_2[!meta_2$Status=="Bad",] # remove recordings from nights you don't want to work with at all 


# Remove recs that don't contain anything, and corresponding pauses if if they exist 
remove.empty = meta_2[!meta_2$length>0 | is.na(meta_2$length),]
remove.pause = meta_2[meta_2$status == "pause" & meta_2$end_time == remove.empty$start_time & meta_2$station == remove.empty$station,]

# now remove them
meta_2 = meta_2[!meta_2$filename %in% remove.empty$filename,]
meta_2 = meta_2[!(meta_2$station %in% remove.pause$station & meta_2$start_time %in% remove.pause$start_time),]


# create station night ID
meta_2$station_night = paste0(meta_2$station,"_",meta_2$ynight)



# loop trial settings 
#i=1

# should start by looping through ordinal nights
# Test with first iteration
#station_night = unique(meta_2$station_night)[1]





for (station_night in unique(meta_2$station_night)){
  
  meta_sn = meta_2[meta_2$station_night == station_night,]  #  filter by ynight
  timezone = as.character(unique(meta_sn$))
  meta_sn = meta_sn[order(as.POSIXct(meta_sn$start_time,format = time_format, tz = "Etc/GMT+7")),]
  
  timezone = unique(meta_sn)
  
  # get two dates involved in this recording night
  date_range = unique(date(strftime(as.POSIXct(meta_sn$start_time,format = "%Y-%m-%dT%H:%M:%S%z", tz = "Etc/GMT+7"),"%Y-%m-%dT%H:%M:%S%z")))
  
  ## return values and sources of error if we run into problems 
  
  # Issues with dates in ynight?
  if(!length(date_range)==2){cat("\n\n ---- \n\nProblem with number of nights in",station_night)} # number of dates
  
  if(!as.numeric(difftime(date_range[1],date_range[length(date_range)])) %in% c(1,-1)){
    
    cat("\n\n ---- \n\nProblem with dates in",station_night,"\n\nDates apparently not within 1 \n\nDates\n")
    
    for (date in unique(date_range)){
      
      cat(as.character(as.Date(date,format = "%Y-%m-%d")),"\n")
      
    }
    
  } # number of days separating them
  
  # check if only the one latitude and longitude values for each station night
  # latitude
  if(!length(unique(meta_sn$Latitude)) == 1){ 
    cat("\n\n ---- \n\nProblem with latitude in",station_night,"\n latitudes")
    
    for(lat in unique(meta_sn$Latitude)){
      
      cat(lat,"\n")
      
    }
    
  }
  # longitude
  if(!length(unique(meta_sn$Longitude)) == 1){
    cat("\n\n ---- \n\nProblem with longitude in",station_night,"\n longitudes")
    
    for(lon in unique(meta_sn$Longitude)){
      
      cat(lon,"\n")
      
    }
  }
  
  
  
  
  # finished error check 
  
  ## calculate times and reformat existing time variables
  # get sunrise sunset times 
  sunrise = suncalc::getSunlightTimes(date = date(max(date_range)),lat = meta_sn$Latitude[1],lon = meta_sn$Longitude[1], keep = c("sunrise"))
  sunset = suncalc::getSunlightTimes(date = date(min(date_range)),lat = meta_sn$Latitude[1],lon = meta_sn$Longitude[1], keep = c("sunset"))
  
  meta_sn$sunrisetime = strftime(with_tz(sunrise$sunrise,tzone = "Etc/GMT+7"),format = "%Y-%m-%dT%H:%M:%S%z")
  meta_sn$sunsettime = strftime(with_tz(sunset$sunset,tzone = "Etc/GMT+7"),format = "%Y-%m-%dT%H:%M:%S%z")
  
  
  
  
  # format time for use
  meta_sn$start_time_form = strftime(as.POSIXct(meta_sn$start_time,format = "%Y-%m-%dT%H:%M:%S%z", tz = "Etc/GMT+7"),"%Y-%m-%dT%H:%M:%S%z")
  meta_sn$end_time_form = strftime(as.POSIXct(meta_sn$end_time,format = "%Y-%m-%dT%H:%M:%S%z", tz = "Etc/GMT+7"),"%Y-%m-%dT%H:%M:%S%z")
 
  
  
   # Filter out recordings that are in the middle of the day and we really don't want anyway (set )
  
  # meta_sn = meta_sn %>% filter(!between(
  #   as.numeric(format(as.POSIXct(meta_sn$start_time,format = "%Y-%m-%dT%H:%M:%S%z", tz = "Etc/GMT+7"),"%H"))
  #   ,11,13)) 
  # 
  
  # calculate sunrise/sunset differences to recording/pauses 
  # ** All are as.numeric() and in seconds
  # ** Negative values indicate time between sunrise and sunset (diurnal); visa versa for possitive 
  
  suntime = meta_sn %>% 
    mutate_at(vars(start_time_form, sunrisetime), ~as.POSIXct(.x,format = "%Y-%m-%dT%H:%M:%S%z")) %>% # sunrise time
    mutate(start_time_to_sunrise =  as.numeric(seconds(sunrisetime) - seconds(start_time_form))) %>%
    
    mutate_at(vars(start_time_form, sunsettime), ~as.POSIXct(.x,format = "%Y-%m-%dT%H:%M:%S%z")) %>% # sunset time
    mutate(start_time_since_sunset = as.numeric(seconds(start_time_form) - seconds(sunsettime))) %>% 
  
    # repeat for the end of the recordings 
    mutate_at(vars(end_time_form, sunrisetime), ~as.POSIXct(.x,format = "%Y-%m-%dT%H:%M:%S%z")) %>% # sunrise time
    mutate(end_time_to_sunrise =  as.numeric(seconds(sunrisetime) - seconds(end_time_form))) %>%
    
    mutate_at(vars(end_time_form, sunsettime), ~as.POSIXct(.x,format = "%Y-%m-%dT%H:%M:%S%z")) %>% # sunset time
    mutate(end_time_since_sunset = as.numeric(seconds(end_time_form) - seconds(sunsettime)))
  
  
  
  ## Now we can make a summary for each recording period (station night) ##
  nightly_by_SN = suntime %>% group_by(station_night) %>%
    mutate(station = station) %>%
    mutate(ynight = ynight) %>%
    reframe(past.sunrise = min(end_time_to_sunrise[status=="recording"]),
              
              before.sunset = min(start_time_since_sunset[status=="recording"]),
              
              rec.start = start_time[start_time_since_sunset == min(start_time_since_sunset[status=="recording"])], 
              
              rec.end = end_time[end_time_to_sunrise == min(end_time_to_sunrise[status=="recording"])]
              
              )
  
  # Combine these two dataframes as you go
  
  if(station_night == unique(meta_2$station_night)[1]){
    
    nightly_summary = nightly_by_SN # assign summary
    
    sun_relative_meta = suntime # assign full df
    
    
    
    # 
    
  } else {
    
    nightly_summary = rbind(nightly_summary,nightly_by_SN) # combine summary
    
    sun_relative_meta = rbind(sun_relative_meta,suntime) # combine meta
    
    if (!meta_sn$station[1] == last.station){cat("\n\n ---- \n",meta_sn$station[1],"Complete\n ----")}
    
  }
  
  last.station = meta_sn$station[1] # for tracking stations completion
  
  
  
}


# now we have lots of info on this and we can save it all
# 
# write.csv(nightly_summary,
#         file = file.path("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/recording_tracking",
#           "NightSummary_Relative_Sun_Final.csv"),row.names = F)
# 
# write.csv(sun_relative_meta,
#        file = file.path("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/recording_tracking",
#           "Start_End_Times_Relative_Suntimes_Final.csv"),row.names = F)









