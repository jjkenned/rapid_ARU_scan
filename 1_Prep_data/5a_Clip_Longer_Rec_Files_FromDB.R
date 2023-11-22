#####################################################################################
########### Manipulate audio files into shorter processing schedule  ################
#####################################################################################

#### Pre-requisites for this script
## - 3_Review_recording_schedules.R 
## - 4_Prep_Clipping_Longer_Recordings_Sunlight_Times.R


## This script is for:
## - Setting new schedule to clip with from base rec schedule
## - Clip recordings to new schedule from base schedule and save in new location for use

# debugonce(View)


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
db.path = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/MetaData"
db.name = "20231106_WLRS_Meta_Database.sqlite"
jpg.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/recording_sessions/"


#### set functions

source("sourced_Functions/Or_Night_20000101_Function.R")
source("sourced_Functions/Or_Seconds_20000101_Function.R")


get_end_time = function(start,format,timezone,length){
  
  end = strptime(start, 
                 format = format, tz = timezone)+ceiling(lubridate::seconds(length))
  end = strftime(end, format = format, tz = timezone)
  
  return(end)
  
}


# Make quick function to format dates and times quicker
ct = function(time,time_format,tz){

  strptime(time, format = time_format ,tz = tz)


} # Converts character to time using iso8601 format in -0700 timezone

tc = function(time,time_format,tz){

  strftime(time, format = time_format, tz = tz)

} # Converts time to character using iso8601 format in -0700 timezone

in_out_time = function(func,time,time_format,tz){

  time = strptime(time, format = time_format, tz = tz)

  time_value = func(time)

  out = strftime(time_value, format = time_format, tz = tz)

  return(out)

} # Converts string to time, applies function, then converts resulting time back to character, using iso8601 format in -0700 timezone



#########~~~~~~~ Section 1 ~~~~~~~~##########
### design recording schedule

## Read in data
# database conenct
meta.db = dbConnect(RSQLite::SQLite(),file.path(db.path,db.name))
meta = dbReadTable(meta.db,"metadata") # read the table


dbDisconnect(meta.db)




### Start setting presets for processing and functions

date_org = "ynight" # name of the column with the date organizer
# this should be Julian date (daysw from any starting point) or ordinal date (days from jan 1st of active year)
# For nocturnal recordings this can be the same as above but for nocturnal time periods where 1200hrs is the date shift
station_org = "station" # Column name with station IDs


# time (in seconds) (relative to sunrise/sunset) when recordings should start/end on a given night for new recording schedule
# "-" indicates recording is away from focused dial window (eg. if dial_status = nocturnal, "-" for diurnal recordings and "+" for nocturnal recordings )
# for recordings between 1200hrs and 2359hrs, start and end times will be relative to sunset
# for recordings between 0000hrs and 1159hrs, start and end times will be relative to sunrise  
start_time = -5400 
end_time = -5400
rec_length = 180 # length of recordings (in seconds) for final recording schedule
pause_length = 720 #  length of pauses between recordings (in seconds) for final recording schedule
dial_focus = "nocturnal" # either "diurnal" or "nocturnal" 
# Whether the main window of recording is diurnal or nocturnal
# if unsure, use diurnal, as nocturnal requires shifting ordinal date to ordinal night
min_pause = 540 # minimum pause length between recordings (seconds) 9mins base

#### ~~~ other options
## These settings are not required but can help space out subrec selections
# These are the maximum number of sub recordings (n_max) for each of the following full recordings lengths (f_max) in minutes
n_max_15 = 2 # 0-15min
n_max_30 = 2 # 16-30min
n_max_45 = 3 # 31-45min
n_max_60 = 4 # 46-60min

# These are the minimum length of pauses (p_min) for each of the following full recording lengths (f_max) in minutes
p_min_15 = 9  # 0-15min
p_min_30 = 9 # 16-30min
p_min_45 = 12 # 31-45min
p_min_60 = 12 # 46-60min

# additionally, if you are wanting to run this with different time bins (ie_ bins that aren't separated by 15 mins) 
# you can define values using the same format (ie_ p_min_## or n_max_##) and set the separator value to the difference between these bins
# Here it is 15 because the windows are 15 mins (actually 14, but this is included in the script)
separator = 15 # 15 minutes



# testing 
pars = list(n_max_15 = 2,
            n_max_30 = 2,
            n_max_45 = 3,
            n_max_60 = 4,
            p_min_15 = 9,
            p_min_30 = 9, 
            p_min_45 = 12,
            p_min_60 = 12)




# calculate the min length of recordings to contain given number of subrecordings 
# given the parameters set


# n = number of subrecordings per recording
# f = length of larger recording to subset from (full rec)
# s = length of subrecordings
# p = length of pauses


get_n = function(f,s,p){
  
  floor((f+p)/(s+p))
  
  
}




# set values to check 
# Start with max value of number of recordings per main recording 
p_min = min_pause
p_max = pause_length
f_min = rec_length
f_max = max(meta$length)



p_f_combos = tibble(expand_grid(p_choices = seq(p_min,p_max,60), f_choices = seq(f_min,f_max,60)))

# fill in the number of rec sessions (n)

p_f_combos$n_recs = mapply(get_n, p_f_combos$f_choices,rec_length,p_f_combos$p_choices)

# now we have a reference list to check how many subrecs(s) we want from each large rec (f)
# based on the pause length variation and f length variation 
# visualize using minutes instead 
p_f_combos$fminute = p_f_combos$f_choices/60
p_f_combos$pminute = p_f_combos$p_choices/60

# check and apply p.min and n.max values for each time frame applied
# pars = list(...) # list parameters

keeping = do.call(rbind, pars) # combine list

pars = data.frame(value = keeping) # dataframe and naming value column

pars$name <- lapply(row.names(pars), '[[', 1)  # move row name into it's own parameter ID column



# pars = tester
# pars$name = gsub(".","_",pars$name)



# create upper and lower seconds bounds for filtering
pars$upper = as.numeric(sapply(
  strsplit(as.character(pars$name),"_")
  , getElement, 3))*60


pars$lower = pars$upper-((separator)*60)+1

pars$by.type = sapply(
  strsplit(as.character(pars$name),"_")
  , getElement, 1)



# Apply optional filters within loop


# full = 2340


for (full in p_f_combos$f_choices){
  
  combo.by.f = p_f_combos[p_f_combos$f_choices==full,]
  combo.by.f$p.keep = NA
  combo.by.f$n.keep = NA
  
  
  # check which bins the recordings fit in 
  
  param_check = pars[pars$upper>=full & pars$lower<=full,]
  
  
  if(nrow(param_check)>2){cat("too many pars in",full)}
  
  pars_max_n = param_check[param_check$by.type=="n",]
  pars_min_p = param_check[param_check$by.type=="p",]
  
  #i=1
  for (i in 1:nrow(combo.by.f)){
    #j=1
    
    
    
    if(combo.by.f$f_choices[i]>=pars_max_n$lower & 
       combo.by.f$f_choices[i]<=pars_max_n$upper &
       combo.by.f$n_recs[i]>pars_max_n$value){
      
      combo.by.f$n.keep[i] = "remove"
      
    } else(combo.by.f$n.keep[i] = "keep")
    
    
    
    # j=3
    
    
    
    
    if(combo.by.f$f_choices[i]>=pars_min_p$lower & 
       combo.by.f$f_choices[i]<=pars_min_p$upper &
       combo.by.f$p_choices[i]<(pars_min_p$value*60)){
      
      combo.by.f$p.keep[i] = "remove"
      
    } else(combo.by.f$p.keep[i] = "keep")
    
    
    
  }
  
  combo_keepers = combo.by.f[combo.by.f$p.keep=="keep" & combo.by.f$n.keep=="keep",]
  
  if(nrow(combo_keepers)==0){print(combo.by.f)}
  
  if(nrow(combo_keepers)>1){
    
    combo_keepers = combo_keepers[combo_keepers$n_recs == max(combo_keepers$n_recs),]
    
    
  }
  
  if(nrow(combo_keepers)>1){
    
    combo_keepers = combo_keepers[combo_keepers$p_choices == max(combo_keepers$p_choices),]
    
  }
  
  if(nrow(combo_keepers)>1){cat("still too many keepers",combo_keepers$f_choices)}
  
  if (full == unique(p_f_combos$f_choices)[1]){
    
    combo_keep = combo_keepers
    
  } else (combo_keep=rbind(combo_keep, combo_keepers))
  
  
  
  
}

## let's prep this data for the next steps
# ordinal values

# apply ordinal night function
for (i in 1:nrow(meta)){
  
  meta$ynight[i] = or.night(meta$form_tstamp[i],12,"2000-01-01",meta$tzone_R[i])
  
  
}

## Create real station ID for looping, based on correct names and not the (potentially incorrect) prefix field 
# Thankfully we've done this earlier, and just need to rename
colnames(meta)[colnames(meta)=="real_prefix"] <- c("station")




# new column for recording status 
meta$status = "recording"


# Create an end time for each recording 
meta$end_time = NA

# test settings
# start = meta_2$form_tstamp[i]
# format = "%Y-%m-%d %H:%M:%S%z"
# timezone = meta_2$tzone_R[i]
# length = meta_2$calc_length[i]
# 



#i=1
for (i in 1:nrow(meta)){
  
  meta$end_time[i] = get_end_time(meta$form_tstamp[i],"%Y-%m-%d %H:%M:%S%z",meta$tzone_R[i],meta$calc_length[i])
  
  
}

colnames(meta)[colnames(meta)=="form_tstamp"] = "start_time"

# get ordinal seconds for sorting

for (i in 1:nrow(meta)){
  
  meta$or.secs[i] = or.seconds(meta$start_time[i],"2000-01-01 00:00:00",meta$tzone[i],meta$tzone_R[i])
  
  
}

# set length of recordings to numeric
meta$length = as.numeric(meta$length)


# Now that we have a combination of values for setting standard time frames for clipping full audio
# into new, smaller files, compare these to the main values

sink("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/MetaData/20231109_Renaming_Tracking.txt")






# Dataframe meta
#i=2

station=unique(meta$station)[1]
meta_stn = meta[meta$station==station,]
night = unique(meta_stn$ynight)[1]



for (station in unique(meta$station)){
  
  meta_stn = meta[meta$station==station,]
  
  for (night in unique(meta_stn$ynight)){
    
    meta_date = meta_stn[meta_stn$ynight==night,]
    meta_date = meta_date[order(meta_date$or.secs),] # sort by start time just incase
    
    timezone_R = meta_date$tzone_R[1]
    

    ## calculate times and reformat existing time variables
    # get date range
    # get two dates involved in this recording night
    date_range = unique(date(strptime(meta_date$start_time, format = time_format ,tz = timezone_R)))
    
    # get sunrise sunset times 
    sunrise = suncalc::getSunlightTimes(date = date(max(date_range)),lat = meta_date$int_lat[1],lon = -1*meta_date$int_lon[1], keep = c("sunrise"))
    sunset = suncalc::getSunlightTimes(date = date(min(date_range)),lat = meta_date$int_lat[1],lon = -1*meta_date$int_lon[1], keep = c("sunset"))
    
    meta_date$sunrisetime = strftime(with_tz(sunrise$sunrise,tzone = timezone_R),format = time_format,tz = timezone_R)
    meta_date$sunsettime = strftime(with_tz(sunset$sunset,tzone = timezone_R),format = time_format, tz = timezone_R)
    
    ###### insert
 
    
    suntimes = meta_date %>% 
      mutate_at(vars(start_time, sunrisetime), ~ as.POSIXct(.x,format = time_format,tz = timezone_R)) %>% # sunrise time
      mutate(start_time_to_sunrise =  as.numeric(lubridate::seconds(sunrisetime) - lubridate::seconds(start_time))) %>%
      
      mutate_at(vars(start_time, sunsettime), ~ as.POSIXct(.x,format = time_format,tz = timezone_R)) %>% # sunset time
      mutate(start_time_since_sunset = as.numeric(lubridate::seconds(start_time) - lubridate::seconds(sunsettime))) %>% 
      
      # repeat for the end of the recordings 
      mutate_at(vars(end_time, sunrisetime), ~as.POSIXct(.x,format = time_format,tz = timezone_R)) %>% # sunrise time
      mutate(end_time_to_sunrise =  as.numeric(lubridate::seconds(sunrisetime) - lubridate::seconds(end_time))) %>%
      
      mutate_at(vars(end_time, sunsettime), ~as.POSIXct(.x,format = time_format,tz = timezone_R)) %>% # sunset time
      mutate(end_time_since_sunset = as.numeric(lubridate::seconds(end_time) - lubridate::seconds(sunsettime)))
    

    ###############
    
    meta_date$start_time_to_sunrise = suntimes$start_time_to_sunrise
    meta_date$start_time_since_sunset = suntimes$start_time_since_sunset
    meta_date$end_time_to_sunrise = suntimes$end_time_to_sunrise
    meta_date$end_time_since_sunset = suntimes$end_time_since_sunset
    
    
    
    # let's start with a tracking file and an output file
    track_out = list()
    new_recs = list()
    
    # i=2
    for (i in 1:nrow(meta_date)){
      
      
      
      # subset data
      old_rec = meta_date[i,]
      
      # reset offset
      offset = 0
      
      
      #### Section 1 ~~ Trim to pre-set sunrise/sunset based start and end times ####
      start_since_sunset = old_rec$start_time_since_sunset
      start_to_sunrise = old_rec$start_time_to_sunrise
      end_since_sunset = old_rec$end_time_since_sunset
      end_to_sunrise = old_rec$end_time_to_sunrise
      
      
      
      
      # track what you do 
      old_rec$kept = "keep"
      old_rec$removal_reason = NA
      old_rec$start_time_offset = NA
      old_rec$end_time_offset = NA
      old_rec$prior_rec_offset = NA
      old_rec$full_rec_start_time = old_rec$start_time
      
      
      
      
      
      
      # Dont include if they both start and end before start time
      if(start_since_sunset<start_time & end_since_sunset<start_time){
        
        cat("\n\n ~~~~~~~~ \n\n","Recording occurs before start of recording period ~ Skipping","\n\n",old_rec$station,"\n From ",old_rec$start_time,"\n To ",old_rec$end_time,"\n",old_rec$filename)
        
        old_rec$kept = "removed"
        
        old_rec$removal_reason = "Recording occurs before start of recording period"
        
        track_out[[i]]=old_rec 
        
        next
        
      }
      
      # Dont include if they both start and end after end time
      if(start_to_sunrise<end_time & end_to_sunrise<end_time){
        
        cat("\n\n ~~~~~~~~ \n\n","Recording occurs after end of recording period ~ Skipping","\n\n",old_rec$station,"\n From ",old_rec$start_time,"\n To ",old_rec$end_time,"\n",old_rec$filename)
        
        old_rec$kept = "removed"
        
        old_rec$removal_reason = "Recording occurs after end of recording period"
        
        track_out[[i]]=old_rec
        
        next
        
      }
      
      # if recording starts before start time and ends after start time
      # trim the start of recording 
      if(start_since_sunset<start_time & end_since_sunset>start_time){
        
        offset = start_time - start_since_sunset
        old_rec$start_time = in_out_time(func = function(x) offset+x, time = old_rec$start_time,time_format = time_format, tz = timezone_R)
        old_rec$length = old_rec$length-offset
        old_rec$start_time_offset = offset
        
        
        
        
        
      } 
      
      # if recording starts before end time and ends after end time
      # trim the end of recording 
      
      if (start_to_sunrise>end_time & end_to_sunrise<end_time){
        
        offset = end_time - end_to_sunrise
        old_rec$end_time = in_out_time(func = function(x) x-offset,time = old_rec$end_time,time_format = time_format, tz = timezone_R)
        old_rec$length = old_rec$length-offset
        old_rec$end_time_offset = offset
        
        
        
        
      }
      
      
      #### Section 2 ~~ offset the start of any recordings that may be too close to the last ####
      # take the end of the last subset
      # we are using anything after the first record to do this because the end time of the last subrecording from the past full recording
      # is calculated by record i-1
      
      
      if(i>1){
        
        last_rec_end = newend
        
        if(ct(old_rec$start_time[1],time_format,timezone_R)<last_rec_end+min_pause & !track_out[[i-1]]$kept == "removed"){
          
          cat("\n\n ~~~~~~~~ \n\n","Last sub rec too close to start of recording ~ removing...","\n\n",
              new_recs[[i-1]]$station[[nrow(new_recs[[i-1]])]],"\n From ",
              new_recs[[i-1]]$new_start_time[[nrow(new_recs[[i-1]])]],"\n To ",
              new_recs[[i-1]]$end_time[[nrow(new_recs[[i-1]])]],"\n",
              new_recs[[i-1]]$filename[[nrow(new_recs[[i-1]])]])
          
          new_recs[[i-1]]$kept[[nrow(new_recs[[i-1]])]] = "removed"
          new_recs[[i-1]]$removal_reason[[nrow(new_recs[[i-1]])]] = "too close to next recording start"
          
          
        }
        
        
      }
      
      #### Section 3 ~~ Remove recordings that are now too short ###### 
      if(old_rec$length<rec_length){
        
        cat("\n\n ~~~~~~~~ \n\n","Recording too short ~ Skipping","\n\n",old_rec$station,"\n From ",old_rec$start_time,"\n To ",old_rec$end_time,"\n",old_rec$filename)
        
        old_rec$kept = "removed"
        old_rec$removal_reason = "Full recording too short for subsetting"
        
        track_out[[i]]=old_rec 
        
        next
        
      }
      
      
      
      #### Section 4 ~~ Check number of sessions to be fit into longer recordings ####
      
      # FInd recording length value in list of chunking choices
      sub_pattern = combo_keep[combo_keep$f_choices ==
                                 floor(old_rec$length/60)*60,]
      
      if(!nrow(sub_pattern)==1){cat("too many choices for",old_rec$filename)}
      
      # start while loop through number of recs 
      
      n=0
      while(n<=sub_pattern$n_recs){
        
        # make a new name and corresponding offset and ending offset from beginning of recording
        rec_time = ct(old_rec$start_time,time_format,timezone_R)
        
        offset = n*sub_pattern$p_choices
        
        newstart = rec_time+offset
        
        # end time calculations 
        newend = newstart + rec_length
        oldend = ct(old_rec$end_time, time_format = time_format,tz = timezone_R)
        
        if(newend>oldend){
          
          newend = oldend
          
        }
        
        newtime = strftime(newstart, format = time_format, tz = timezone_R)
        
        
        
        newname = paste0(old_rec$station,"_",newtime,".wav")
        
        sub_keeper = old_rec
        
        sub_keeper$new_start_time = strftime(newstart, format = time_format, tz = timezone_R)
        
        sub_keeper$new_name = newname
        
        sub_keeper$clip_offset = offset
        
        sub_keeper$new_end = newend
        
        newlength = as.numeric(newend - newstart,units = "secs")
        
        if(newlength<=0){
          
          sub_keeper$kept = "removed"
          
        }
        
        
        sub_keeper$new_length = newlength
        
        if (n==0){
          
          new_names_per_rec = sub_keeper
          
          
        } else(new_names_per_rec = rbind(new_names_per_rec,sub_keeper))
        
        
        
        n=n+1 
        
      }
      
      
      new_recs[[i]] = new_names_per_rec
      track_out[[i]] = old_rec
      
    }
    
    
    # save results from tracking file by date
    if(night == unique(meta_stn$ynight)[1]){
      
      track_date = do.call("rbind",track_out)
      new_name_date = do.call("rbind",new_recs)
      
      
    } else{
      
      
      track_date = rbind(track_date,do.call("rbind",track_out))
      new_name_date = rbind(new_name_date, do.call("rbind",new_recs))
      
    }
    
    
    
    
  }
  
  # save results from tracking file by station
  if(station == unique(meta$station)[1]){
    
    track_stn = track_date
    new_name_stn =new_name_date
    
    
  } else{
    
    
    track_stn = rbind(track_stn,track_date)
    new_name_stn = rbind(new_name_stn, new_name_date)
    
  }
  
}

sink()




## now visualize how that went
# Run same recording schedule visualization script used earlier

# check removed
check = track_stn[!track_stn$kept == "keep",]

meta_2 = new_name_stn[new_name_stn$kept=="keep",]



## Save the results
write.csv(new_name_stn, file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/MetaData/20231111_All_new_rec_Names_meta.csv",row.names = F)

write.csv(meta_2, file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/MetaData/20231111_New_rec_Names_only_keep.csv",row.names = F)



make.jpgs="make"

## trial setttings for loop
station = unique(meta_2$station)[1]
meta_stn = meta_2[meta_2$station == station,]
ynight = unique(meta_stn$ynight)[1]



for (station in unique(meta_2$station)){
  
  # subset by station
  meta_stn = meta_2[meta_2$station == station,]
  
  ## 
  
  # now loop through nights 
  # test with 1 first
  
  
  for (ynight in unique(meta_stn$ynight)){
    
    # subset by ynight
    meta_night = meta_stn[meta_stn$ynight == ynight,]
    timezone_R = meta_night$tzone_R[1]
    
    
    
    
    # loop through the recordings on the night
    # i=1
    for (i in 1:nrow(meta_night)){
      
      # new two df with data to keep for each session
      meta_rec = meta_night[i,]
      meta_rec$new_end = strftime(meta_rec$new_end,format = time_format,tz = timezone_R)
      
      
      
      
      if (i == nrow(meta_night)){
        
        session_rec = meta_rec
        
      } else {
        
        # now use the subsequent recording entry to get end time and length for pause
        
        meta_pause = meta_rec # create record of pause
        meta_pause$filename = NA
        meta_pause$status = "pause" # change status to pause
        meta_pause$new_start_time = meta_rec$new_end # change start time to end time of recording
        meta_pause$new_end = meta_night[i+1,]$new_start_time # get start and end
        
        if (strptime(meta_pause$new_end,format = time_format,tz = timezone_R)>strptime(meta_pause$end_time,format = time_format,tz = timezone_R)){
          
          meta_pause$new_end = meta_pause$end_time
          
          
        }
        
        
        meta_pause$new_length = as.numeric(lubridate::seconds(strptime(meta_pause$new_end,format = time_format,tz = timezone_R)) -
                                            lubridate::seconds(strptime(meta_pause$new_start_time,format = time_format,tz = timezone_R)),units = "secs") # length of pause
        
        # Now combine these two into a rec_pause dataframe
        session_rec = rbind(meta_rec,meta_pause)
        
        
        
      }
      
      
      # combine rec pauses into nightly dataframe
      if (i == 1){
        
        session_night = session_rec
        
      } else(session_night = rbind(session_night,session_rec)) 
      
      
      
      
    } 
    
    # create jpg name for saving and also using during jpg creation
    dir.stn = file.path(jpg.dir,station) # directory build
    jpg.base = paste0(station,"_",ynight,".jpg")
    jpg.name = file.path(dir.stn,jpg.base) # create jpg full name
    session_night$jpg_name = jpg.name # fill df with jpg name
    
    
    # Combine nightly recordings into station DF
    if (ynight == unique(meta_stn$ynight)[1]){
      
      session_station = session_night
      
    } else (session_station = rbind(session_station,session_night))
    
    #########################
    # now, let's make figures to review here too
    
    # new df to plot with
    toplot = session_night
    toplot$yaxis = 1
    toplot$new_length = as.numeric(toplot$new_length)
    
    # set sunrise and sunset times
    sunset = toplot$sunsettime[1]
    sunrise = toplot$sunrisetime[1]
    
    # if you want to run this section set 'make.jpgs' to 'make'
    
    if (make.jpgs=="make"){
      
      
      
      if(!file.exists(dirname(jpg.name))){
        
        dir.create(dirname(jpg.name),recursive = T)
        
      }
      
      
      jpeg(filename = jpg.name,width = 1400, height = 800,res = 200)
      
      # start plot
      
      
      
      plot = ggplot(toplot,aes(x = as.POSIXct(new_start_time,format = time_format,tz = timezone_R),y = yaxis,colour = status)) + 
        
        theme(plot.margin = margin(1,1,1,1, "cm"), plot.title = element_text(hjust = 0.5),axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
        labs(title = paste0(station," on ",ynight, " night")) + 
        xlab("Time of night") + 
        ylab ("") +
        
        scale_x_datetime(date_breaks = '2 hours',minor_breaks = '10 mins',
                         limits = c(min(as.POSIXct(meta_night$sunsettime,format = time_format,tz = timezone_R)) - lubridate::minutes(120),
                                    max(as.POSIXct(meta_night$new_end,format = time_format,tz = timezone_R)) +  lubridate::minutes(120)),
                         date_labels = "%H %M") +
        ylim(0,2) +
        
        theme(text = element_text(size = 10)) +
        
        # low let's apply some sessions to this plot 
        
        annotate('rect', xmin=as.POSIXct(sunset,format = time_format,tz = timezone_R),
                 xmax=as.POSIXct(sunrise,format = time_format,tz = timezone_R), ymin=1.2, ymax=Inf, alpha=.2, fill='darkorchid4') +
        
        
        annotate('rect', xmin=as.POSIXct(sunset,format = time_format,tz = timezone_R) - lubridate::minutes(90),
                 xmax=as.POSIXct(sunset,format = time_format,tz = timezone_R), ymin=1.2, ymax=Inf, alpha=.2, fill='darkorange3') +
        
        annotate('rect', xmin=as.POSIXct(sunrise,format = time_format,tz = timezone_R),
                 xmax=as.POSIXct(sunrise,format = time_format,tz = timezone_R) + lubridate::minutes(90), ymin=1.2, ymax=Inf, alpha=.2, fill='darkorange3') +
        
        geom_linerange(aes(y = ifelse(status == "recording",yaxis,yaxis-.2), xmin = as.POSIXct(new_start_time,format = time_format,tz = timezone_R),
                           xmax = as.POSIXct(new_end,format = time_format,tz = timezone_R)),linewidth = 5) +
        
        
        # geom_text(aes(x=as.POSIXct(new_start_time,tz = timezone_R)+new_length/2,label = ifelse(!new_length %in% c(180,540),new_length,""), 
                      # y = ifelse(status == "recording",yaxis+.1,yaxis-.1))) + 
        
        geom_linerange(aes(colour = "purple", y = yaxis-.8, xmin = as.POSIXct(full_rec_start_time,format = time_format,tz = timezone_R),
                          xmax = as.POSIXct(end_time,format = time_format,tz = timezone_R)),linewidth = 3) +
        
        theme(legend.position = 'none')
      
      
      
      
      print(plot)
      dev.off()
      
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ################################### 
    
    
    
  }
  
  
  # Combine combine sation into full sessions dataframe
  if (station == unique(meta_2$station)[1]){
    
    sessions = session_station
    
  } else (sessions = rbind(sessions,session_station))
  
  
  
  
  
  print(paste0("Station ", station, " complete"))
  
  
  
}




## Clipping recordings


write.csv(sessions, file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/MetaData/20231111_new_rec_Names_w_pauses.csv",row.names = F)










