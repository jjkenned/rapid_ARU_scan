#####################################################################################
########### Manipulate audio files into shorter processing schedule  ################
#####################################################################################

#### Pre-requisites for this script
## - 3_Review_recording_schedules.R 
## - 4_Prep_Clipping_Longer_Recordings_Sunlight_Times.R


## This script is for:
## - Setting new schedule to clip with from base rec schedule
## - Clip recordings to new schedule from base schedule and save in new location for use




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
# detach("package:terra",unload = T)

####  working spaces
# Set locations for source folder and destination folder 
# you may also want to have a temporary folder 
SourceFolder = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/recordings/2022_Nawhitti" #where your recording files are kept

OutputFolder =  "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/indices/shorter_recordings" # where saving images

project.dir = c("C:/Users/jeremiah.kennedy/Documents/PMRA/Code/rapid_ARU_scan") # directory for your R project 

# meta data is kept in three main files
meta.extract.path = file.path("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/recording_tracking",
                         "Full_WAMD_GUANO_EXIF.csv") # metadata extracted from recordings 

sun_relative_meta.path = file.path("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/recording_tracking",
                  "Start_End_Times_Relative_Suntimes_Final.csv") # sunrise and sunset relative times and metadata

nightly_summary.path = file.path("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/recording_tracking",
                  "NightSummary_Relative_Sun_Final.csv") # sunrise and sunset times nightly summary 



#########~~~~~~~ Section 1 ~~~~~~~~##########
### design recording schedule

## Read in data
meta.extract = read.csv(meta.extract.path)
sun_relative_meta = read.csv(sun_relative_meta.path)
nightly_summary = read.csv(nightly_summary.path)



## Function for creating a new (similar schedule) based on a consistent recording schedule 
# This function assumes the following naming convention within metadata file

# This function requires the following values 
meta = sun_relative_meta[sun_relative_meta$status=="recording",] # metadata including: 

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


sub_schedule = function(meta,start_time,end_time,rec_length,pause_length,dial_focus,min_pause,...){
  
  # Make quick function to format dates and times quicker
  ct7 = function(time){
    
    strptime(time, format = "%Y-%m-%dT%H:%M:%S%z", tz = "Etc/GMT+7")
    
    
  } # Converts character to time using iso8601 format in -0700 timezone
  
  tc7 = function(time){
    
    strftime(time, format = "%Y-%m-%dT%H:%M:%S%z", tz = "Etc/GMT+7")
    
  } # Converts time to character using iso8601 format in -0700 timezone
  
  in_out_time = function(func,time){
    
    time = strptime(time, format = "%Y-%m-%dT%H:%M:%S%z", tz = "Etc/GMT+7")
    
    time_value = func(time)
    
    out = strftime(time_value, format = "%Y-%m-%dT%H:%M:%S%z", tz = "Etc/GMT+7")
    
    return(out)
    
  } # Converts string to time, applies function, then converts resulting time back to character, using iso8601 format in -0700 timezone
  
  # default dial focus is daytime
  if(missing(dial_focus)){dial_focus = "diurnal"}
  if(missing(min_pause)){min_pause = pause_length}
  if(missing(separator)){separator = 15}
  
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
  
  
  
  p_f_combos = data_frame(expand_grid(p_choices = seq(p_min,p_max,60), f_choices = seq(f_min,f_max,60)))
  
  # fill in the number of rec sessions (n)

  p_f_combos$n_recs = mapply(get_n, p_f_combos$f_choices,rec_length,p_f_combos$p_choices)
  
  # now we have a reference list to check how many subrecs(s) we want from each large rec (f)
  # based on the pause length variation and f length variation 
  # visualize using minutes instead 
  p_f_combos$fminute = p_f_combos$f_choices/60
  p_f_combos$pminute = p_f_combos$p_choices/60
  
  # check and apply p.min and n.max values for each time frame applied
  params = list(...) # list parameters
  # params = tester
  params$name = gsub(".","_",params$name)
  
  keeping = do.call(rbind, params) # combine list
  
  params = data.frame(value = keeping) # dataframe and naming value column
   
  params$name <- lapply(row.names(params), '[[', 1)  # move row name into it's own parameter ID column
  
  # create upper and lower seconds bounds for filtering
  params$upper = as.numeric(sapply(
    strsplit(as.character(params$name),"_")
    , getElement, 3))*60
  
  
  params$lower = params$upper-((separator)*60)+1
  
  params$by.type = sapply(
    strsplit(as.character(params$name),"_")
    , getElement, 1)
  
  
  
  # Apply optional filters within loop
  
  
  # full = 2340
  
  
  for (full in p_f_combos$f_choices){
    
    combo.by.f = p_f_combos[p_f_combos$f_choices==full,]
    combo.by.f$p.keep = NA
    combo.by.f$n.keep = NA
    
    
    # check which bins the recordings fit in 
    
    param_check = params[params$upper>=full & params$lower<=full,]
    
    
    if(nrow(param_check)>2){cat("too many params in",full)}
    
    params_max_n = param_check[param_check$by.type=="n",]
    params_min_p = param_check[param_check$by.type=="p",]
    
    #i=1
    for (i in 1:nrow(combo.by.f)){
      #j=1
      
      
      
      if(combo.by.f$f_choices[i]>=params_max_n$lower & 
         combo.by.f$f_choices[i]<=params_max_n$upper &
         combo.by.f$n_recs[i]>params_max_n$value){
        
        combo.by.f$n.keep[i] = "remove"
        
      } else(combo.by.f$n.keep[i] = "keep")
      
        
        
      # j=3
     
        
        
        
        if(combo.by.f$f_choices[i]>=params_min_p$lower & 
           combo.by.f$f_choices[i]<=params_min_p$upper &
           combo.by.f$p_choices[i]<(params_min_p$value*60)){
          
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
  
  
  
  # ggplot(data = combo_keep,aes(x = fminute,y=n_recs,col = pminute))+
  #   geom_point()
  # # 
  
  
  
  # Now that we have a combination of values for setting standard time frames for clipping full audio
  # into new, smaller files, compare these to the main values
  
  
  # Dataframe meta
  i=2
  
  station=unique(meta$station)[1]
  night = unique(meta_stn$ynight)[1]
  
  for (station in unique(meta$station)){
    
    meta_stn = meta[meta$station==station,]
    
    for (night in unique(meta_stn$ynight)){
      
      
      meta_date = meta_stn[meta_stn$ynight==night,]
      meta_date = meta_date[order(ct7(meta_date$start_time),decreasing = F),] # sort by start time just incase
      
      
      # let's start with a tracking file and an output file
      track_out = list()
      new_recs = list()
      
      for (i in 1:nrow(meta_date)){
        
       
        
        # subset data
        old_rec = meta_date[i,]
        
        #### Section 1 ~~ Trim to pre-set sunrise/sunset based start and end times ####
        
        start_since_sunset = old_rec$start_time_since_sunset
        start_to_sunrise = old_rec$start_time_to_sunrise
        end_since_sunset = old_rec$end_time_since_sunset
        end_to_sunrise = old_rec$end_time_to_sunrise
        
        
        # track what you do 
        old_rec$kept = "keep"
        old_rec$start_time_offset = NA
        old_rec$end_time_offset = NA
        old_rec$prior_rec_offset = NA
        old_rec$full_rec_start_time = old_rec$start_time
        
        # Dont include if they both start and end before start time
        if(start_since_sunset<start_time & end_since_sunset<start_time){
          
          cat("\n\n ~~~~~~~~ \n\n","Recording occurs before start of recording period ~ Skipping","\n\n",old_rec$station,"\n From ",old_rec$start_time,"\n To ",old_rec$end_time,"\n",old_rec$filename)
          
          old_rec$kept = "removed"
          
          track_out[[i]]=old_rec 
          
          next
          
        }
        
        # Dont include if they both start and end after end time
        if(start_to_sunrise<end_time & end_to_sunrise<end_time){
          
          cat("\n\n ~~~~~~~~ \n\n","Recording occurs after end of recording period ~ Skipping","\n\n",old_rec$station,"\n From ",old_rec$start_time,"\n To ",old_rec$end_time,"\n",old_rec$filename)
          
          old_rec$kept = "removed"
          
          track_out[[i]]=old_rec
          
          next
          
        }
        
        # if recording starts before start time and ends after start time
        # trim the start of recording 
        if(start_since_sunset<start_time & end_since_sunset>start_time){
          
          offset = start_time - start_since_sunset
          old_rec$start_time = in_out_time(func = function(x) offset+x,time = old_rec$start_time)
          old_rec$length = old_rec$length-offset
          old_rec$start_time_offset = offset
          
          
          
          
          
        } 
        
        # if recording starts before end time and ends after end time
        # trim the end of recording 
        
        if (start_to_sunrise>end_time & end_to_sunrise<end_time){
          
          offset = end_time - end_to_sunrise
          old_rec$end_time = in_out_time(func = function(x) x-offset,time = old_rec$end_time)
          old_rec$length = old_rec$length-offset
          old_rec$end_time_offset = offset
          
          
          
          
        }
        
        
        #### Section 2 ~~ offset the start of any recordings that may be too close to the last ####
        # take the end of the last one
        
        
        if(i>1){
          
          last_rec = rec_time+rec_length
          
          if(ct7(old_rec$start_time)<last_rec+min_pause & !track_out[[i-1]]$kept == "removed"){
            
            offset = as.numeric(difftime((last_rec+min_pause),ct7(old_rec$start_time),units = "secs"))
            
            old_rec$start_time = in_out_time(func = function(x) offset+x,time = old_rec$start_time)
            
            old_rec$length = old_rec$length-offset
           
            old_rec$prior_rec_offset = offset
             
          }
          
          
        }
        
        #### Section 3 ~~ Remove recordings that are now too short ###### 
        if(old_rec$length<rec_length){
          
          cat("\n\n ~~~~~~~~ \n\n","Recording too short ~ Skipping","\n\n",old_rec$station,"\n From ",old_rec$start_time,"\n To ",old_rec$end_time,"\n",old_rec$filename)
          
          old_rec$kept = "removed"
          
          track_out[[i]]=old_rec 
          
          next
          
        }
        
        
        
        #### Section 4 ~~ Check number of sessions to be fit into longer recordings ####
        
        # FInd recording length value in list of chunking choices
        sub_pattern = combo_keep[combo_keep$f_choices == floor(old_rec$length/60)*60,]
        
        if(!nrow(sub_pattern)==1){cat("too many choices for",old_rec$filename)}
        
        # start while loop through number of recs 
        
        n=0
        while(n<=sub_pattern$n_recs){
          
          # make a new name and corresponding offset and ending offset from beginning of recording
          rec_time = ct7(old_rec$start_time)
          
          offset = n*sub_pattern$p_choices
          
          newstart = rec_time+offset
          
          newtime = strftime(newstart, format = "%Y%m%d_%H%M%S%z", tz = "Etc/GMT+7")
          
          
          
          newname = paste0(old_rec$station,"_",newtime,".wav")
          
          sub_keeper = old_rec
          
          sub_keeper$new_name = newname
          
          sub_keeper$clip_offset = offset
        
          
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
  
  
  
  return(track_stn,new_name_stn)
  
  
  
}  
  
  

# a second function for organizing the output
meta_2_convert = new_name_stn[new_name_stn$kept=="keep",]
meta_2_convert = meta_2_convert[c("filename","new_name","station","ynight","start_time","end_time","length","start_time_offset","end_time_offset","prior_rec_offset","clip_offset")]

meta_2_convert$true_start_offset = rowSums(meta_2_convert[,c("start_time_offset","prior_rec_offset","clip_offset")],na.rm = T)




###


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  for (i in 1:nrow(meta)){
    
    old_rec = meta[i,]
    
    # FInd recording length value in list of chunking choices
    sub_pattern = combo_keep[combo_keep$f_choices == floor(old_rec$length/60)*60,]
    
    if(!nrow(sub_pattern)==1){cat("too many choices for",old_rec)}
    
    # start while loop through number of recs 
    n=0
    while(n<=sub_pattern$n_recs){
      
      # make a new name and corresponding offset and ending offset from beginning of recording
      rec_time = ct7(old_rec$start_time)
      
      offset = n*sub_pattern$p_choices
      
      newstart = rec_time+offset
      
      newtime = strftime(newstart, format = "%Y%m%d_%H%M%S%z", tz = "Etc/GMT+7")
      
      
      
      newname = paste0(old_rec$station,"_",newtime,".wav")
      
      sub_keeper = data.frame(old.full = old_rec$filename,new.name = newname,offset = offset)
      
      
      
      
      #### filter by sunrise and sunset
      # and figure out how to save these output values from a while loop 
      
      
      
      
      
      
    }
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  # Loop through station and Date
  # Rename for ease
  colnames(meta)[colnames(meta)==station_org] = "station"
  colnames(meta)[colnames(meta)==date_org] = "ydate"
  
  
  ####### Trial ~ Remove ######
  
  station = unique(meta$station)[1]
  meta_stn = meta[meta$station==station,]
  date = unique(meta_stn$ydate)[1]
  meta_date = meta_stn[meta_stn$ydate==date,]
  
  ######################
  
  # loop test IDs
  # station = unique(meta$station)[1]
  
  for (station in meta$station){
    
    meta_stn = meta[meta$station==station,]
    
    # date = unique(meta_stn$ydate)[1]
    for (date in meta_stn$ydate){
      
      meta_date = meta_stn[meta_stn$ydate==date,]
      
      # Order by time (just in case)
      meta_date = meta_date[order(ct7(meta_date$start_time),decreasing = F),]
      
      # get start and end
      night_start = in_out_time(min,meta_date$start_time)
      night_end = in_out_time(max,meta_date$end_time)
      
      
      # Trial loop
      i=1
      for (i in 1:nrow(meta_date)){
        # x = (t-r)/(r+p)
        tlength = floor(meta_date$length[i]/60)*60
        Nrecs = as.numeric(rec.counts[rec.counts$long.rec %in% tlength,]$Nr) # number of subrecordings to extract
        
        # Use equation to calculate the length of pauses between 
        
        
        
      }
      
      # what do I need from this?
      # - file name to read from
      # - time offset from start of file
      # - Length of file to be written
      # - Create new file name from:
      #   - Station
      #   - new time (including timezone)
      
      
      
      # Then we can use this to loop through and call sox to clip and create in another function  
      
      
      
    }
    
    
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
}







































