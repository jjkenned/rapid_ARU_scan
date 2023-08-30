###########################################################
########### Visualize recording schedules  ################
###########################################################

## This script is for:
## - Determining/describing recording schedule
## - Exploring recording schedules within a set of recordings to find inconsistencies  
##  - in time of year (date)
##  - between Stations within a transect (or other deployment type)
##  - between actual and expected recording schedule 



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
prnt.source = c("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/recordings/2022_Nawhitti") # Base folder with recordings present 
project.dir = c("C:/Users/jeremiah.kennedy/Documents/PMRA/Code/rapid_ARU_scan") # directory for your R project 
jpg.dir = c("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/final_schedule_vis") # where the nightly recording schedule visualizations are to be kept 
GPS.Locs = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/recording_tracking/ARU_Locations.csv" # aru locations

# and time settings
time_format = "%Y-%m-%d %H:%M:%S%z"


# get functions
or.night = function(date_time,cutoff_hour,timezone){
  
  # get lubridate
  require(lubridate)
  date_time = as.POSIXct(date_time,format = time_format, tz = timezone)
  
  # part 1 makes datetime into yday
  or_date = as.numeric(yday(date_time))
  
  # and hour
  hour = hour(date_time)
  
  # part 2 compares these values
  if (hour<cutoff_hour){or_night = or_date
  } else {or_night = or_date + 1}
  
  return(or_night)
  
  
}

source(paste0(project.dir,"/Sourced_Functions/Get_wamd_GUANO_4.R"))

#### Skip to ~~ Time Manipulation ~~ (approx. line 134) on subsequent runs, once completed first time

#### ~~ Check directory consistency ~~  ######

# set intermediate directories
## IMPORTANT NOTE: This won't work if you don't at least have all recordings kept in the same depth of directories throughout the parent directory 
# Functions
# Gathers intermediate directory depth based on where the recordings are kept and how the prnt.source (parent directory) is defined
get.dir.depth = function(prnt.source){
  
  all.dirs = data.frame(Full = list.dirs(prnt.source,recursive = T,full.names = T)) # full names of directories present
  all.dirs$depth <- lengths(strsplit(all.dirs$Full, "/")) # get directory depth 
  all.dirs = all.dirs[all.dirs$depth == max(all.dirs$depth),]# keep only the deepest directories  
  all.dirs$path.from.parent = gsub(prnt.source,"",all.dirs$Full)
  all.dirs$depth.from.parent = str_count(all.dirs$path.from.parent,"/")
  
  
    
  
  return(all.dirs)
  
  
  
} 

dir.depth = get.dir.depth(prnt.source)

int.depth = unique(dir.depth$depth.from.parent) # check if inconsistent depths of directories 
#### rectify before continuing ##### 




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


meta_alt = read.csv(file.path("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/recording_tracking",
                              "Full_WAMD_GUANO_EXIF.csv"))
## sort out and keep only what you want 
# check unique values you may not want to keep, but need to know how many there are
unique(meta_alt$version)
unique(meta_alt$make)
unique(meta_alt$model)
unique(meta_alt$firmware)
unique(meta_alt$software)
unique(meta_alt$notes)
unique(meta_alt$samplerate)
unique(meta_alt$gain)
unique(meta_alt$meta_data_format)


# keep only fields that you want
keep=c('filename','serial_number', 'prefix', 'original_filename', 'timestamp', 'location', 'notes', 'samplerate', 'gain', 'length', 'temperature',"meta_data_format")

meta = meta_alt[keep]



# Reformat Timezone and timestamp values 

reform_tz = function (time,get){

  if(grepl("+",time,fixed = T)){
    
    zone_pre = getElement(unlist(strsplit(time,"+")),
                          length(unlist(strsplit(time,"+"))))
    sign = "+"
    
    
    } else {zone_pre = getElement(unlist(strsplit(time,"-")),
                        length(unlist(strsplit(time,"-"))))
    
    sign = "-"
    
    }
  

  
  if (grepl(":",zone_pre,fixed = T)){
    
    zone_true = paste0(sign,sprintf("%04d",as.numeric(gsub(":","",zone_pre))))
    
  } else (zone_true = paste0(sign,sprintf("%04d",as.numeric(zone_pre))))
    
    
  time_out = gsub(paste0(sign,zone_pre),zone_true,time)  
  
  
  
  if(get == "timestamp"){
    
    return(time_out)
    
  } else if(get == "timezone"){
    
    return(zone_true)
    
  } 
  
    
  
}



meta$form_tstamp = lapply(meta$timestamp,FUN = reform_tz, get = "timestamp")
meta$tzone = lapply(meta$timestamp,FUN = reform_tz, get = "timezone")

# to characters
meta$form_tstamp = as.character(meta$form_tstamp)
meta$tzone = as.character(meta$tzone)

## Create the R-recognized timezone value
# only deals with whole timezones, unsure how to format half hour timezones
timezone = "-0800"
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

meta$tzone_R = lapply(meta$tzone,FUN = R_timezone)
meta$tzone_R = as.character(meta$tzone_R)

# another quick function to pull the rest of the meta data
# Because Guano and WAMD encoded files contain different information, you may want or need this to supply missing info 
# for Guano files, include the following for filling in missing metadata
exif_keep = c("SourceFile","NumChannels","BitsPerSample","FileSize")

## IF you have a lot of files, this may take some time. Be aware of this 
# apply across dataset
meta_exif = plyr::mdply(all.recs$Full, 
                        
                        .fun = function(x) {exifr::read_exif(x, tags = exif_keep)},
                        
                       .progress=plyr::progress_text(style = 3))[exif_keep]


##  Combine metadata 
# now that all metadata is extracted from this dataset you can find everything you need in the future for these recordings 
# Make sure you keep what you could need down the road 
colnames(meta_exif)[colnames(meta_exif)=="SourceFile"]="filename"

meta_full = merge(meta_exif,meta, by = "filename")



#### ~~ save for later so you don't need to extract length anymore ~~ ### 

filename = file.path("S:","ProjectScratch","398-173.07","PMRA_WESOke","PMRA_SAR","2023_WLRS_Contract",
                     "processing",
                     "recording_tracking",
                     "Full_WAMD_GUANO_EXIF.csv")
if(!exists(dirname(filename))){dir.create(dirname(filename))}
# write.csv(meta_full,file = filename,row.names = F)



## Check unit naming and data redundancies ~ You can re-save file after this

meta = meta_full

redun_name_status = function(meta){
  
  cat("summarizing ")
  
  
  # Check number of different channels in metadata set
  
  subdat = "NumChannels"
  
  if(subdat %in% colnames(meta)){
    
    # Check channels
    
    suppressWarnings({
      
    meta_col = meta %>% group_by_(subdat) %>%
      
      summarise(units = n_distinct(prefix),
                recordings = n_distinct(basename(filename)))
    
    
    })
    
    cat("\n\n\n------\n\n",
        "Number of Active Mics",
        "\n\n")
    
    print(as.data.frame(meta_col),row.names = F)
    
    cat("\n------\n\n")
    
  } else {
    
    cat("\n\n\n------\n\n",
        "No mic/Channel count data",
        "\n\n------\n\n")
    
  }
  
  # Check Bits per sample data in metadata set 
  subdat = "BitsPerSample"
  
  if(subdat %in% colnames(meta)){
    
    
    suppressWarnings({
      
      meta_col = meta %>% group_by_(subdat) %>%
        
        summarise(units = n_distinct(prefix),
                  recordings = n_distinct(basename(filename)))
      
      
    })
    
    cat("\n\n\n------\n\n",
        "Bits per sample settings",
        "\n\n")
    
    print(as.data.frame(meta_col),row.names = F)
    
    cat("\n------\n\n")
    
  } else {
    
    cat("\n\n\n------\n\n",
        "No bits per sample data",
        "\n\n------\n\n")
    
  }
  
  # check sample rates
  subdat = "samplerate"
  
  if(subdat %in% colnames(meta)){
    
    
    suppressWarnings({
      
      meta_col = meta %>% group_by_(subdat) %>%
        
        summarise(units = n_distinct(prefix),
                  recordings = n_distinct(basename(filename)))
      
      
    })
    
    cat("\n\n\n------\n\n",
        "Sampleing rate (Hz) settings",
        "\n\n")
    
    print(as.data.frame(meta_col),row.names = F)
    
    cat("\n------\n\n")
    
  } else {
    
    cat("\n\n\n------\n\n",
        "No sampleing rate data",
        "\n\n------\n\n")
    
  }
  
  # check gain
  subdat = "gain"
  
  if(subdat %in% colnames(meta)){
    
    
    
    suppressWarnings({
      
      meta_col = meta %>% group_by_(subdat) %>%
        
        summarise(units = n_distinct(prefix),
                  recordings = n_distinct(basename(filename)))
      
      
    })
    
    cat("\n\n\n------\n\n",
        "Gain settings",
        "\n\n")
    
    print(as.data.frame(meta_col),row.names = F)
    
    cat("\n------\n\n")
    
  } else {
    
    cat("\n\n\n------\n\n",
        "No gain data",
        "\n\n------\n\n")
    
  }
  
  
  
  
  ## Prefix and serial number (unit ID) consistencies
  
  
  named_by_serial = meta %>% filter(serial_number == prefix) %>% distinct(serial_number,prefix)

  other_name = meta %>% filter(!serial_number == prefix) %>% distinct(serial_number,prefix)
  
  
  if(nrow(named_by_serial)>0){
    
    cat("\n\n\n------\n\n",
        "Named by Serial Number",
        "\n\n")
    
    print(as.data.frame(named_by_serial),row.names = F)
    
    cat("\n------\n\n")
    
    
    
  } else {cat("\n\n\n------\n\n",
              "No units named by Serial Number",
              "\n\n------\n\n")}
  
  
  
  if(nrow(other_name)>0){
    
    cat("\n\n\n------\n\n",
        "Named other than serial number",
        "\n\n")
    
    print(as.data.frame(other_name),row.names = F)
    
    cat("\n------\n\n")
    
    
    
  } else {cat("\n\n\n------\n\n",
              "All units apparently named by serial number",
              "\n\n------\n\n")}
    
  
  
  
  
  
  ## Filename changes from origional
  
  original_name = meta %>% filter(basename(filename) == original_filename) %>% distinct(basename(filename),original_filename)
  
  
  
  name_changed = meta %>% filter(!basename(filename) == original_filename) %>% distinct(basename(filename),original_filename)
  
  
  if(nrow(original_name)>0){
    
    cat("\n\n\n------\n\n",
        "Original Names",
        "\n\n")
    
    cat(nrow(original_name))
    
    cat("\n------\n\n")
    
    
    
  } else {cat("\n\n\n------\n\n",
              "All names apparenty changed",
              "\n\n------\n\n")}
  
  
  
  if(nrow(name_changed)>0){
    
    cat("\n\n\n------\n\n",
        "File names changed from original",
        "\n\n")
    if (nrow(name_changed)<50){
      
      print(as.data.frame(name_changed),row.names = F)
      
    } else(cat(nrow(name_changed)))
    
    
    cat("\n------\n\n")
    
    
    
  } else {cat("\n\n\n------\n\n",
              "All names apparently unchanged",
              "\n\n------\n\n")}
  
  
  
  
  
  
  
}


# review data and track in text file in tracking folder

Track_output = file.path("S:","ProjectScratch","398-173.07","PMRA_WESOke","PMRA_SAR","2023_WLRS_Contract",
                     "processing",
                     "recording_tracking",
                     "Review_all_recs_2ndPass.txt")

capture.output(redun_name_status(meta = meta),file = Track_output,append = T)
redun_name_status(meta = meta)


# Based on this we can now get rid of dead weight and and re-save metadata set
# haven't made changes so won't be doing this





######### Run Script from here down if first section one once already ######## 
######### REVIEW FROM HERE DOWN SINCE FIRST PART WAS RUN EARLIER ###### 

##### ~~ Time Manipulation ~~ #####



## Read data from last section (if running subesquent to first itteration)

filename = file.path("S:","ProjectScratch","398-173.07","PMRA_WESOke","PMRA_SAR","2023_WLRS_Contract",
                     "processing",
                     "recording_tracking",
                     "Full_WAMD_GUANO_EXIF.csv")

all.recs = read.csv(file = filename) # read the file

## You can check the time   
## When using units that aren't encoded with WAMD, next function gives you all the other metadata that this function doesn't (specifically for SM4 and earlier units)




## **Caution** ~ Time calculations crossing over the daylight savings shift will come out as NAs unless timezone set to "ETC/GMT+7" or "ETC/GMT-9" etc.
## **Important** ~ In "ETC/GMT+7", 7 refers to the timezone offset (+/-0700) but the (+/-) is reversed, here the timezone is PDT/MST (-0700) even though the sign is +


### Categorize recordings into ordinal night ~ Function from the beginning of script
# if recordings are across jan 1, this function will not work
# you will need to add 365 to the second year of recordings to compensate. THen it will be number of nights since Jan 1, the year before



# test df for manipulations
meta_2 = all.recs


# apply ordinal night function
meta_2$ynight = mapply(or.night,meta_2$form_tstamp, 12, meta_2$tzone_R)



# ······ Following Section could be deleted if works without now



# # Function for plotting start and end time of recording sessions for each night and station in a dataset
# # quick data prep. pre-loop/function
# meta_2$rec.name = basename(meta_2$Full) # get basename of recordings
# meta_2$station = sapply(str_split(meta_2$rec.name,"_"),'[',1) # get station name using split and extraction function
# 
# # empty columns to fill
# meta_2$time_new = NA
# meta_2$date_new = NA
# meta_2$date_time = NA
# meta_2$date_time = as_datetime(meta_2$date_time)
# 
# 
# # Create new date time DF to deal with NAs of crossing the daylight savings time boarder
# # i = 1 
# for (i in 1:nrow(meta_2)){
#   
#   # create date and time with appropriate flags
#   year = meta_2$year[i]
#   month = formatC(meta_2$month[i],width = 2,flag = 0)
#   day = formatC(meta_2$day[i], width = 2, flag = 0)
#   
#   hour =  formatC(meta_2$hour[i], width = 2, flag = 0)
#   minute =  formatC(meta_2$min[i], width = 2, flag = 0)
#   second =  formatC(meta_2$sec[i], width = 2, flag = 0)
#   
#   
#   meta_2$date_new[i] = paste0(year,"-",month,"-",day)
#   meta_2$time_new[i] = paste0(hour,"-",minute,"-",second)
#   meta_2$date_time[i] = ymd_hms(paste0(meta_2$date_new[i],"-",meta_2$time_new[i]))
#   
#   
# }


# new column for recording status 
meta_2$status = "recording"



#######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Get ARU locations for sunrise sunset times 

# check first the internal stuff
table(meta_2$location) #  if this is consistent across units you can likely skip some of the sections comparing the lat lon values 


# read in gps data 
gps = read.csv(GPS.Locs)


###### if you have UTM coordinates, you need to do this

# make function to get rid of the letter in the zone and return as numeric
substrRight <- function(x){
  x = substr(x, 1, nchar(x)-1)
  as.numeric(x)
}

gps$zone.num = substrRight(gps$Zone) # apply function


# function to manage the zones if locations cross zones


get.latlong = function(x,y,zone,datum){
  points = cbind(x,y) # GPS points from column
  colnames(points) = c("x","y") # rename for ease
  crs.1 = paste0("+proj=utm +zone=",zone," +datum=",datum,"  +units=m")# set CRS
  v = vect(points,crs = crs.1) # get vector of points
  crs.2 = paste0("+proj=longlat +datum=",datum) # set crs 
  lat.lon = project(v,crs.2) # project points
  geom(lat.lon)[,c("x","y")]
  
  
}


# new columns to keep results
gps$Latitude = NA
gps$Longitude = NA


# Loop function across rows (until I can figure out how to apply somnething like this)
for (i in 1:nrow(gps)){
  
  gps[i,c("Longitude","Latitude")] = get.latlong(gps$Lat[i],gps$Long[i],gps$zone.num[i],"WGS84")
  
  
}

# keep what you want for combining
gps_latlong = gps[c("New.Name","Latitude","Longitude")]
gps_latlong = gps_latlong %>% rename("station" = New.Name,  # rename this stuff
                                    "Ext_Lat" =  Latitude,
                                     "Ext_Lon" =  Longitude)





###### SKIP THIS IF EVERYTHING IS THE SAME GPS LOCATION ####### 

## Quick comparison between lat, lon from unit and from spreadsheet
gps_internal = meta_2 %>%
  group_by(prefix,location) %>%
  distinct(prefix,location) %>% separate(location,into = c('latitude','longitude'),sep = " ") %>%
  rename(station = prefix, Int_Lat = latitude,Int_Lon = longitude)

gps_comp = merge(gps_internal,gps_latlong,by = "station")
gps_comp$Int_Lat = as.numeric(gps_comp$Int_Lat)
gps_comp$Int_Lon = as.numeric(gps_comp$Int_Lon)
gps_comp$Ext_Lat = as.numeric(gps_comp$Ext_Lat)
gps_comp$Ext_Lon = as.numeric(gps_comp$Ext_Lon)


# check what difference is 
# i = 1
gps_comp$distance = NA
for(i in 1:nrow(gps_comp)){
  
   gps_comp$distance[i]= distm(c(gps_comp$Int_Lon[i],gps_comp$Int_Lat[i]),
        c(gps_comp$Ext_Lon[i],gps_comp$Ext_Lat[i]),fun = distHaversine)
  
  
  
}


## combine GPS and recording info ##
# check if anything is missing from either
unique(meta_2[!unique(meta_2$prefix) %in% unique(gps_latlong$station),]$station) # missing from gps reference set
unique(gps_latlong[!unique(gps_latlong$station) %in% unique(meta_2$prefix),]$station) # present only in gps reference set (not an issue)

# remove anything in gps reference set that isn't needed
gps_merge = gps_comp[gps_comp$station %in% meta_2$prefix,]
gps_merge = gps_merge[,!names(gps_merge) %in% "distance"] # get rid of distance before merging

# rename to merge 
colnames(meta_2)[colnames(meta_2)=="prefix"] = "station"
meta_3 = merge(meta_2,gps_merge,by = "station",all = T) # combine 

meta_2 = meta_3 %>% rename(True_Lat = Ext_Lat,True_Lon = Ext_Lon,
                           Int_Loc = location,start_time = form_tstamp)


lapply(meta_2,class)
  
# Now we can create recording and pause dataframe 
# Sessions section



# start by creating an end time for each recording 
# length = meta_2$length[1]
# start = meta_2$start_time[1]
# format = time_format
# timezone = meta_2$tzone_R[1]

get_end_time = function(start,format,timezone,length){
  
  end = as.POSIXct(start,format = format, tz = as.character(timezone))+ceiling(seconds(length))
  end = strftime(end, format = format, tz = as.character(timezone))
  
  return(end)
  
}

meta_2$end_time = mapply(get_end_time,meta_2$start_time,time_format,meta_2$tzone_R,meta_2$length)

meta_2$status = "recording"


## if you want to create jpegs with this, set the following to 'make'
make.jpgs = "no, I've got them already" # either use "make", if you want them made,or write literally anything else if you don´t want them made

# sort by 'station', 'ynight' to create new df with sessions and pauses in them

# test with 1 first
# station = unique(meta_2$station)[1]
station = unique(meta_2$station)[1]
meta_stn = meta_2[meta_2$station == station,]
ynight = unique(meta_stn$ynight)[1]
meta_night = meta_stn[meta_stn$ynight == ynight,]




for (station in unique(meta_2$station)){
  
  # subset by station
  meta_stn = meta_2[meta_2$station == station,]
  
  
  
  # now loop through nights 
  # test with 1 first
  # ynight = unique(meta_stn$ynight)[1]
  
  for (ynight in unique(meta_stn$ynight)){
    
    # subset by ynight
    meta_night = meta_stn[meta_stn$ynight == ynight,]
    
    
    ### Here down for check ####
    # check timezone values and use unique value for station night incase they change on deployment
    timezone = unique(meta_night$tzone_R)
    
    if(!length(timezone)==1){cat("\n\n ---- \n\nProblem with timezones in",ynight,"~",station)}
    
    
    # get two dates involved in this recording night
    date_range = unique(date(as.POSIXct(meta_night$start_time,format = time_format, tz = timezone)))

    ## return values and sources of error if we run into problems 
    
    # Issues with dates in ynight?
    if(!length(date_range)==2){cat("\n\n ---- \n\nProblem with number of nights in",ynight,"~",station)} # number of dates
    
    if(!as.numeric(difftime(date_range[1],date_range[length(date_range)])) %in% c(1,-1)){
      
      cat("\n\n ---- \n\nProblem with dates in",ynight,"~",station,"\n\nDates apparently not within 1 day \n\nDates\n")
      
      for (date in unique(date_range)){
        
        cat(as.character(as.Date(date,format = "%Y-%m-%d")),"\n")
        
      }
      
    } # number of days separating them
    
    # check if only the one latitude and longitude values for each station night
    # latitude
    if(!length(unique(meta_night$Int_Lat)) == 1){ 
      cat("\n\n ---- \n\nProblem with latitude in",ynight,"~",station,"\n latitudes")
      
      for(lat in unique(meta_night$latitude)){
        
        cat(lat,"\n")
        
      }
      
    }
    # longitude
    if(!length(unique(meta_stn$Int_Lon)) == 1){
      cat("\n\n ---- \n\nProblem with longitude in",ynight,"~",station,"\n longitudes")
      
      for(lon in unique(meta_stn$longitude)){
        
        cat(lon,"\n")
        
      }
    }
    

    
    
    # snag sunrise sunset times here (need to deal with timezones first)

    
    
    # finished error check 
    
    ## calculate times and reformat existing time variables
    # get sunrise sunset times 
    sunrise = suncalc::getSunlightTimes(date = date(max(date_range)),lat = meta_night$Int_Lat[1],lon = meta_night$Int_Lon[1], keep = c("sunrise"))
    sunset = suncalc::getSunlightTimes(date = date(min(date_range)),lat = meta_night$Int_Lat[1],lon = meta_night$Int_Lat[1], keep = c("sunset"))
    
    meta_night$sunrisetime = strftime(with_tz(sunrise$sunrise,tzone = timezone),format = time_format,tz = timezone)
    meta_night$sunsettime = strftime(with_tz(sunset$sunset,tzone = timezone),format = time_format, tz = timezone)
    
    
    true_sunrise = suncalc::getSunlightTimes(date = date(max(date_range)),lat = meta_night$True_Lat[1],lon = meta_night$True_Lon[1], keep = c("sunrise"))
    true_sunset = suncalc::getSunlightTimes(date = date(min(date_range)),lat = meta_night$True_Lat[1],lon = meta_night$True_Lon[1], keep = c("sunset"))
    
    meta_night$true_sunrise = strftime(with_tz(true_sunrise$sunrise,tzone = timezone),format = time_format,tz = timezone)
    meta_night$true_sunset = strftime(with_tz(true_sunset$sunset,tzone = timezone),format = time_format, tz = timezone)
    
    
    
    
    
    
    # loop through the recordings on the night
    # i=1
    for (i in 1:nrow(meta_night)){
      
      # new two df with data to keep for each session
      meta_rec = meta_night[i,c("filename","station","ynight","start_time","end_time","length","status","sunrisetime","sunsettime","true_sunrise","true_sunset","Int_Lon","Int_Lat","True_Lat","True_Lon","tzone","tzone_R")]
      
      
      
      if (!i==nrow(meta_night)){
        
        # now use the subsequent recording entry to get end time and length for pause
        
        meta_pause = meta_rec # create record of pause
        meta_pause$filename = NA
        meta_pause$status = "pause" # change status to pause
        meta_pause$start_time = meta_rec$end_time # change start time to end time of recording
        meta_pause$end_time = meta_night[i+1,]$start_time # get start and end
        meta_pause$length = as.numeric(seconds(as.POSIXct(meta_pause$end_time,format = time_format,tz = timezone)) - seconds(as.POSIXct(meta_pause$start_time,format = time_format, tz = timezone))) # length of pause
        
        # Now combine these two into a rec_pause dataframe
        session_rec = rbind(meta_rec,meta_pause)
        
      } else (session_rec = meta_rec)
      
      
      # combine rec pauses into nightly dataframe
      if (i == 1){
        
        session_night = session_rec
        
      } else(session_night = rbind(session_night,session_rec)) 
      
      
      
      
    } 
    
    
    # create jpg name for saving and also using during jpg creation
    dir.stn = paste0(jpg.dir,"/",station) # directory build
    jpg.name = paste0(dir.stn,"/Sessions_",station,"_",ynight,".jpg") # create jpg full name
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
    toplot$length = as.numeric(toplot$length)
   
    
    # if you want to run this section set 'make.jpgs' to 'make'
    
    if (make.jpgs=="make"){
    if(!exists(dirname(jpg.name))){
      
      dir.create(dirname(jpg.name),recursive = T)
      
    }
   
      
       jpeg(filename = jpg.name,width = 1400, height = 800,res = 110)
    
    # start plot
    
    
    
    plot = ggplot(toplot,aes(x = as.POSIXct(start_time,format = time_format,tz = timezone),y = yaxis,colour = status)) + 
      
      theme(plot.margin = margin(1,1,1,1, "cm"), plot.title = element_text(hjust = 0.5),axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      labs(title = paste0(station," on ",ynight, " night")) + 
      xlab("Time of night") + 
      ylab ("") +
      
      scale_x_datetime(date_breaks = '2 hours',minor_breaks = '30 mins',
                       limits = c(min(as.POSIXct(meta_night$start_time,format = time_format,tz = timezone)) - minutes(60),
                                  max(as.POSIXct(meta_night$start_time,format = time_format,tz = timezone)) + minutes(60)),
                       date_labels = "%H %M") +
      ylim(0,2) +
      
      theme(text = element_text(size = 25)) +
      
      # low let's apply some sessions to this plot 
      
      annotate('rect', xmin=as.POSIXct(sunset$sunset,format = time_format,tz = timezone),
               xmax=as.POSIXct(sunrise$sunrise,format = time_format,tz = timezone), ymin=1.2, ymax=Inf, alpha=.2, fill='darkorchid4') +
      
      
      annotate('rect', xmin=min(as.POSIXct(meta_night$start_time,format = time_format,tz = timezone)) - minutes(60),
               xmax=as.POSIXct(sunset$sunset,format = time_format,tz = timezone), ymin=1.2, ymax=Inf, alpha=.2, fill='darkorange3') +
      
      annotate('rect', xmin=as.POSIXct(sunrise$sunrise,format = time_format,tz = timezone),
               xmax=max(as.POSIXct(meta_night$start_time,format = time_format,tz = timezone)) + minutes(60), ymin=1.2, ymax=Inf, alpha=.2, fill='darkorange3') +
      
      geom_linerange(aes(y = yaxis, xmin = as.POSIXct(start_time,format = time_format,tz = timezone),
                         xmax = as.POSIXct(end_time,format = time_format,tz = timezone)),linewidth = 3) +
      
      geom_text(aes(x=as.POSIXct(start_time,format = time_format,tz = timezone)+length/2,label = ifelse(length>60,
                                                           round(length/60,0.1),
                                                           paste0(floor(length)," Sec")), 
                                                          vjust = ifelse(status == "recording",-1.5,+2))) + 
      geom_linerange(aes(x=as.POSIXct(start_time,format = time_format,tz = timezone), ymin = 0, ymax = 0.6, linetype = status), linewidth = 1.2) +
      
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


#### save file for next script


write.csv(sessions,file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/recording_tracking/2022_Nawhitti_recording_data_recs_pauses_final.csv",row.names = F)











### Copy over to new folder 
# 
# 
# files.move = data.frame(Full = list.files(path = jpg.dir,full.names = T,recursive = T,pattern = ".jpg"))
# 
# files.move = files.move[grepl("/Sessions_",files.move$Full,fixed = T),]
# 
# files.move$new.name = gsub(pattern = "nightly_recording_schedule",replacement = "new_schedule_vis",files.move$Full)
# 
# 
# for (i in 1:nrow(files.move)){
#   
#   if(!exists(dirname(files.move$new.name[i]))){
#     
#     dir.create(dirname(files.move$new.name[i]),recursive = T)
#     
#   }
#   
#   file.copy(from = files.move$Full[i],to = files.move$new.name[i],)
#   
# }
# 
# for(i in 1:nrow(files.move)){
#   
#   unlink(files.move$Full[i])
#   
# }
# 
# 
# 




##### Figure out what we have for lengths ###### 

# lengths

length.check = sessions %>% group_by(status,length) %>% summarise(count = n())




# extra stuff that may not be needed 



# First loop through stations

# test with 1 first
# station = unique(meta_2$station)[1]
# 
# for (station in unique(meta_2$station)){
#   
#   # subset by station
#   meta_stn = meta_2[meta_2$station == station,]
#   
#   
#   
#   # now loop through nights 
#   # test with 1 first
#   # ynight = unique(meta_stn$ynight)[1]
#   
#   for (ynight in unique(meta_stn$ynight)){
#     
#     # subset by ynight
#     meta_night = meta_stn[meta_stn$ynight == ynight,]
#     
#     # create directory for saving files
#     dir.stn = paste0(jpg.dir,"/",station)
#     if (!dir.exists(dir.stn)){dir.create(dir.stn,recursive = T)}
#     
#     # start jpeg creation by naming with station and night
#     jpeg(filename = paste0(dir.stn,"/",station,"_",ynight,".jpg"),width = 1400, height = 500)
#     
#     # start plot
#     plot = ggplot(meta_night,aes(x = date_time, y = status)) + 
#       
#       theme(plot.margin = margin(1,1,1,1, "cm"), plot.title = element_text(hjust = 0.5),
#             axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
#       labs(title = paste0(station," on ",ynight, " night")) + 
#       xlab("time") + 
#       ylab("Recorder Status") +
#       
#       scale_x_datetime(date_breaks = '2 hours',minor_breaks = '30 mins',
#                        limits = c(min(meta_night$date_time) - minutes(60),max(meta_night$date_time) + minutes(60)),
#                        date_labels = "%H %M") +
#       theme(text = element_text(size = 25)) +
#       
#       # low let's apply some sessions to this plot 
#       
#       geom_linerange(aes(y = status, xmin = date_time,
#                          xmax = date_time+seconds(length)),linewidth = 2)
#     
#     print(plot)
#     
#     
#     
#     
#     
#     
#     dev.off()
#     
#     # track 
#     
#     
#     print(paste0("station ",station, " night ",ynight, " Completed"))
#     
#     
#     
#   }
#   
#   print(paste0("Station ", station, " complete"))
#   
#   
#   
# }
# 





























