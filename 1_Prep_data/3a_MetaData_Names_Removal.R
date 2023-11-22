###########################################################
########### Organize and Clean Recordings  ################
###########################################################


## This is basically the first script dealing with recordings in their final storage/processing locations

## This script is for:
# ~ Retrieving Meta data from wav files (GUANO and WAMD)
# ~ Finding and removing/storing corrupted/empty files
# ~ Creating new 'basic name' for recording to keep names the same between recorder types and name formats
# - Storing all this data in a place to be retrieved in later scripts and processing

# This script should only need to be run once on your entire data set if applied properly
# it can, however, be run on individual directories quite easily to break appart the processing


##### PRE-SCRIPT WARNINGS ###### 
# ~ Make sure to check what kind of recordings you're collecting 
#   - Define this in recording types early in the script
#   - 

## SQLite in R
# download sqlite for appropriate OS (https://sqlite.org/download.html) 
# For windows, find "A bundle of command-line tools for managing SQLite database files"
# 



## Resets ## 
# Reset your script when needed
dev.off()
rm(list=ls())

# set Timezone
Sys.setenv(TZ = "Etc/GMT+7") # "https://www.ibm.com/docs/en/cloudpakw3700/2.3.0.0?topic=SS6PD2_2.3.0/doc/psapsys_restapi/time_zone_list.htm"
Sys.timezone() # check
options(stringsAsFactors = FALSE)
time_format = "%Y-%m-%d %H:%M:%S%z"

## Packages ##
# ~ Install if you don't already have them installed
# ~ install.packages(c("RSQLite","exifr","suncalc","tidyverse","lubridate","RSQLite","DBI","chron","stringr"))


# library what is needed
library(stringr)
library(av)
library(seewave)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(exifr)
library(suncalc)
#library(terra)
#library(geosphere)
library(RSQLite)
library(DBI)
library(chron)


## set directories ##

# Recording and input directories

# "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract"
# "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti"

prnt.source = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Recordings/BIRD/2022/MKVI" # Base folder with recordings present 

# MetaData and output directories
base.meta.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/MetaData" 
GPS.Locs = file.path(base.meta.dir,"External","20231011_Location_Info.csv") # aru locations
jpg.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Recordings/BIRD/2022/MKVI" # images for rec schedule review

# set location for metadata Database
db.path = "S:/Projects/107182-01/06 Data/ARU processing/meta_data"


## Functions ##

# ~ Sourced from project folder
# if not using 'RStudio Project ~ rapid_ARU_scan' modify path to location of the following function on your computer
source("Sourced_Functions/Get_wamd_GUANO_4.R")
source("sourced_Functions/Or_Night_20000101_Function.R")
source("sourced_Functions/Or_Seconds_20000101_Function.R")

## File Types ##
# Define File types for audio recordings to be encountered
file.types = c(".wav",".wac",".mp3",".flac",".w4v")


############################
##### ~~~~ PART 1 ~~~~ #####
#### Extract metadata ######
############################


# Better alternative to songmeter function in seewave
# This takes much longer, but only really needs to be done once

# List audio files
all.recs = data.frame(Full = list.files(prnt.source,recursive = T,full.names = T, pattern = paste(as.character(file.types),collapse = "|"))) # full names of directories present

# multiplefolders

prnt.source = c("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Recordings/BIRD/2022/MKVI/MKVI-04",
                "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Recordings/BIRD/2022/MKVI/MKVI-06")

for (i in 1:length(prnt.source)){
  
  stn.recs = data.frame(Full = list.files(prnt.source[i],recursive = T,full.names = T, pattern = ".wav")) # full names of directories present

  if(i==1){all.recs = stn.recs}else(all.recs = rbind(all.recs,stn.recs))
  
}

all.recs = data.frame(Full = list.files(prnt.source,recursive = T,full.names = T, pattern = ".wav")) # full names of directories present

all.recs.run = all.recs %>% filter(row(all.recs)>1)


some.recs = all.recs[basename(dirname(all.recs$Full)) %in% c("MKVI-04","MKVI-06"),]

# sample for each 
all.recs$station = basename(dirname(all.recs$Full))

for (station in unique(all.recs$station)){
  
  if (station == unique(all.recs$station)[1]){
    
    
    sub.recs = head(all.recs[all.recs$station == station,],n=5)
      
    
  } else {
    
    sub.recs = rbind(sub.recs,head(all.recs[all.recs$station == station,],n=5))
    
  }
  
  print(station)
  
  
}



### get meta data for recordings 
# Step 1 - extract WAMD and GUANO encoded meta data
meta_alt = plyr::mdply(all.recs.run$Full[3], # 
                       Get_wamd_guan,
                       .progress=plyr::progress_text(style = 3))



### STOP #### 

# Files that contain warnings in the 'comments' field are typically corrupted.
# Run through them with this next section and manually check before moving out of the 
# recording storage location and into somewhere where they can't interfere with scripts


# Based on above information, check potentially corrupted files and set aside
remove = data.frame(old.name = meta_alt$filename[!is.na(meta_alt$notes)])
timeless = data.frame(old.name = meta_alt$filename[is.na(meta_alt$timestamp)])


# Copy and delete these files from their parent directory 
remove$new.name = gsub("Recordings","problem_recordings",remove$old.name)

for (i in 1:nrow(remove)){
  
  if(!exists(dirname(remove$new.name[i]))){dir.create(dirname(remove$new.name[i]),recursive = T)}
  file.copy(from = remove$old.name[i], to = remove$new.name[i],recursive = T)
  
  
  
}



# good to go? Remove the old ones

for (i in 1:nrow(remove)){
  
  unlink(remove$old.name[i], recursive = T)
  
  
  
}


# exclude the recordings you dont want to use
# this will remove all these recordings from the lists so you should move them before you go any further 
# validate that these are indeed corrupted (or otherwise should not be included)
all.recs.use = data.frame(Full = all.recs[!all.recs$Full %in% remove$old.name,])

# now extract the exif data


# another quick function to pull the rest of the meta data
# Because Guano and WAMD encoded files contain different information, you may want or need this to supply missing info 
# for Guano files, include the following for filling in missing metadata
exif_keep = c("SourceFile","NumChannels","BitsPerSample","FileSize","SampleRate")


## IF you have a lot of files, this may take some time. Be aware of this 
# apply across dataset
meta_exif = plyr::mdply(all.recs.use$Full, 
                        
                        .fun = function(x) {exifr::read_exif(x, tags = exif_keep)},
                        
                        .progress=plyr::progress_text(style = 3))[exif_keep]


##  Combine metadata 
# now that all metadata is extracted from this dataset you can find everything you need in the future for these recordings 
# Make sure you keep what you could need down the road 
colnames(meta_exif)[colnames(meta_exif)=="SourceFile"]="filename"

meta_full = merge(meta_exif,meta_alt, by = "filename")

### This next section gives the user an idea of what fields are complete and which ones are missing values
## you can also use this section to trouble shoot why you arent getting some of this data (model, make etc)

# what columns have NAs and how many do they have
get.NAs = function(df){
  
  output = data.frame((colnames(df)))
  
  colnames(output) = c("field")
  
  for (i in 1:nrow(output)){
    
    count = length(df[!complete.cases(df[,i]),i])
    
    output$na_count[i] = count
    
  }
  
  output = output[order(output$na_count),]
  
  return(output)
  
  
}

na.counts = get.NAs(meta_full)



## start by seeing what patterns are occuring regarding NA counts (consistencies between models and units etc)
# Check this by looking at the na.counts manually (ordered by number of occurances)
# and see what types of units pr columns you want to filter by, based on the similar counts of NAs in recordings



# As long as neither 'models' nor 'prefix' contains NAs run the next collection of script
# if there are NAs in these you will want to likely remove or relocate these recordings
models = data.frame(table(meta_full$model))

stn.model.name = meta_full %>% group_by(prefix,model) %>% summarise(rec.count = n(),
                                                                    name.length = unique(nchar(prefix)))

# check individuals if you want
sm3BAT = meta_full[meta_full$model=="SM3BAT",]


## What different naming convensions are you dealing with
naming.conv = meta_full %>% group_by(prefix,model) %>% summarise(rec.count = n())

## Combine values found in multiple columns
# first, check that the sample rates match
# If the only mis-matches between these columns comes down to missing values (NA) use the complete, exif, samplerate output
unique(meta_full[c("samplerate","SampleRate")])

# if you have multiple mics you may need to separate the mics and sensitivity columns
meta_save = meta_full %>% separate(sensitivity,c("left.gain","right.gain"),sep = ",")

# If the only mis-matches between these columns comes down to missing values (NAs) you are good to continue
# issues can arrise if there are overlapping values or missing values
unique(meta_save[c("left.gain","gain")])



## Identify the columns you want to keep
# 

keep=c('filename','serial_number', 'prefix', 'original_filename', 'timestamp', 'location', 'GPS2','notes', 'NumChannels',
       'SampleRate', 'BitsPerSample', 'FileSize', 'gain','left.gain','right.gain', 'length', 'mics', 'temperature','firmware','model',"meta_data_format")

meta = meta_save[keep]




## 
dir.create(dirname("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Results/Tracking/BIRD/2023/MKVI/2023_BIRD_MKVI_Meta_Data_Recordings.csv"),recursive = T)
# write.csv(meta,file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Results/Tracking/BIRD/2023/MKSC/2023_BIRD_MKSC_Meta_Data_Recordings.csv")




### Saved base data prior to more work to save all of the time to create this

time.lengths = table(nchar(meta$timestamp))

## differences between timestamps between ARU models (even between different wildlife acoustics models)

# SM3 and SM4 timestamps have an extra 0 before the timezone component
# "2023-02-01 16:37:00-07:00" - SM3, SM4 (25 characters)
# "2023-03-26 18:02:00-7:00" - SMmini and likely others (24 characters)

# check what lengths there are (should be 24 or 25 characters long)
time.lengths = meta_full %>% group_by(model) %>% summarise(char.lengths = unique(nchar(timestamp)))


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
meta$form_tstamp = as.character(meta$form_tstamp)

meta$tzone = lapply(meta$timestamp,FUN = reform_tz, get = "timezone")
meta$tzone = as.character(meta$tzone)

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

meta$tzone_R = lapply(meta$tzone,FUN = R_timezone)
meta$tzone_R = as.character(meta$tzone_R)

meta$real_prefix = substr(basename(meta$filename),1,11)
meta$real_transect = substr(meta$real_prefix,1,7)
## Now remove and combine fields that are duplicated

# start with GPS fields
# loop presets
meta$int_lat = NA
meta$int_lon = NA
options(digits = 9)
gps.missing.count = 0

# loop-filled dfs



i=10789
for (i in 1:nrow(meta)){
  
  if (!is.na(meta$location[i])){

    meta$int_lat[i] = as.numeric(unlist(strsplit(meta$location[i],split = "-"))[1])
    meta$int_lon[i] = as.numeric(unlist(strsplit(meta$location[i],split = "-"))[2])
    
    
  } else if (!is.na(meta$GPS2[i])){

    meta$int_lat[i] = as.numeric(unlist(strsplit(meta$GPS2[i],split = ","))[2])
    meta$int_lon[i] = as.numeric(unlist(strsplit(meta$GPS2[i],split = ","))[4])
    
  } else {
    
    gps.missing = meta[i,c("prefix","serial_number","model")]
    
    if(gps.missing.count > 0){
      
      all.gps.missing = rbind(all.gps.missing,gps.missing)
      
    } else (all.gps.missing = gps.missing)
    
    gps.missing.count = gps.missing.count + 1
    
    
    
  }
  
}


## You can upload any missing GPS data for SM3 or SM3-BATs that you require here
# check what's missing

deployments.missing.gps = unique(all.gps.missing)

# get the points for the function units to transfer to SM3 units

unique.gps = unique(meta[c("real_transect","int_lat","int_lon")])


# import external gps information here

add.gps.sm3 = read.csv("S:/Projects/107182-01/06 Data/ARU processing/Supplemental_processing_data/Supplemental_GPS_Locations.csv",stringsAsFactors = F)


for(i in 1:nrow(meta)){
  
  if(is.na(meta$int_lat[i]) & is.na(meta$int_lon[i])){
    
    meta$int_lat[i] = add.gps.sm3[add.gps.sm3$Code == meta$real_transect[i],]$sm3.lat
    meta$int_lon[i] = add.gps.sm3[add.gps.sm3$Code == meta$real_transect[i],]$sm3.lon  
    
  } else if (is.na(meta$int_lat[i]) | is.na(meta$int_lon[i])){
    
    print(paste0(meta$filename[i]," missing either lat or lon"))
    
  }
  
  
  
}


unique.gps.check = unique(meta[c("real_transect","int_lat","int_lon")])



## combine and calculate the length of recordings metadata

check.size = meta %>% 
  group_by(filename,SampleRate,BitsPerSample,FileSize,length,NumChannels) %>% 
  summarise(calc_length = ((as.numeric(FileSize)*8)/(as.numeric(SampleRate)*as.numeric(BitsPerSample)))/NumChannels)

check.size$calc_length = floor(check.size$calc_length*10)/10

meta = merge(meta,check.size[c("filename","calc_length")],by = "filename")


## Check unit naming and data redundancies ~ You can re-save file after this

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

Track_output = file.path("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/MetaData",
                     "Review_all_recs_2023_Oct.txt")

capture.output(redun_name_status(meta = meta),file = Track_output,append = T)
redun_name_status(meta = meta)


############################
##### ~~~~ PART 2 ~~~~ #####
##### Create Database ######
############################


# set location for Database
meta.db = dbConnect(RSQLite::SQLite(),file.path(db.path,"20231022_Meta_Database.sqlite"))

# check status ~ if new, will get no info
dbListTables(conn = meta.db)
meta = as.data.frame(meta)

# create a table to keep data
if(!dbExistsTable(meta.db,"metadata")){
  
  dbWriteTable(meta.db,"metadata",meta)
  
  
} else {
  
  dbAppendTable(meta.db, "metadata", meta)
  
  
}



dbDisconnect(meta.db)









######### Run Script from here down if first section run once already ######## 
######### REVIEW FROM HERE DOWN SINCE FIRST PART WAS RUN EARLIER ###### 




############################
##### ~~~~ PART 3 ~~~~ #####
##### Time_Manipulation ####
############################



## Read data from last section (if running subesquent to first itteration)
meta.db = dbConnect(RSQLite::SQLite(),file.path(db.path,"20231022_Meta_Database.sqlite"))
meta_2 = dbReadTable(meta.db,"metadata") # read the file


dbDisconnect(meta.db)

# Remove anything that you don't want to deal with 
#meta_2 = meta_2[meta_2$real_prefix %in% meta$real_prefix,]

## Subset data here incase you dont want to process everything
# meta_2 = meta_2[meta_2$real_transect=="MKVI-04",]


## compare with current subset of recordings
#recs = data.frame(full = list.files("F:/PMRA_SAR/Recordings/BIRD/2023/MKVI/MKVI-04",full.names = T,recursive = T,pattern = ".wav"))

#recs$basename = basename(recs$full)

#meta_2$real_base = basename(meta_2$filename)

# list of missing
#recs.missing = recs[!recs$basename %in% meta_2$real_base,]

## **Caution** ~ Time calculations crossing over the daylight savings shift will come out as NAs unless timezone set to "ETC/GMT+7" or "ETC/GMT-9" etc.
## **Important** ~ In "ETC/GMT+7", 7 refers to the timezone offset (+/-0700) but the (+/-) is reversed, here the timezone is PDT/MST (-0700) even though the sign is +


### Categorize recordings into ordinal night ~ Function from the beginning of script
# if recordings are across jan 1, this function will not work
# you will need to add 365 to the second year of recordings to compensate. THen it will be number of nights since Jan 1, the year before



# test df for manipulations
meta_2$ynight = NA

# apply ordinal night function
for (i in 1:nrow(meta_2)){
  
  meta_2$ynight[i] = or.night(meta_2$form_tstamp[i],12,"2000-01-01",meta_2$tzone_R[i])
  
  
}

## Create real station ID for looping, based on correct names and not the (potentially incorrect) prefix field 
# Thankfully we've done this earlier, and just need to rename
colnames(meta_2)[colnames(meta_2)=="real_prefix"] <- c("station")




# new column for recording status 
meta_2$status = "recording"


# Create an end time for each recording 
meta_2$end_time = NA

### remove anything you dont want to deal with right now




# test settings
# start = meta_2$form_tstamp[i]
# format = "%Y-%m-%d %H:%M:%S%z"
# timezone = meta_2$tzone_R[i]
# length = meta_2$calc_length[i]
# 


get_end_time = function(start,format,timezone,length){
  
  end = strptime(start, 
                format = format, tz = timezone)+ceiling(lubridate::seconds(length))
  end = strftime(end, format = format, tz = timezone)
  
  return(end)
  
}


#i=1
for (i in 1:nrow(meta_2)){
  
  meta_2$end_time[i] = get_end_time(meta_2$form_tstamp[i],"%Y-%m-%d %H:%M:%S%z",meta_2$tzone_R[i],meta_2$calc_length[i])
  
  
}

colnames(meta_2)[colnames(meta_2)=="form_tstamp"] = "start_time"



## if you want to create jpegs with this, set the following to 'make'
make.jpgs = "make" # either use "make", if you want them made,or write literally anything else if you don´t want them made
period.labels = "no" # either use "make", if you want the length of the pauses/recording sessions printed next to the recording plot
                    # or write literally anything else if you don´t want them made

# sort by 'station', 'ynight' to create new df with sessions and pauses in them

# test with 1 first
# 
# station = unique(meta_2$station)[1]
# meta_stn = meta_2[meta_2$station == station,]
# ynight = unique(meta_stn$ynight)[1]
# meta_night = meta_stn[meta_stn$ynight == ynight,]




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
    timezone = unique(meta_stn$tzone_R)
    
    if(!length(timezone)==1){cat("\n\n ---- \n\nProblem with timezones in",ynight,"~",station)}
    
    
    # get two dates involved in this recording night
    date_range = unique(date(strptime(meta_night$start_time, format = "%Y-%m-%d %H:%M:%S%z" ,tz = timezone)))

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
    if(!length(unique(meta_night$int_lat)) == 1){ 
      cat("\n\n ---- \n\nProblem with latitude in",ynight,"~",station,"\n latitudes")
      
      for(lat in unique(meta_night$int_lat)){
        
        cat(lat,"\n")
        
      }
      
    }
    # longitude
    if(!length(unique(meta_night$int_lon)) == 1){
      cat("\n\n ---- \n\nProblem with longitude in",ynight,"~",station,"\n longitudes")
      
      for(lon in unique(meta_night$int_lon)){
        
        cat(lon,"\n")
        
      }
    }
    

    
    
    # snag sunrise sunset times here (need to deal with timezones first)

    
    
    # finished error check 
    
    ## calculate times and reformat existing time variables
    # get sunrise sunset times 
    sunrise = suncalc::getSunlightTimes(date = date(max(date_range)),lat = meta_night$int_lat[1],lon = -1*meta_night$int_lon[1], keep = c("sunrise"))
    sunset = suncalc::getSunlightTimes(date = date(min(date_range)),lat = meta_night$int_lat[1],lon = -1*meta_night$int_lon[1], keep = c("sunset"))
    
    meta_night$sunrisetime = strftime(with_tz(sunrise$sunrise,tzone = timezone),format = time_format,tz = timezone)
    meta_night$sunsettime = strftime(with_tz(sunset$sunset,tzone = timezone),format = time_format, tz = timezone)
    
    
    
    
    # loop through the recordings on the night
    # i=1
    for (i in 1:nrow(meta_night)){
      
      # new two df with data to keep for each session
      meta_rec = meta_night[i,]
      
      
      
      if (!i==nrow(meta_night)){
        
        # now use the subsequent recording entry to get end time and length for pause
        
        meta_pause = meta_rec # create record of pause
        meta_pause$filename = NA
        meta_pause$status = "pause" # change status to pause
        meta_pause$start_time = meta_rec$end_time # change start time to end time of recording
        meta_pause$end_time = meta_night[i+1,]$start_time # get start and end
        
        meta_pause$calc_length = as.numeric(lubridate::seconds(strptime(meta_pause$end_time,format = time_format,tz = timezone)) -
                                         
                                              lubridate::seconds(strptime(meta_pause$start_time, format = time_format, tz = timezone))) # length of pause
        
        # Now combine these two into a rec_pause dataframe
        session_rec = rbind(meta_rec,meta_pause)
        
      } else (session_rec = meta_rec)
      
      
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
    toplot$calc_length = as.numeric(toplot$calc_length)
   
    
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
                         xmax = as.POSIXct(end_time,format = time_format,tz = timezone)),linewidth = 8) +

      # 
      # geom_text(aes(x=as.POSIXct(start_time,format = time_format,tz = timezone)+calc_length/2,label = ifelse(calc_length>60,
      #                                                      round(calc_length/60,0.1),
      #                                                      paste0(floor(calc_length)," Sec")), 
      #                                                     vjust = ifelse(status == "recording",-1.5,+2))) + 
      
      
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


write.csv(sessions,file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/MetaData/Recording/2023_All_Meta_Nov15.csv",row.names = F)


############################
#### ~~~ Part 4 ~~~~ #######
#### Alternate Names #######
############################


## remove pause records from dataframe 
full.meta = sessions[sessions$status == "recording",]

# add basename field for easy manipulation
full.meta$base.name = basename(full.meta$filename)


# review full meta for patterns
check = full.meta %>% group_by(station,model) %>% summarise(format.char = nchar(base.name))

check.all = data.frame(unique(check))



## Create column for each alternate name you require
# start with basic renames 
full.meta$name.basic = NA
full.meta$name.basic.tz = NA


# i=2940
for (i in 1:nrow(full.meta)){
  
  full.name = full.meta$filename[i]
  base.name = full.meta$base.name[i]
  
  
  # Remove _0+1_, __0__ and __1__
  
  if(grepl("_0\\+1_",base.name)){
    
    new.basic = gsub(pattern = "_0\\+1_",replacement = "_",base.name) 
    
  } else if (grepl("__0__",base.name)){
    
    new.basic = gsub(pattern = "__0__",replacement = "_",base.name) 
    
  } else if (grepl("__1__",base.name)){
    
    new.basic = gsub(pattern = "__1__",replacement = "_",base.name) 
    
  } else (new.basic = base.name)
  
  
  
  # add timezone
  
  new.basic.tz = gsub(pattern = ".wav",replacement = paste0(full.meta$tzone[i],".wav"),new.basic)
  
  ## import into dataframe
  full.meta$name.basic[i] = new.basic
  full.meta$name.basic.tz[i] = new.basic.tz
  
  
  
}


### Create new names for the LDFCS formation

# Start with calculating the following:
# 1) Seconds since jan 1, 2000 for sorting and accounting for night crossing
full.meta$or.secs = NA

for (i in 1:nrow(full.meta)){
    
   full.meta$or.secs[i] = or.seconds(time = full.meta$start_time[i],
                                  start_time = "2000-01-01 00:00:00",
                                  tzone = full.meta$tzone[i],
                                  tzone_R = full.meta$tzone_R[i])
   
   full.meta$start.to.sunset[i] = as.numeric(strptime(full.meta$start_time[i],format = time_format,full.meta$tzone_R[i]) - strptime(full.meta$sunsettime[i],format = time_format,full.meta$tzone_R[i]),units = "secs")
   
   full.meta$start.to.sunrise[i] = as.numeric(strptime(full.meta$sunrisetime[i],format = time_format,full.meta$tzone_R[i]) - strptime(full.meta$start_time[i],format = time_format,full.meta$tzone_R[i]),units = "secs")
   
   
}


# double check formats again
check.2 = full.meta %>% group_by(station,model) %>% summarise(format.base = nchar(base.name),
                                                              format.basic = nchar(name.basic),
                                                              format.w.tz = nchar(name.basic.tz))

check.2 = data.frame(unique(check.2))


# Calculate time before and after sunset and sunrise
# use 6000 as cutoff for seconds before sunrise (1 and 2/3 hrs)
full.meta.use = full.meta[full.meta$start.to.sunset > -6000 & full.meta$start.to.sunrise > -6000,]
dont.use = full.meta[full.meta$start.to.sunset < -6000 | full.meta$start.to.sunrise < -6000,]

same.date = dont.use[as.Date(dont.use$sunrisetime) == as.Date(dont.use$sunsettime),]
same.check = full.meta[as.Date(full.meta$sunrisetime) == as.Date(full.meta$sunsettime),]
# You will need to start by setting the start time you wanna use


start.hr = 0
start.min = 0
start.sec = 0


# testing presets

station = "MKSC-03-S02" # unique(full.meta$station)[2]
meta_stn = full.meta.use[full.meta.use$station == station,]
meta_stn = meta_stn[order(meta_stn$or.secs),]
night = "8487" #unique(meta_stn$ynight)[1]
night_dat = meta_stn[meta_stn$ynight == night,]


# 
for (station in unique(full.meta.use$station)){
  
  meta_stn = full.meta.use[full.meta.use$station == station,]
  
  # sort station ID by time
  meta_stn = meta_stn[order(meta_stn$or.secs),]
  
  for (night in unique(meta_stn$ynight)){
    
    # night filter
    night_dat = meta_stn[meta_stn$ynight == night,]
    
    night_dat$new.start = NA
    

    # loop across to rename each individually
    # i=2
    for (i in 1:nrow(night_dat)){
      
      
      if(i==1) {
        
        night_dat$new.start[i] = paste0(substr(night_dat$end_time[i],1,10)," 00:00:00",night_dat$tzone[i])
        
        # hours=start.hr
        # minutes=start.min
        # seconds=start.sec 
      } else {
        
        # add time get end time for last recording
        # make new start time
        new.start = as.character(as.POSIXct(
          as.POSIXct(night_dat$new.start[i-1],format = time_format,tz = night_dat$tzone_R[i-1])+
            lubridate::seconds(night_dat$calc_length[i-1]),format = time_format,tz = night_dat$tzone_R[i]))
        

      night_dat$new.start[i] = paste0(new.start,night_dat$tzone[i])
        
      }
      
      
      
      
    }
    
    
    
    
    # save as some other dataframe  
    if (night == unique(meta_stn$ynight)[1]) {night_out = data.frame(night_dat)} else (night_out = rbind(night_out,data.frame(night_dat)))
    
  }
  
  if (station == unique(full.meta.use$station)[1]) {dat_out = data.frame(night_out)} else (dat_out = rbind(dat_out,data.frame(night_out)))
  
  print(paste0("Completed ",station))
  
}






## you may run into file issues here
check = dat_out %>% group_by(station,name.basic,model,new.start) %>% summarise(format.time = nchar(new.start))

check.2 = dat_out[dat_out$new.start=="NA-0700",]
## check which files arent supposed to be part of the schedule
# Should.remove = full.meta[as.numeric(substr(full.meta$start_time,12,13))<=15 & as.numeric(substr(full.meta$start_time,12,13))>=11,]


# make new ldfcs name
# time = dat_out$new.start[1]
time.to.name = function(time){
  
  
  str_sub(time,5,5) = ""
  str_sub(time,7,7) = ""
  str_sub(time,9,9) = "_"
  str_sub(time,12,12) = ""
  str_sub(time,14,14) = ""
  
  return(time)
}



dat_out$name.ldfcs = paste0(dat_out$station,"_",time.to.name(dat_out$new.start),".wav")










write.csv(dat_out,file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/MetaData/Recording/2023_All_meta_and_names_for_analysis.csv",row.names = F)




write.csv(Should.remove,file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/MetaData/Recording/2023_MKSC__Files_To_Remove",row.names = F)
































