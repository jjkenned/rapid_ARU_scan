#######################################################################################
########### Re-name Indices output files for LDFCS formation         ##################
#######################################################################################

## Script Sections

# 1) Build new names based on recording names in station file
# this means you need to make sure that the ind.path is the transect ID 
# 2) 
#





# Re-set  your script when needed
dev.off()

rm(list=ls())

# libraries
library(stringr)
library(tidyverse)
library(fs)
library(av)
library(chron)
library(seewave)
library(lubridate)
library(rjson)
library(jsonlite)
library(warbleR)
#library(SciencesPo)

# set file path to location of indices files and database
# set transect ID
transect = "MKVI-04"
region = substr(transect,1,4)
year = "2023"

ind.root = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/BIRD"
rec.root = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Recordings/BIRD"
results.root = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/Name_Change_Tracking/BIRD"

ind.path = file.path(ind.root,year,region,"by_rec",transect)
rec.path = file.path(rec.root,year,region,transect)
results.path = file.path(results.root,year,region,transect,"new_name_indices")

## database setup
db.path = "S:/Projects/107182-01/06 Data/ARU processing/meta_data"
db.name = "20231022_Meta_Database.sqlite"

# get rid of problem files before running script


# Get depth that ends with 'towsey.acoustic


# Get file paths and info 
# File paths for indices folders
paths = fs::dir_info(path = ind.path,recurse = 2) %>% # two depth 
  as_tibble() %>% 
  filter(blocks==0) %>% # set block allocation to 0
  select(c("path")) # keep only path 
colnames(paths) = "ind.path"


# file path for corresponding ARU recordings
recs = fs::dir_info(path = rec.path,recurse = T) %>% 
  as_tibble() %>% # set block allocation to 0
  filter(type=="file") %>% # keep files and not folders
  select(c("path")) %>%
  filter(grepl(".wav",path,ignore.case = T))
colnames(recs) = "rec.path"  


## quick check the legnths and see what's missing
nrow(paths) == nrow(recs)


# now combine by the basename of the files 
paths$basename = basename(paths$ind.path) # Base name of indices folder
recs$basename = basename(recs$rec.path)

pathstest = merge(paths,recs,by = "basename")

# if everything checks out
paths = pathstest

# extract metadata required
paths$rec.name = gsub(".wav","",paths$basename) # get rid of wav name

# contingencies for SM3s
# make a baseline rec name that removes sm3 issues
paths$base.rec = gsub("_0\\+1_","_",paths$rec.name)

meta = separate(paths,base.rec,into = c("station","date","time"),sep = "_") # get recording time, date and location data
meta$transect = substr(meta$station,1,7)



############################
##### ~~~~ PART 2 ~~~~ #####
##### Read from Database ######
############################

# set location for Database
meta.db = dbConnect(RSQLite::SQLite(),file.path(db.path,db.name))

# check status ~ if new, will get no info
dbListTables(conn = meta.db)
meta = as.data.frame(meta)





###### Dates are frustrating ###### 

# let's do it the hard way !
# Get julian date setting the start date to jan 1, 2000
# date is in format yyyymmdd

get.jday = function(date){
  
  # pull date info 
  yrs = as.numeric(substr(date,1,4))
  mons = as.numeric(substr(date,5,6))
  days = as.numeric(substr(date,7,8))
  true.date = ymd(paste(yrs,mons,days,sep = "-")) # convert to date
  jdays = as.numeric(true.date - ymd("2000-01-01")) # days since
  
  
  return(jdays)
  
  
  
}

# Apply function to all data
meta$j.day = mapply(get.jday,meta$date)


# let's get time info from recording
meta$hr = as.numeric(substr(meta$time,1,2))
meta$min = as.numeric(substr(meta$time,3,4))
meta$sec = as.numeric(substr(meta$time,5,6))

meta$day = as.numeric(substr(meta$date,7,8))
meta$month = as.numeric(substr(meta$date,5,6))
meta$year = as.numeric(substr(meta$date,1,4))




# quick function for translating day number into night number 
# day = night ID, hr = hour in 24 hr clock (as integer), split = when to cut off the nights from one another (usually 12...noon)
d2n.func = function(day,hr,split){
  
  if (hr<split){night = day} else (night = day+1)
  
  return(night)
  
}





# apply function accross the dataframe 
meta$j.night = mapply(d2n.func,meta$j.day,meta$time,12) 

# now let's get night ID including jnight and station ID
file.grps = meta %>% group_by(station,j.night) %>% mutate(night.ID = cur_group_id()) # group by date



# now let's put these recordings in order by the absolute number of seconds since 2000-01-01
file.grps$sec_order=NA
for (i in 1:nrow(file.grps)){
  
  file.grps$sec_order[i] = (86400*as.numeric(file.grps$j.day[i])) + 
    as.numeric(file.grps$sec[i]) + 
    (as.numeric(file.grps$min[i])*60) + 
    (as.numeric(file.grps$hr[i])*60*60)
  
  
  
}





# Now we can loop through the files to rename
# You will need to start by setting the start time you wanna use
start.hr = 0
start.min = 0
start.sec = 0


# night = unique(file.grps$night.ID)[1]
for (night in unique(file.grps$night.ID)){
  
  
  # night filter
  night_dat = file.grps[file.grps$night.ID == night,]
  
  # sort by time
  night_dat = night_dat[order(night_dat$sec_order),]
  
  # loop across to rename each individually
  # i=1
  for (i in 1:nrow(night_dat)){
    
    
    if(i==1) {
      night_dat$hr[i]=start.hr
      night_dat$min[i]=start.min
      night_dat$sec[i]=start.sec 
    } else {
      start.time.seconds=(night_dat$hr[i-1]*60*60)+(night_dat$min[i-1]*60)+(night_dat$sec[i-1]) # get start time in seconds
      add.time = ceiling(av_media_info(night_dat$rec.path[i-1])$duration) # recording duration
      new.time.seconds = start.time.seconds + add.time # add times to see what it should start as
      night_dat$hr[i] = floor(new.time.seconds/3600)# convert back to hours minutes seconds with NO PACKAGE BECAUSE FUCK TIME! 
      night_dat$min[i] = floor(new.time.seconds/60)-(night_dat$hr[i]*60) # mintues 
      night_dat$sec[i] = new.time.seconds - (night_dat$hr[i]*3600+night_dat$min[i]*60) # seconds
      
      # and assign dates so we dont get no problems corsssssssssing across the nightssssss
      night_dat$year[i]=night_dat$year[1]
      night_dat$month[i]=night_dat$month[1]
      night_dat$day[i]=night_dat$day[1]
    }
    
    
  }
  
  # save as some other dataframe  
  if (night == unique(file.grps$night.ID)[1]) {dat_out = data.frame(night_dat)} else (dat_out = rbind(dat_out,data.frame(night_dat)))
  
  print(paste0("complete night ",night," of ",max(unique(file.grps$night.ID)))) # nights may not line up propperly 
  
}




# manually recreate the names based on recording times etc
dat_out$new_name_final = paste0(dat_out$station,"_",dat_out$year,
                                formatC(dat_out$month,width = 2,flag = 0),
                                formatC(dat_out$day,width = 2,flag = 0),"_",
                                formatC(dat_out$hr,width=2,flag = 0),
                                formatC(dat_out$min,width = 2, flag = 0),
                                formatC(dat_out$sec,width=2,flag=0),"-0700.wav")


###
# Save this massive list 
dir.create(results.root,recursive = T)
write.table(dat_out,file = paste0(results.root,"/Old_New_NameConverstions.txt"),sep = "\t",col.names = T,row.names = F)

# read data if needed to re-run
# dat_out = read.table("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/BIRD/2023/MKSC/by_rec/MKSC-01/new_name_indices/Old_New_NameConverstions.txt",sep = "\t",header = T)


###### MKSC-01-S10 check what we can chagne properly # didnt change first time






# now list and rename things

## for testing
# indices.enclosing.folder = dat_out$ind.path[1]
# new.basename = dat_out$name.ldfcs[1]
# old.basename = dat_out$base.name[1]


# Function

change.index.output.names = function(indices.enclosing.folder,new.basename,old.basename){
  
  # get basename file name to work with
  base.rec.ID.old = gsub(pattern = ".wav",replacement = "",old.basename)
  base.rec.ID.new = gsub(pattern = ".wav",replacement = "",new.basename)
  
  
  ## list files in index output folder
  sub.files = fs::dir_info(path = indices.enclosing.folder,recurse = T) %>%
    as_tibble() %>%
    filter(type=="file") %>% # keep files and not folders
    select(c("path"))
   
  ## Copy files into new location with new name
  # set new location name
  sub.files$new.files = gsub("by_rec","new_name_indices",sub.files$path)
  sub.files$new.files = gsub(base.rec.ID.old,base.rec.ID.new,sub.files$new.files)
  
  # copy files over and rename
  dir.create(dirname(sub.files$new.files[1]),recursive = T) # create new dir
  file.copy(sub.files$path,sub.files$new.files,recursive = T) # copy with new names
  
  # read the one json file we need
  json.path = sub.files$new.files[grepl("IndexGenerationData.json",sub.files$new.files,fixed = T)]
  json.old = jsonlite::fromJSON(json.path,simplifyVector = F)
  
  json.old$RecordingBasename = base.rec.ID.new
  
  json.new = jsonlite::toJSON(json.old,pretty = T,auto_unbox = T, na='null',null = 'null')
  
  write(json.new,json.path)
  
  
  
  
}

# apply function 
mapply(change.index.output.names,
       indices.enclosing.folder = dat_out$ind.path,
       new.basename = dat_out$new_name_final,
       old.basename = dat_out$basename)

# subset

#dat_ret = dat_out[5201:nrow(dat_out),]

#mapply(change.index.output.names,
       #indices.enclosing.folder = dat_ret$ind.path,
       #new.basename = dat_ret$new_name_final,
       #old.basename = dat_ret$basename)
# 
# 
# # make some changes to the json files
# json.list = fs::dir_info(path = "D:/PMRA_SAR/Processing/Timelapse_files/LDFCS/BIRD/2022/MKDI/new_name_indices",recurse = T) %>%
#   as_tibble() %>%
#   filter(type=="file",grepl("IndexGenerationData.json",path,ignore.case = T)) %>% # keep files and not folders
#   select(c("path"))
# 
# 
# 
# # now let's rename the files
# # name = json.list$path[1]
# 
# 
# null.json = function(name){
#   
#   json.old = jsonlite::fromJSON(name,simplifyVector = F)
#   
#   
#   
# 
#     
#     
#   lapply(json.old, function(x) if (length(x) == 0) NA else x)
#     
#  
#   
#   
#   
#   json.new = jsonlite::toJSON(json.old,pretty = T,auto_unbox = T,null = "null",na="null")
#   
#   write(json.new,paste0(gsub("IndexGenerationData.json","IndexGenerationData_TEST.json",name)))
#   
#   print(dirname(name))
#   
#   
#   
# }
# 
# 


















