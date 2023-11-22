#######################################
#### Making Spectrograms with SoX ##### 
#######################################

# Re-set  your script when needed
dev.off()
rm(list=ls())


# set Timezone
Sys.setenv(TZ = "Etc/GMT+8") # "https://www.ibm.com/docs/en/cloudpakw3700/2.3.0.0?topic=SS6PD2_2.3.0/doc/psapsys_restapi/time_zone_list.htm"
Sys.timezone() # check
options(stringsAsFactors = FALSE)
time_format = "%Y-%m-%d %H:%M:%S%z"




# Library required packages
#library(seewave)
#library(tuneR)
#library(readxl)
library(lubridate)
#library(av)
library(tidyverse)
library(stringr)
#library(magick)
library(RSQLite)
library(RODBC)

# set functions you will want to use 

# use function to convert day to night ID
# day to night function
# source("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Script/Functions/Day_To_Night.txt")


source("sourced_Functions/Or_Night_20000101_Function.R")

# Set locations for source folder and destination folder 
# you may also want to have a temporary folder 

SourceFolder = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/clipped_recordings" # where your recording files are kept
OutputFolder =  "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/RTS/raw" # where saving images

# ldfcs data
ldfcs.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/Timelapse_files/LDFCS"
ldfcs.file = "IndicesProcessing4.ddb"

# meta data database
meta.path = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/processing/2022_Nawhitti/MetaData"
meta.db = "20231111_All_new_rec_Names_meta.csv"

list.files(meta.path)

# list the files you want
full.file = list.files(SourceFolder,recursive = T,full.names = T,pattern = ".wav") # full file names
rec.list = data.frame(Full = full.file)

rec.list$base = basename(rec.list$Full)

#### Read in LDFCS results file #### 
LDFC.res <- DBI::dbConnect(RSQLite::SQLite(), file.path(ldfcs.dir,ldfcs.file))

# if you have a list of nights to process import here
night.use = data.frame(tbl(LDFC.res,"DataTable"))
night.use = data.frame(night.use %>% select(File, ForProcessing))

dbDisconnect(LDFC.res)

#### read in  #### 

# break name up into components for station and date
night.use = night.use %>%
  separate(File,c("station","date"),"_") 

night.use$date = gsub(".jpg","",night.use$date)
night.use$date.form = paste0(substr(night.use$date,1,4),"-",substr(night.use$date,5,6),"-",substr(night.use$date,7,8))

station.nights = night.use[night.use$ForProcessing == "Process",] %>% group_by(station) %>% summarise(count = n())

### break recording list into station and date
rec.list$station = sapply(strsplit(rec.list$base,"_"), `[`, 1)
rec.list$date = sapply(strsplit(rec.list$base,"_"), `[`, 2)
rec.list$date = paste0(substr(rec.list$date,1,4),"-",substr(rec.list$date,5,6),"-",substr(rec.list$date,7,8))
rec.list$time = sapply(strsplit(rec.list$base,"_"), `[`, 3)
rec.list$time.stamp = paste0(rec.list$date," ",
                             substr(rec.list$time,1,2),":",
                             substr(rec.list$time,3,4),":",
                             substr(rec.list$time,5,11))











time = rec.list$time.stamp[1]


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


rec.list$tzone = lapply(rec.list$time.stamp,FUN = reform_tz, get = "timezone")
rec.list$tzone = as.character(rec.list$tzone)

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

rec.list$tzone_R = lapply(rec.list$tzone,FUN = R_timezone)
rec.list$tzone_R = as.character(rec.list$tzone_R)





# apply ordinal night function
for (i in 1:nrow(rec.list)){
  
  rec.list$ynight[i] = or.night(rec.list$time.stamp[i],12,"2000-01-01",rec.list$tzone_R[i],time_format = time_format)
  
  
}


# ###### OVERLAP with PB surveys ###### 


# apply ordinal night function
for (i in 1:nrow(night.use)){
  
  night.use$ynight[i] = or.night(night.use$date.form[i],12,"2000-01-01",tzone = "Etc/GMT+8",time_format = "%Y-%m-%d")
  
  
}




## Let's filter the nights to be processed within the 'night use' data obtained from TimeLapse results
night.filt = night.use[night.use$ForProcessing == "Process",c("station","ForProcessing","date.form","ynight")]
night.filt$stn.night = paste0(night.filt$station,"_",night.filt$ynight)

night.filt = night.filt[night.filt$station %in% station.nights[station.nights$count>2,]$station,]

rec.list$stn.night = paste0(rec.list$station,"_",rec.list$ynight)

# filter recordings to process
to.process = rec.list[rec.list$stn.night %in% night.filt$stn.night,]

station.effort = to.process %>% group_by(station) %>% tally() # double check station effort


## combine the recording length data
rec.length = meta[c("new_name","new_length")]
colnames(rec.length) = c("base","length")

filename = rec.length$base[1]

format.name = function(filename){
  
  name.parts = list()
  
  if(grepl(":",filename)){
    
    filename = gsub(":","",filename)
    
  } 
  if(grepl(" ",filename)){
    
    filename = gsub(" ","_",filename)
    
  } 
  
  if(str_count(filename,"-")>1){
 
    for(i in 1:(str_count(filename,"-")+1)){
      
      name.parts[[i]] = sapply(strsplit(filename,"-"), `[`, i)
  
      
      
    }
    
    tz = name.parts[[str_count(filename,"-")+1]]
    
    new.filename = paste(name.parts,collapse = "")
    
    filename = gsub(tz,paste0("-",tz),new.filename)
    
  } 
  
  return(filename)
  
  
}


rec.length$new.base = lapply(rec.length$base,format.name)

rec.length = rec.length[c("new.base","length")]
colnames(rec.length) = c("base","length")



meta_2 = merge(to.process,rec.length,by = "base")





# Dplyr for ordering recordings within day 
meta_2 = meta_2 %>% group_by(stn.night) %>% mutate(night.seq = order(time.stamp)) %>% arrange(station,ynight,night.seq) 

# write.csv(meta_2,file = file.path(meta.path,"Chosen_Nights_meta_2022.csv"),row.names = F)


# Basic Loop for making specs


# pre-loop, spectrogram settings/specifications
Interval <- 60 # x-axis length in seconds 

# the following can be set specifically in the loops if need be (see other write spectrograms scrpt)
Length=180 # total length of recording in seconds
Breaks=seq(0,Length,Interval) # sequence of break locations 




# pb <- txtProgressBar(min = 0, max = length(data), style = 3)





# site = unique(meta_2$prefix)[1]
for (site in unique(meta_2$station)){
  
  # What site we working with 
  print(paste0("started site ~ ",site))
  
  
  # Keep only appropriate site data
  dat_in = meta_2[meta_2$station==site,]
  
  all_nights = unique(dat_in$ynight)
  
  # Loop through recording nights 
  # j = 1
  for (j in 1:length(all_nights)){
    
    grp_night = all_nights[j]
    
    
    dat_ret = dat_in[dat_in$ynight %in% grp_night,]
    
    print("Millenial dates")
    print(unique(dat_ret$ynight))
    
    
    # Create directory for site specs
    dir.out = paste0(OutputFolder,"/",site,"/",grp_night,"/")
    if(!dir.exists(dir.out)){
      dir.create(dir.out,recursive = T)
    } 
    
    # through sessions within night
    # i=1
    for (i in 1:nrow(dat_ret)){
      
      # all info for recording
      dat_use = dat_ret[i,]
      
      # the following can be set specifically in the loops if need be (see other write spectrograms scrpt)
      Length=dat_use$length # total length of recording in seconds
      Breaks=seq(0,Length,Interval) # sequence of break locations 
      
      ptm = proc.time()
      
      # loop through 60 sec periods
      # k=1
      for (k in 1:(length(Breaks)-1)){
        
        ### processing time
       
        ### 
        
        # set start and end of segment within recordings
        Start = Breaks[k]
        End = Breaks[k+1]
        
        
        # create name for each time image
        
        name=paste0(gsub(pattern = "*.wav",replacement = "",x = basename(dat_use$base)),"_",formatC(Start, width = 3,flag = 0))
        
        name=paste(name,"png",sep = ".")
        
        full.name = paste0(dir.out,name) # full name
        
        # 
        recording = dat_use$Full # Identify file name required here
        
          
          # create sox command line
          args_command = paste0(recording,
                                " -n remix 1 rate 12k trim ", Start," ", Interval, " spectrogram -r -z 90 -x 1500 -y 1200 -o ", # -o always goes at the end
                full.name)
          
          
          
          system2("sox",
                  args = args_command)
          
          
          
          
          
          
          
          
        
        
        
        
      }
      
      print(proc.time() - ptm)
      
    }
    
    
    
    
    # Track sessions
    # 
    print(paste0("Session ",i," of ", max(dat_in$night.seq)," site ",site))    
  }
  
  
  
  
  
}



# NOw it's time to clip those nasty sox images to the right size


## Function for this

# This function, trims all files within a give directory 


# preset values for function 
dir = OutputFolder

# Image sizing format (and default) is "1500x600+0+600" where 1500(x) = width, (x)600 = height, (+)0 = x offset, (+)600 = y offset
# x offset is pixels from the left edge of image
# y offset is pixels from the top edge of image

# Set image Sizing
image.sizing = "1500x600+0+600" 

# Function 

trim.sox.folder = function(dir,image.sizing){
  
  # tell me what you're doing
  print(paste0("Listing files in ~ ",dir))
  cat("\n") # space
  
  images = list.files(dir,full.names = T,recursive = T,pattern = ".png") # list files
  
  image.frame = data.frame(images) # into dataframe
  
  image.frame$size = file.info(image.frame$images)$size # get size of images
  
  remove = image.frame[!image.frame$size>0,] # list problem files
  
  if (nrow(remove)>0){print(paste0("Removing following files:",basename(remove$images)))
  } else (print("No missing png files"))
  cat("\n") # space
  
  image.frame = image.frame[image.frame$size>0,] # filter fake images
  
  image_use = image.frame$images # 
  
  
  
  image=image_use[1]
  for (image in image_use){
    
    pic = image_read(image)
    
    out = image_crop(pic,image.sizing)
    nameout = gsub(pattern = ".png",replacement = ".jpg",image)
    nameout = gsub(pattern = "/raw/",replacement = "/clipped/",nameout)
    
    if (!dir.exists(dirname(nameout))){
      
      dir.create(dirname(nameout),recursive = T)
      
    } 
    image_write(out,path = nameout)
    
  }
  
  
  
}


trim.sox.folder(dir,image.sizing)


# check if anything made it to the destination with PNG
images = list.files(OutputFolder,full.names = T,recursive = T,pattern = ".png")
image.frame = data.frame(images)
image.frame$size = file.info(image.frame$images)$size
image.frame = image.frame[image.frame$size>0,]






















