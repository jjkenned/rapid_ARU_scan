#######################################
#### Making Spectrograms with SoX ##### 
#######################################

# Re-set  your script when needed
dev.off()
rm(list=ls())


# Library required packages
library(seewave)
library(tuneR)
library(readxl)
library(lubridate)
library(av)
library(tidyverse)
library(magick)
library(RSQLite)
library(RODBC)

# set functions you will want to use 

# use function to convert day to night ID
# day to night function
# source("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Script/Functions/Day_To_Night.txt")




# Set locations for source folder and destination folder 
# you may also want to have a temporary folder 


SourceFolder = "E:/PMRA_SAR/Recordings/BIRD/2022/MKVI/MKVI-24" #where your recording files are kept
OutputFolder =  "E:/PMRA_SAR/Processing/Timelapse_files/RTS/BIRD/2022/MKVI/MKVI-24/raw" # where saving images



# list the files you want
full.file = list.files(SourceFolder,recursive = T,full.names = T,pattern = ".wav") # full file names
file.name = basename(full.file)


# Get metadata for looping
meta = songmeter(file.name)
meta$file.name = full.file

#### Add selection list of files or nights #### 
LDFC.res <- DBI::dbConnect(RSQLite::SQLite(), "E:/PMRA_SAR/processing/Timelapse_files/LDFCS/BIRD/2022/MKVI/MKVI-24/IndicesProcessing2.ddb")

# if you have a list of nights to process import here
night.use = tbl(LDFC.res,"DataTable") 
night.use = data.frame(night.use %>% select(File, Process))

# do you need to change the output to match new naming conventions?
# source("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Script/Functions/Update_Prefix_Name_Bird.R")
# night.use$File.New = mapply(name.change.u2s,night.use$File)


### Check if playback surveys are from the same night ##### 
# let's see what's been done for the group of files you want to work on 
# by default it's to site
site = basename(SourceFolder)

# break name up into components for station and date
night.use = night.use %>%
  separate(File,c("station","date"),"_") 

night.use$date = gsub(".jpg","",night.use$date)

# count our station nights

station.nights = night.use %>% group_by(station,Process) %>% summarise(nights = n())
nights.process.stn = station.nights[station.nights$Process == "Process",]

# ## if everything adds up to your desired count number, you can proceed
# 
# ###### OVERLAP with PB surveys ###### 
# 
# 
# 
# 
# # if you have survey night data from playbck sessions, include it here
# playback.db = odbcConnectAccess("S:/Projects/107182-01/06 Data/221115c_WESOke Survey Dbase.accdb")
# 
# # let's use these to filter our data
# playback = playback[c("Full_Station_ID","Date","Start_Time")] # playback surveys
# 
# playback$or.day = yday(playback$Date) # set ordinal date values
# 
# playback$hour = hour(playback$Start_Time) # get hour 
# 
# playback<-playback[!is.na(playback$Full_Station_ID),] # remove nas
# 
# playback = playback[c("Full_Station_ID","Date","or.day","hour")] # removed what isnt needed
# 
# # get site nights for the playback sessions 
# playback$site = gsub(x = substr(playback$Full_Station_ID,1,8),pattern = "T",replacement = "U")  # site ID
# 
# playback$or.night = mapply(d2n.func,playback$or.day,playback$hour,12)
# 
# 
# # now convert back to date
# playback$new.date = as.character(as.Date(playback$or.night,origin = "2020-12-31"))
# playback$new.date = gsub("-","",playback$new.date)
# 
# dont_use = unique(playback[c("site","new.date")])


##### back to filtered nights #####

# Extract metadata from file name  in night images (get from end so you don't need to deal with prefix length)
# date from end

# get Millenial date (days since start of millenium)
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
night.use$j.day = mapply(get.jday,night.use$date)


# Now use to process again to get just the stuff to process
to.process = night.use[night.use$Process=="Process",] # nights selected
to.process = to.process[substr(to.process$station,1,7)==site,] # by site
station.effort = to.process %>% group_by(station) %>% tally() # double check station effort




### NOW  continue

## work with time carefuly 
# extract only the numeric date


meta$date = paste0(meta$year,formatC(meta$month,width = 2,flag = 0),formatC(meta$day,width = 2,flag = 0))


meta$j.day = mapply(get.jday,meta$date)



# summarize recordings per day and days per station
unique(meta$prefix) # what stations are available?

meta$size = as.numeric(file.size(meta$file.name)) #  size of the files


# 
# # Now let's see what's gonna get processed 
# night.use$j.night = yday(as.Date(as.character(substr(night.use$File,14,22)),format = "%Y%m%d"))
# 
# uses = night.use[night.use$Process=="Process",]
to.process$station.night = paste0(to.process$station,"_",to.process$j.day)





# convert to night ID and other for recordings


d2n.func = function(day,hr,split){
  
  if (missing(split)){split=12}
  
  if (hr<split){night = day - 1} else (night = day)
  
  return(night)
  
}

# 




meta$j.night = mapply(d2n.func,meta$j.day,meta$hour,12)

meta$station.night = paste0(meta$prefix,"_",meta$j.night)# make night ID

# now filter by the chosen night IDs
meta_2 = meta[meta$station.night %in% to.process$station.night,]


# check
# meta[!yday(meta$Date)==meta$or.day,]



# Get file duration in seconds for looping across appropriate time frames
meta_2$duration = av_media_info(meta_2$file.name)$duration




# some QAQC
# missing.dat = meta[meta$size == 0,]
# write.table(missing.dat,"C:/Users/jeremiah.kennedy/Documents/Rayrock/Tracking/missing_any_data.txt",sep = " ")

# remove empty files from list
# meta = meta[meta$size>0,]

# Dplyr for ordering recordings within day 
meta_2 = meta_2 %>% group_by(station.night) %>% mutate(night.seq = order(time)) %>% arrange(prefix,j.night,night.seq) 

# write.csv(meta_2,file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/Tracking/Chosen_Nights_meta_MKSC_02_2022.csv",row.names = F)







# Basic Loop for making specs


# pre-loop, spectrogram settings/specifications
Interval <- 60 # x-axis length in seconds 

# the following can be set specifically in the loops if need be (see other write spectrograms scrpt)
Length=180 # total length of recording in seconds
Breaks=seq(0,Length,Interval) # sequence of break locations 




# pb <- txtProgressBar(min = 0, max = length(data), style = 3)





# site = unique(meta_2$prefix)[1]
for (site in unique(meta_2$prefix)){
  
  # What site we working with 
  print(paste0("started site ~ ",site))
  
  
  # Keep only appropriate site data
  dat_in = meta_2[meta_2$prefix==site,]
  
  all_nights = unique(dat_in$j.night)
  
  # Loop through recording nights 
  # j = 1
  for (j in 1:length(all_nights)){
    
    grp_night = all_nights[j]
    
    
    dat_ret = dat_in[dat_in$j.night %in% grp_night,]
    
    print("Millenial dates")
    print(unique(dat_ret$j.day))
    
    
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
      Length=dat_use$duration # total length of recording in seconds
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
        
        name=paste0(gsub(pattern = "*.wav",replacement = "",x = basename(dat_use$file.name)),"_",formatC(Start, width = 3,flag = 0))
        
        name=paste(name,"png",sep = ".")
        
        name=gsub(pattern = "_0//+1_",replacement = "_", name) # final name
        full.name = paste0(dir.out,name) # full name
        
    
        
        # 
        recording = dat_use$file.name # Identify file name required here
        
          
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




trim.sox.folder = function(dir,height,width,min,max){
  
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
  
  # set spectrogram sizing 
  # start by setting x and y pixel count
  
  
  
  image=image_use[1]
  for (image in image_use){
    
    pic = image_read(image)
    width = pic.columns
    height = pic.rows
    
    out = image_crop(pic,"1500x600+0+600")
    nameout = gsub(pattern = ".png",replacement = ".jpg",image)
    nameout = gsub(pattern = "/raw/",replacement = "/clipped/",nameout)
    
    if (!dir.exists(dirname(nameout))){
      
      dir.create(dirname(nameout),recursive = T)
      
    } 
    image_write(out,path = nameout)
    
  }
  
  
  
}






images = list.files(OutputFolder,full.names = T,recursive = T,pattern = ".png")

895

image.frame = data.frame(images)

image.frame$size = file.info(image.frame$images)$size

image.frame = image.frame[image.frame$size>0,]


image_use = image.frame$images[2614:nrow(image.frame)]


image_use = image.frame$images

for (image in image_use){
  
  pic = image_read(image)
  out = image_crop(pic,"1500x600+0+600")
  nameout = gsub(pattern = ".png",replacement = ".jpg",image)
  nameout = gsub(pattern = "/raw/",replacement = "/clipped/",nameout)
  
  if (!dir.exists(dirname(nameout))){
    
    dir.create(dirname(nameout),recursive = T)
    
  } 
  
  image_write(out,path = nameout)
  
}

# for(image in images){
#   
#   pic = image_read(image)
#   out = gsub(pattern = ".png",replacement = ".jpg",image)
#   image_write(pic,path = out)
#   
# }
# 






















for(i in 1:length(data)){
  
  FileLoc=data[i] 
  FileOutput=basename(FileLoc)
  FileOutput=paste0(TempFolder,FileOutput)
  
  
  
  wave <- readWave(FileLoc) #this is where Laura's is broken - if you're getting no read permission error come see us. might be a package version issue.
  duration <- 60 #duration(wave) - this is where I only use the first minute of each recording -- better solution?
  intervals <- ceiling(as.integer(duration)/x) 
  
  filename <- WavData
  filename=basename(filename) #trying as.character(filename) to get around the $ issue didn't work
  j=1 #j is the image number. So I have 1-6.
  for(j in 1:intervals){ #using sox to make pngs ##play with (j-1) to adjust for where in recording to start ###prettiness: extent of yaxis, colours
    
    
    sox (paste(WavData, " -n trim ", (j-1)*x, " ", x, " rate 44k spectrogram -z 90 -o ", # -o always goes at the end
               OutputFolder,
               substr(filename, 1, nchar(filename)-4), "_", (j-1)*10, ".png", sep = ""), #naming png file names (removing the .wav)
         path2exe = "/Users/JillianCameronold/Documents/Grad School/Sox") #this is where sox program is saved
  }
  
  
  setTxtProgressBar(pb, i)
}

close(pb)

