#################################################################
########### Combine LDFCS for rapid assessment using timelapse ##################
#################################################################


# Re-set  your script when needed

rm(list=ls())
dev.off()
# packages
library(OpenImageR)
library(tidyverse)

##################################
#### Step 1 - Group   Indices#####
##################################


## Required settings ##


# pre-set transect/group
<<<<<<< HEAD
group = "MKDI-01"

dir_sep_base = "D:/PMRA_SAR/Processing/Timelapse_files/LDFCS/BIRD/2022/MKDI/by_night" # where the files are kept
dir_return_base = "D:/PMRA_SAR/Processing/Timelapse_Files/LDFCS/BIRD/2022/MKDI/Full_LDFCS" # where the combined files are to go
=======
group = "MKSC-U01-006"

dir_sep_base = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/Timelapse_files/LDFCS/BIRD/2022/MKSC/by_night/MKSC-U01/MKSC-U01-006" # where the files are kept
dir_return_base = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/Timelapse_files/LDFCS/BIRD/2022/MKSC/MKSC-U01/MKSC-U01-006" # where the combined files are to go
>>>>>>> e793c5404dfc2e4a0fde1d2a90589f179f1c21f4


dir_sep = paste0(dir_sep_base,"/",group) # where the files are kept
dir_return =  paste0(dir_return_base,"/",group) # where the combined files are to go

# list in files to make into jpg
img<-list.files(path = dir_sep,pattern = "2Maps.png",recursive = T,full.names = T)
# img = img[file.size(img)>80000]

# set file path for keeping the images in a place that can be referenced by the timelapse database
imgs = data.frame(full.name = img) # turn into dataframe
imgs$full.directory = dirname(imgs$full.name) # get directory for visualization

imgs$date = basename(imgs$full.directory) # date for name
imgs$station = basename(dirname(imgs$full.directory)) # station ID for reference

# Make new dir name from extracted info
imgs$new.dir = paste0(dir_return,"/",imgs$station)
imgs$new.name = paste0(imgs$new.dir,"/",imgs$station,"_",imgs$date,".jpg")

# copy file to new location
for(i in 1:nrow(imgs)){
  
  if (!dir.exists(imgs$new.dir[i])){dir.create(imgs$new.dir[i],recursive = T)}
  
}

file.copy(from = imgs$full.name,to = imgs$new.name,recursive = T)


