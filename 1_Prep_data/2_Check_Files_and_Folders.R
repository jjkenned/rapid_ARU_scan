##################################################
########### Check Recording Files ################
##################################################

## Script Sections

# 1) Get folder structure and depth 
# 2) Folder and file naming conventions
# 3) Insert and check names if needed
# 4) Choose list of files to copy and rename 
# 5) Copy and rename




# Re-set  your script when needed
dev.off()
rm(list=ls())

# libraries
library(stringr)
library(tidyverse)
library(av)

# set file path to location of recording files

par.dir = "D:/2022.WESO.NorthIsland.ARU Audio"


##### 1) Get Folder Structure & Depth ###### 

## list files ##
files = list.files(par.dir,full.names = T,recursive = T)
non_rec = data.frame(full.name = files[str_ends(files$full,".wav",negate = T),]) # check non-wav files 
### (CONVERT .wac, .w4v, .flac etc IF PRESENT) ###

recs = data.frame(full.name = files[str_ends(files$full,".wav"),]) # check wav files

# now get the recording length 
recs$duration = av_media_info(recs$full.name)$duration

# Now let's deal with name issues





