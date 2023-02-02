############################################
#### Extract Spectrograms with Species ##### 
############################################

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
library(DBI)
library(RSQLite)
library(dbplyr)
library(dplyr)

# Function for moving positive detection frames to new folder
db.path = "E:/PMRA_SAR/processing/Timelapse_files/RTS/BIRD/2022/TimelapseData_merged.ddb"
sp.list = c("BADO","GHOW","WESO","NOPO","NSWO")
dest.path = "C:/Users/jeremiah.kennedy/Documents/PMRA/Methods/Protocols/Rapid_Scanning_Training/Song Examples"
img.path.ext = dirname(db.path)

dbDisconnect(RTS.db)
# connect to database
RTS.db <- DBI::dbConnect(RSQLite::SQLite(), db.path)

# if you have a list of nights to process import here
dat.tbl = dbReadTable(RTS.db,"DataTable")
dat.tbl = data.frame(dat.tbl)


# Do species you want

# species = unique(sp.list)[1]
# i=7





### 
for (i in 6:10){
  
  sp = colnames(dat.tbl[i]) # get species you're moving
  
  imgs = dat.tbl[dat.tbl[i]>0,c("File","RelativePath")] # get names of files of positive detections
  
  # Create directory if it don't exist
  img.dir = paste0(dest.path,"/",sp) # make directory name
  if(!dir.exists(img.dir)){dir.create(img.dir)} # create if not present
  
  # now move images over
  imgs.full = paste0(img.path.ext,"/",imgs$RelativePath,"/",imgs$File) # image name based on ddb path name
  
  
  # loop through those names and copy to newly made folder
  for (j in 1:length(imgs.full)){
    
    file.copy(from = imgs.full[j],to = paste0(img.dir,"/",basename(imgs.full[j])))
              
              
  }
  
  
  
}
  