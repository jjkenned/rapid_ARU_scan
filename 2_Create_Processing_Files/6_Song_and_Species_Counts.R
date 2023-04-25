######################################################
#### Song Counts and Species Counts for Stations ##### 
######################################################

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
db.path = "E:/PMRA_SAR/Processing/Timelapse_files/RTS/BIRD/2022/MKDI/MKDI-01/clipped/TimelapseData_merged.ddb"
sp.list = c("BADO","GHOW","WESO","NOPO","NSWO")
output.path = "E:/PMRA_SAR/Results/BIRD/2022/MKDI/MKDI-01"

# dest.path = "C:/Users/jeremiah.kennedy/Documents/PMRA/Methods/Protocols/Rapid_Scanning_Training/Song Examples"
# img.path.ext = dirname(db.path)

# dbDisconnect(RTS.db)
# connect to database
RTS.db <- DBI::dbConnect(RSQLite::SQLite(), db.path)

# if you have a list of nights to process import here
dat.tbl = dbReadTable(RTS.db,"DataTable")
dat.tbl = data.frame(dat.tbl)



##### is everything completed ##### 
incomplete = dat.tbl %>% group_by(RelativePath,Processed) %>% summarise(count = n())
# nothing incomplete? 


## Next, group by nights and species 
# start by creating station and night ID
dat.tbl$station = substr(dat.tbl$RelativePath,1,11)
dat.tbl$night = substr(dat.tbl$RelativePath,13,16)

dat.tbl$WESO = as.numeric(dat.tbl$WESO)
dat.tbl$BADO = as.numeric(dat.tbl$BADO)
dat.tbl$NOPO = as.numeric(dat.tbl$NOPO)
dat.tbl$NSWO = as.numeric(dat.tbl$NSWO)
dat.tbl$GHOW = as.numeric(dat.tbl$GHOW)






species.counts = dat.tbl %>% group_by(station,night) %>% summarise(WESO = sum(WESO),
                                                                   BADO = sum(BADO),
                                                                   NOPO = sum(NOPO),
                                                                   NSWO = sum(NSWO),
                                                                   GHOW = sum(GHOW))

# Save data
# make path if it doesn't exsist 

if (!dir.exists(output.path)){
  
  dir.create(output.path,recursive = T)
  
}

# make output name
filename = paste0(output.path,"/","MKDI-01_2022_Song_Counts.csv")

write.csv(species.nights, file = filename, row.names = F)




# now let's get the station level '# nights with vocalizatrions' values

species.nights = species.counts %>% group_by(station) %>% summarise(nights_processed = n(),
                                                                    WESO = n_distinct(night[WESO>0]),
                                                                    BADO = n_distinct(night[BADO>0]),
                                                                    NOPO = n_distinct(night[NOPO>0]),
                                                                    NSWO = n_distinct(night[NSWO>0]),
                                                                    GHOW = n_distinct(night[GHOW>0]))


# make output name
name.nights =  paste0(output.path,"/","MKDI-01_2022_Number_Nights.csv")

write.csv(species.nights, file = name.nights,row.names = F)







  