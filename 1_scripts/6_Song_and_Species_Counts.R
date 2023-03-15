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
db.path = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/Timelapse_files/RTS/BIRD/2022/MKSC/MKSC-02/clipped/TimelapseData_merged.ddb"
sp.list = c("BADO","GHOW","WESO","NOPO","NSWO")
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

# now let's get the station level '# nights with vocalizatrions' values

species.nights = species.counts %>% group_by(station) %>% summarise(WESO = n_distinct(night[WESO>0]),
                                                                    BADO = n_distinct(night[BADO>0]),
                                                                    NOPO = n_distinct(night[NOPO>0]),
                                                                    NSWO = n_distinct(night[NSWO>0]),
                                                                    GHOW = n_distinct(night[GHOW>0]))



write.csv(species.nights, file = "S:/Projects/107182-01/06 Data/ARU processing/Data Output/20230217_GOldenears_Night_Counts.csv",row.names = F)

















  