###############################################################################################
########### Export and Combine Observation Data for Generalized Output ########################
###############################################################################################


## This script is for:
# ~ 



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

# libraries
library(tidyverse)
library(RSQLite)
library(RODBC)
library(DBI)
library(fs)
library(data.table)
library(reshape)


##################
### Option 1 #####
##################

# Straight from the database and manual extraction 

## define locations for 
# parent directory of training material to move
prnt.source = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Recordings/BIRD/2022/MKVI" # Base folder with recordings present 

# MetaData and output directories
base.meta.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/MetaData" 
GPS.Locs = file.path(base.meta.dir,"External","20231011_Location_Info.csv") # aru locations
jpg.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Recordings/BIRD/2022/MKVI" # images for rec schedule review

# set location for metadata Database
db.path = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Training/reference_Files"
db.name = "CoastOwlsBC.ddb"

RTS.db <- DBI::dbConnect(RSQLite::SQLite(), file.path(db.path,db.name))
DBI::dbListTables(RTS.db)
# Read data from specified table
dat.tbl = dbReadTable(RTS.db,"DataTable")
dat.tbl = data.frame(dat.tbl)
# dat.tbl = dat.tbl[dat.tbl$Processed=="true",]
  
dbDisconnect(RTS.db) # disconnect after extracted


dat.use = dat.tbl[substr(dat.tbl$File,1,4) == "MKVI",]


table(nchar(dat.use$File))

dat.use$new.name = NA  




for (i in 1:nrow(dat.use)){
  
  base.name = dat.use$File[i]
  
  # Remove _0+1_, __0__ and __1__
  
  if(grepl("_0\\+1_",base.name)){
    
    new.basic = gsub(pattern = "_0\\+1_",replacement = "_",base.name) 
    
  } else if (grepl("__0__",base.name)){
    
    new.basic = gsub(pattern = "__0__",replacement = "_",base.name) 
    
  } else if (grepl("__1__",base.name)){
    
    new.basic = gsub(pattern = "__1__",replacement = "_",base.name) 
    
  } else (new.basic = base.name)
  
  
  
  ## import into dataframe
  dat.use$new.name[i] = new.basic
  
  
  
}




table(nchar(dat.use$new.name))




gps = read.csv(GPS.Locs,stringsAsFactors = F)

i=1
for(i in 1:nrow(dat.use)){
  
  dat.use$station[i] = substr(dat.use$new.name[i],1,11)
  year = substr(dat.use$new.name[i],13,16)
  
  month = substr(dat.use$new.name[i],17,18)
  day = substr(dat.use$new.name[i],19,20)
  
  hour = substr(dat.use$new.name[i],22,23)
  
  min = substr(dat.use$new.name[i],24,25)
  
  sec = substr(dat.use$new.name[i],26,27)
  
  dat.use$date_time[i] = paste0(year,"-",month,"-",day," ",hour,":",min,":",sec,"-0700")
  
  
  
}


# Summarize by 3min recordings
str(dat.use$WESO)
dat.out = dat.use %>% group_by(station,date_time) %>% summarise(WESO = sum(as.numeric(WESO)),
                                                                BADO = sum(as.numeric(BADO)),
                                                                NOPO = sum(as.numeric(NOPO)),
                                                                NSWO = sum(as.numeric(NSWO)),
                                                                GHOW = sum(as.numeric(GHOW)))

i=1
for(i in 1:nrow(dat.out)){
  
  remove("lat")
  remove("lon")
  
  
  stn = as.character(dat.out$station[i])
  lat = (gps[gps$Full_Station_ID == stn,]$DecimalLatitude)
  lon = (gps[gps$Full_Station_ID == stn,]$DecimalLatitude)
  
  if(!length(lat) == 0 & !length(lon) == 0){
    
    dat.out$lat[i] = gps[gps$Full_Station_ID == dat.out$station[i],]$DecimalLatitude
    dat.out$lon[i] = gps[gps$Full_Station_ID == dat.out$station[i],]$DecimalLongitude
    
  } else {
    
    dat.out$lat[i] = NA
    dat.out$lon[i] = NA
    
    if(!stn == dat.out$station[i-1]){
      
      print(paste0("No lat lon values for ",stn))
      
    }
    
    
  }
  
  
  
}

check = unique(dat.out[c("station","lat","lon")])

table(check$lon)


# imput alternative locations for these deployments

gps.replace = gps[gps$Common_Stn_ID %in% c("MKVI-24-4"),]

dat.out[dat.out$station == "MKVI-24-S03",]$lat = gps.replace$DecimalLatitude
dat.out[dat.out$station == "MKVI-24-S03",]$lon = gps.replace$DecimalLongitude


stns = data.frame(table(dat.out$station))


# write.csv(dat.out,"S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Results/Nicole/2022_MKVI_Results.csv",row.names = F)












