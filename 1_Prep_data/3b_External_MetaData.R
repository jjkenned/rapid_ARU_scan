#########################################################################
########### External MetaData Gather and Combine in DB ##################
#########################################################################


## This script is for:
# ~ Gather the following 'external' meta data for each recording
#   - lat and lon from deployment/retrieval
#   - 
# ~ Combine internal metadata from MetaData_Names_Removal script and the following MetaData
# ~ Track and save processing 
# ~ 



## SQLite in R
# download sqlite for appropriate OS (https://sqlite.org/download.html) 
# For windows, find "A bundle of command-line tools for managing SQLite database files"
# 

#### Script Pre-set #####


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
# Check packages being used
# install.packages(c("NCmisc"))
# ignore these ones
ignore = c("NCmisc","grDevices","base")
source("Sourced_Functions/list_Packages.R")
packs.to.lib = list.packages(ignore)

# ~ Install if you don't already have them installed
# ~ install.packages(c("RSQLite","exifr","suncalc","tidyverse","lubridate","RSQLite","DBI","chron","stringr"))

# library what is needed
library(RSQLite)
library(DBI)



#### Define locations of data
# databases
# set location for metadata Database
db.main.path = "S:/Projects/107182-01/06 Data/ARU processing/meta_data"
db.main.name = "20231022_Meta_Database.sqlite"

# set location for database with external ARU data
base.meta.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/MetaData"
GPS.Locs = file.path(base.meta.dir,"External","20231011_Location_Info.csv") # aru locations

#### Read in Data ####
meta.db = dbConnect(RSQLite::SQLite(),file.path(db.path,db.main.name))



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
































