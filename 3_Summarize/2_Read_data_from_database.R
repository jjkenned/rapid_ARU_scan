########################################################################################
########### Read Data out of SQLite Database for reviews and Tracking ##################
########################################################################################


# Re-set  your script when needed

rm(list=ls())
dev.off()

# packages
library(tidyverse)
library(RSQLite)
library(DBI)
library(chron)

#############################
#### Step 1 - Read Data #####
#############################


# set location for metadata Database
db.path = "S:/Projects/107182-01/06 Data/ARU processing/meta_data"

# Connect to Database
meta.db = dbConnect(RSQLite::SQLite(),file.path(db.path,"20231022_Meta_Database.sqlite"))

# check status ~ if new, will get no info
dbListTables(conn = meta.db)

# read table you want
meta = RSQLite::dbReadTable(meta.db,"metadata")

# disconnect
dbDisconnect(meta.db)

# keep specific cols

keep = c("serial_number","form_tstamp","real_prefix","real_transect")

meta.rev = meta[keep]

# Get year ID
meta.rev$year = year(meta.rev$form_tstamp)

# summarize

meta.sumry = meta.rev %>% group_by(serial_number,real_prefix,real_transect,year) %>% summarise(count = n())

write.csv(meta.sumry,file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/MetaData/Recording/2023_MKSC_Process_Tracking.csv",row.names = F)





dir.create("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/schedule_visualizations/BIRD/2023/MKSC/MKSC-05",recursive = T)







