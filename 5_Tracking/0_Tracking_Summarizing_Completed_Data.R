#####################################################################
########### Tracking Data Processing and analysis  ##################
#####################################################################


## This script is for:
# ~ Locate all data and track current state
# ~ Track and save processing 
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
# Check packages being used
# install.packages(c("NCmisc"))
# ignore these ones
ignore = c("NCmisc","grDevices","base")

list.packages = function(ignore){
  
  require(NCmisc)
  
  packages = data.frame(functions = unlist(list.functions.in.file(rstudioapi::getSourceEditorContext()$path, alphabetic = TRUE)))
  packages$packages = rownames(packages)
  
  packs = gsub("package:","",packages[grepl("package:",packages$packages,ignore.case = T),]$packages)
  packs = packs[!packs %in% grep(paste(ignore,collapse = "|"),packs,value = T)]
  
  return(packs)
  
}
packs.to.lib = list.packages(ignore)

# ~ Install if you don't already have them installed
# ~ install.packages(c("RSQLite","exifr","suncalc","tidyverse","lubridate","RSQLite","DBI","chron","stringr"))

# library what is needed




#### 




























