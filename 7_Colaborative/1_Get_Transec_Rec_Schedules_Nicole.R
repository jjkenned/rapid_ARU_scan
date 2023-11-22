############################################################################################
########### Sub-recording list for quick access to transect level metadata  ################
############################################################################################

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
# ~ install.packages(c("RSQLite","exifr","suncalc","tidyverse","lubridate","RSQLite","DBI","chron","stringr"))


# library what is needed
library(tidyverse)
library(fs)


## set directories ##

prnt.source = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Recordings/BIRD/2022/MKVI" # Base folder with recordings present 

############################
##### ~~~~ PART 1 ~~~~ #####
#### Extract metadata ######
############################


# Better alternative to songmeter function in seewave
# This takes much longer, but only really needs to be done once

# List audio files
all.paths = data.frame(Full = list.dirs(prnt.source,recursive = T,full.names = T)) # full names of directories present

all.paths$depth 


paths = fs::dir_info(path = ind.path,recurse = 2) %>% # two depth 
  as_tibble() %>% 
  filter(blocks==0) %>% # set block allocation to 0
  filter(grepl(".wav",path,ignore.case = T)) %>%
  select(c("path")) # keep only path 



for (i in 1:nrow(all.paths)){
  
  
  sub.recs = fs::dir_info(paths$Full[i]) %>% 
    as_tibble() %>% 
    filter(str_detect(path, ".wav")) %>% 
    head(5)
  
  if (i == 1){
    
    sub.out = sub.recs
    
    
  } else {
    
    sub.out = rbind(sub.out,sub.recs)
    
  }
  
}


