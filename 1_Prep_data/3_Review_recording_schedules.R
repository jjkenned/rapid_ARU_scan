###########################################################
########### Visualize recording schedules  ################
###########################################################

## This script is for:
## - Determining/describing recording schedule
## - Exploring recording schedules within a set of recordings to find inconsistencies  
##  - in time of year (date)
##  - between Stations within a transect (or other deployment type)
##  - between actual and expected recording schedule 
##  

# Re-set  your script when needed
dev.off()
rm(list=ls())

# Base folder with recordings present 
recs.loc = c("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/recordings/2022_Nawhitti")

#### First trial of this component of script ######
# List files in location
files = data.frame(Full_name = list.files(path = recs.loc,all.files = T,full.names = T,recursive = T, pattern = ".wav"))

# Get Meta data 
meta = songmeter(basename(files$Full_name)) 
meta$recording = basename(files$Full_name) # append recording name
meta$full_name = files$Full_name # append full file path


# Get ordinal date
orday = yday(as.Date(paste0(file.grps$year[i],"-",file.grps$month[i],"-",file.grps$day[i])))



# Summarize by nights, volume of data and locations
file.grps = meta %>% group_by(prefix,year,month,day) %>% mutate(group_id = cur_group_id()) # group by date
# quick function for translating day number into night number 
# day = night ID, hr = hour in 24 hr clock (as integer), split = when to cut off the nights from one another (usually 12...noon)
d2n.func = function(day,hr,split){
  
  if (hr<split){night = day} else (night = day+1)
  
  return(night)
  
}






# set intermediate directories 
all.dirs = list.dirs(prnt.source,recursive = T,full.names = T)

