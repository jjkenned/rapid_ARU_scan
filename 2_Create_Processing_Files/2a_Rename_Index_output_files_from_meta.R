###########################################################
########### Check completed indices and what's missing  ################
###########################################################


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
library(lubridate)
library(RSQLite)
library(DBI)



# set file path to location of indices files and database
# set transect ID
transect = "MKVI-04"
region = substr(transect,1,4)
year = "2023"

ind.root = "F:/PMRA_SAR/Processing/BIRD"
results.root = "F:/PMRA_SAR/Processing/Name_Change_Tracking/BIRD"

ind.path = file.path(ind.root,year,region,"by_rec",transect)
# results.path = file.path(results.root,year,region,"new_name_indices",transect)

meta.file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/MetaData/Recording/2023_All_meta_and_names_for_analysis.csv"
ind.dir = "F:/PMRA_SAR/Processing/BIRD/2023/MKVI"


## Read in meta data with name conversions in it

name.dat = read.csv(meta.file,stringsAsFactors = F)

dat.use = name.dat[c("base.name","name.ldfcs")]
dat.use = dat.use[substr(dat.use$base.name,1,7)==transect,]



# Get file paths and info 
# File paths for indices folders
paths = fs::dir_info(path = ind.path,recurse = 2) %>% # two depth 
  as_tibble() %>% 
  filter(blocks==0) %>% # set block allocation to 0
  filter(grepl(".wav",path,ignore.case = T)) %>%
  select(c("path")) # keep only path 

colnames(paths) = "ind.path"

# get basename for merge
paths$base.name = basename(paths$ind.path)


## combine

dat.in = merge(dat.use,paths, by = "base.name")



# Function

change.index.output.names = function(indices.enclosing.folder,new.basename,old.basename){
  
  # get basename file name to work with
  base.rec.ID.old = gsub(pattern = ".wav",replacement = "",old.basename)
  base.rec.ID.new = gsub(pattern = ".wav",replacement = "",new.basename)
  
  
  ## list files in index output folder
  sub.files = fs::dir_info(path = indices.enclosing.folder,recurse = T) %>%
    as_tibble() %>%
    filter(type=="file") %>% # keep files and not folders
    select(c("path"))
  
  ## Copy files into new location with new name
  # set new location name
  sub.files$new.files = gsub("by_rec","new_name_indices",sub.files$path)
  sub.files$new.files = gsub(base.rec.ID.old,base.rec.ID.new,sub.files$new.files)
  
  # copy files over and rename
  dir.create(dirname(sub.files$new.files[1]),recursive = T) # create new dir
  file.copy(sub.files$path,sub.files$new.files,recursive = T) # copy with new names
  
  # read the one json file we need
  json.path = sub.files$new.files[grepl("IndexGenerationData.json",sub.files$new.files,fixed = T)]
  json.old = jsonlite::fromJSON(json.path,simplifyVector = F)
  
  json.old$RecordingBasename = base.rec.ID.new
  
  json.new = jsonlite::toJSON(json.old,pretty = T,auto_unbox = T, na='null',null = 'null')
  
  write(json.new,json.path)
  
  
  
  
}

# apply function 
mapply(change.index.output.names,
       indices.enclosing.folder = dat.in$ind.path,
       new.basename = dat.in$name.ldfcs,
       old.basename = dat.in$base.name)





























