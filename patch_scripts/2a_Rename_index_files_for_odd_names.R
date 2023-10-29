##############################################################################################################
########### Re-name Indices output files for LDFCS formation based on some incorrect names  ##################
##############################################################################################################

## Script Sections

# 1) Build new names based on recording names in station file
# this means you need to make sure that the ind.path is the transect ID 
# 2) 
#





# Re-set  your script when needed
dev.off()

rm(list=ls())

# libraries
library(stringr)
library(tidyverse)
library(fs)
library(av)
library(chron)
library(seewave)
library(lubridate)
library(rjson)
library(jsonlite)
library(warbleR)
#library(SciencesPo)

# set file path to location of indices files
ind.root = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/BIRD/2023/MKSC/by_rec/MKSC-01"
rec.root = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Recordings/BIRD/2023/MKSC/MKSC-01"
results.root = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/Name_Change_Tracking/BIRD/2023/MKSC/MKSC-01/new_name_indices"


# Get file paths and info 
# File paths for indices folders
paths = fs::dir_info(path = ind.root,recurse = 2) %>% # two depth 
  as_tibble() %>% 
  filter(blocks==0) %>% # set block allocation to 0
  select(c("path")) # keep only path 
colnames(paths) = "ind.path"

# get recording id for paths
paths = paths[substr(paths$ind.path,nchar(paths$ind.path)-3,nchar(paths$ind.path))==".wav",]

paths$base.name = basename(paths$ind.path)
paths.use = paths[substr(paths$base.name,1,11)=="MKSC-01-S10",]



# 
# Read in metadata
meta = read.csv(file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/MetaData/Recording/2023_MKSC__All_meta_and_names_missing_short_recs.csv",stringsAsFactors = F)

meta.use = meta[meta$station=="MKSC-01-S10",]


# combine
dat_out = merge(meta.use,paths.use,by = "base.name")


## function for changing those file names
change.index.output.names = function(indices.enclosing.folder,new.basename,old.basename){
  
  # get basename file name to work with
  base.rec.ID.old = gsub(pattern = ".wav",replacement = "",old.basename)
  base.rec.ID.new = gsub(pattern = ".wav",replacement = "",new.basename)
  
  
  ## list files in index output folder
  sub.files = fs::dir_info(path = indices.enclosing.folder,recurse = T) %>%
    as_tibble() %>%
    filter(type=="file") %>% # keep files and not folders
    select(c("path"))
  
  
  # replace symbols in old name
  if(grepl("_0\\+1_",base.rec.ID.old)){
    
    base.rec.ID.old = gsub(pattern = "_0\\+1_",replacement = "_0\\\\+1_",base.rec.ID.old) 
    
  }
  ## Copy files into new location with new name
  # set new location name
  sub.files$new.files = gsub("by_rec","new_name_indices",sub.files$path)
  
  
  
  sub.files$new.files = gsub(base.rec.ID.old,base.rec.ID.new,sub.files$new.files)
  
  


  
  # copy files over and rename
  if(!exists(dirname(sub.files$new.files[1]))){
    
    dir.create(dirname(sub.files$new.files[1]),recursive = T) # create new dir
    
  }
  
  file.copy(sub.files$path,sub.files$new.files,recursive = T) # copy with new names
  
  # read the one json file we need
  json.path = sub.files$new.files[grepl("IndexGenerationData.json",sub.files$new.files,fixed = T)]
  json.old = jsonlite::fromJSON(json.path,simplifyVector = F)
  
  json.old$RecordingBasename = base.rec.ID.new
  
  json.new = jsonlite::toJSON(json.old,pretty = T,auto_unbox = T, na='null',null = 'null')
  
  write(json.new,json.path)
  
  print(old.basename)
  
}

# apply function 
mapply(change.index.output.names,
       indices.enclosing.folder = dat_out$ind.path,
       new.basename = dat_out$name.ldfcs,
       old.basename = dat_out$base.name)





























