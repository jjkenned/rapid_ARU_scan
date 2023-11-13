###############################################################################################
########### Validate Training Results from the RTS Standard Training Dataset ##################
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


################
### Part 1 #####
################

### Get applicable .db paths
## DB's should be in the same format (template) to compare multiple



##### Option 1 ##########


## Extract db paths from one parent directory DIRECTLY enclosing multiple (identically layed out) subdirectories 

## define locations for 
# parent directory of training material to move
prt.source = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/Training"


# add sub-directories as needed (can be longer than one file)
# These can be of any length, but must be identical in depth (ideally)
# it can work without, but gets a little more complicated 

daughters = c("CC","TS")

prt.sources = lapply(daughters,FUN = function(x) file.path(prt.source,x))
prt.sources = do.call(rbind,prt.sources)


# get DB paths

# i=1
for (i in 1:nrow(prt.sources)){
  
  
  path = data.frame(full = list.files(path = prt.sources[i,],pattern = ".ddb",recursive = T,full.names = T,all.files = T))
  
  path$lengths = lengths(regmatches(path$full, gregexpr("/", path$full)))
  
  
  db.path = path[path$lengths==min(path$lengths),]$full

  
  if(i == 1){db.paths = c(db.path)}else(db.paths = c(db.paths,db.path))
  
  
  
}







##### Option 2 ##########




# just define paths you want to use here
db.paths = c("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Training/Intake_Files/Jamie/Timelapse_files/RTS/CoastOwlsBC.ddb",
             "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Training/Intake_Files/Kelsey/Timelapse_files/RTS/CoastOwlsBC.ddb")

proc.ID = c("JT","KN") 


################
### Part 2 #####
################

# Extract results 

# loop to extract data that you want
for (i in 1:length(db.paths)){
  
  cat("~~~~~\n\n")
  cat("Starting",db.paths[i])
  
  
  RTS.db <- DBI::dbConnect(RSQLite::SQLite(), db.paths[i])
  
  # Read data from specified table
  dat.tbl = dbReadTable(RTS.db,"DataTable")
  dat.tbl = data.frame(dat.tbl)
  # dat.tbl = dat.tbl[dat.tbl$Processed=="true",]
  
  dbDisconnect(RTS.db) # disconnect after extracted
  
  # get individual IDs for this
  dat.tbl$trainee.path = db.paths[i]
  
  dat.tbl$proc.ID = proc.ID[i]
  
  if(i==1){dat.out = dat.tbl}else(dat.out = rbind(dat.out,dat.tbl))
  
  
  
}





## Read in reference dataset

# path to reference database
ref.paths = c("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Training/reference_Files/CoastOwlsBC.ddb")

proc.ID = c("JK")

# loop to extract data that you want
for (i in 1:length(ref.paths)){
  
  cat("~~~~~\n\n")
  cat("Starting",ref.paths[i])
  
  
  RTS.db <- DBI::dbConnect(RSQLite::SQLite(), ref.paths[i])
  
  # Read data from specified table
  dat.tbl = dbReadTable(RTS.db,"DataTable")
  dat.tbl = data.frame(dat.tbl)
  # dat.tbl = dat.tbl[dat.tbl$Processed=="true",]
  
  dbDisconnect(RTS.db) # disconnect after extracted
  
  # get individual IDs for this
  dat.tbl$trainee.path = ref.paths[i]
  
  dat.tbl$proc.ID = proc.ID[i]
  
  if(i==1){dat.ref = dat.tbl}else(dat.ref = rbind(dat.ref,dat.tbl))
  
  
  
}





################
### Part 3 #####
################

# Confirm that processors used the right terms and buttons

table(dat.out$Training)
table(dat.out$Process)
table(dat.out$trainee.path) # got all data you want?
table(dat.out$Notes)

# list comments you want to explore
# comments = c("clipped short, no ratings","incomplete LDFCS; no ratings","na","Not processed","Not reviewed")
comments = unique(dat.out[!dat.out$Notes == "",c("File","Notes")])




################
### Part 4 #####
################


### Compare these datasets

# filter main dataset by the recordings included
ref.use = dat.ref[dat.ref$File %in% dat.out$File,]

# Combine data to compare
dat.comp = rbind(dat.out,ref.use)

# what hasnt been processed
unprocessed = dat.comp[dat.comp$Processed=="false",]

## group by dataset and comparisons 
#test fileds
dataset = dat.comp
obs.field = c("proc.ID")
spec.fields = c("WESO","BADO","NSWO","NOPO","GHOW")
spec.field = spec.fields[1]  
file.field = "File"

compare = function(dataset,obs.field,spec.fields,file.field){
  
  # rename fields for ease of tidyverse code
  colnames(dataset)[colnames(dataset)==obs.field] = "obs.field"
  
  # clear tracking and counting element 
  if(exists("n.itter")){rm(n.itter)}
  
  for (spec.field in spec.fields){
    
    spec.by.field = dataset %>% group_by_at(c("obs.field",file.field)) %>% filter_at(vars(spec.field),all_vars(.>0)) %>% 
      select_(file.field,spec.field,"obs.field")
    
    colnames(spec.by.field)[colnames(spec.by.field)==spec.field] = "spec.field"
    
    
    field.by.obs = spec.by.field %>%
      spread(obs.field, spec.field)
    
    field.by.obs$species = spec.field
    
    
    if(!exists("n.itter")){
    
      comps = field.by.obs
      n.itter = 1
      
    } else {
      
      comps = rbind(comps,field.by.obs)
      n.itter = n.itter + 1
      
    }
    
  }
  
  return(comps)
  
}


comps = compare(dat.comp,obs.field = c("proc.ID"),
                spec.fields = c("WESO","BADO","NSWO","NOPO","GHOW"),
                file.field = "File")


# save the results

write.csv(comps,"S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Training/Results/20231108_Jamie_Kelsey_Jeremiah_Train_Compare.csv",row.names = F)

















