###############################################################################################
########### Validate Training Results from the LDFCS Standard Training Dataset ################
###############################################################################################

## This script is for validating processing results from LDFCS training 
# usually for training purposes

# Re-set  your script when needed
dev.off()
rm(list=ls())


# libraries
library(tidyverse)
library(RSQLite)
library(RODBC)
library(DBI)
library(fs)
library(data.table)


#### Part 1 #####

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
# db.paths = c("db.fullname.1","db.fullname.2")


#### Part 2 #####
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
  
  if(i==1){dat.out = dat.tbl}else(dat.out = rbind(dat.out,dat.tbl))
  
  
  
}


##### Part 3 #### 
# Confirm that processors used the right terms and buttons

table(dat.out$Training)
table(dat.out$Process)
table(dat.out$trainee.path) # got all data you want?
table(dat.out$Comments)

# list comments you want to explore
comments = c("clipped short, no ratings","incomplete LDFCS; no ratings","na","Not processed","Not reviewed")

check = dat.out[dat.out$Comments %in% comments,]
check = check[c("File","Comments")]

# now remove frames that are not present for both
# Im filtering by check because I dont want any of them

remove = check$File

dat.out = dat.out[!dat.out$File %in% remove,]

# make sure the same is present for each
# there should be exactly 2 per image
compare = dat.out %>% group_by(File) %>% summarise(count = n())

# random assign names for this group
# if list gets longer then two you will want to expand into loop or function 
table(dat.out$trainee.path)
dat.out$Obs.ID = NA

dat.out$Obs.ID[dat.out$trainee.path == "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/Training/CC/
               Timelapse_files/LDFCS/BIRD/2022/MKSC/IndicesProcessing4.ddb"] = daughters[1]

dat.out$Obs.ID[dat.out$trainee.path == "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/Training/TS/
               Timelapse_files/LDFCS/BIRD/2022/MKSC/IndicesProcessing4.ddb"] = daughters[2]




# get rid of columns you dont want
dat.out = dat.out[!colnames(dat.out) %in% c("DeleteFlag","RelativePath","DateTime","Process","Training")]


### New DF for only image IDs and to keep difference values
comp.meas = data.frame(File = unique(dat.out$File))


## compare function
# This function groups the values by file name and observer ID and outputs a comparison dataframe for each field to compare 
# using the user-defined comparison metric between the observers
# with two you can do differences
# with more you will need to use more complicated values
# Normalized distribution away from a 'true value'


## compare
file.long = data.table::melt(data.table(file),id.vars = c("File","Obs.ID"),measure.vars = c(comps.cols),value.name = "measure",variable.name = "source")

long.comp = dcast(file.long,File + source ~ Obs.ID, value.var = "measure")

long.comp$diff = NA

for (i in 1:nrow(long.comp)){
  
  long.comp$diff[i] = diff(c(as.numeric(long.comp[i,3]), as.numeric(long.comp[i,4])))
  
  
}

# Now use something else
summary = long.comp %>% group_by(source) %>% summarise(max = max(abs(diff)),
                                                       min = min(abs(diff)),
                                                       mean = mean(abs(diff)),
                                                       median = median(abs(diff)))



plot = ggplot(long.comp, aes(x=diff)) + geom_histogram(binwidth=.5) +
  facet_wrap(~source, ncol = (n_distinct(long.comp$source)/2))

#ggsave(filename = paste0(prt.source,"/","CC_TS_LDFSC_Comparison_plot.png"),plot = plot)

#write.csv(long.comp,file = paste0(prt.source,"/","CC_TS_LDFSC_Results.csv"))

#write.csv(summary,file = paste0(prt.source,"/","CC_TS_LDFSC_Results_SummaryFIle.csv"))




#### 

# read in names of nights to move data from
chosen = read.csv(file.path(prt.dest,"Training_List_Station_Nights_2022.csv"))

## get the associated night data from the spectrograms

# function for extracting observation data from multiple database files
# only extracts 'processed' data and data from specified 'table.name' 

#table.name = "DataTable"

get_specs = function(db.paths,table.name,chosen){
  
  # lists for keeping track of name changes
  tbls.out = list()
  tbls.return = list()
  
  # loop through db files in dbpaths
  for (i in 1:length(db.paths)){
    
    cat("~~~~~\n\n")
    cat("Starting",db.paths[i])
    
    
    RTS.db <- DBI::dbConnect(RSQLite::SQLite(), db.paths[i])
    
    # Read data from specified table
    dat.tbl = dbReadTable(RTS.db,table.name)
    dat.tbl = data.frame(dat.tbl)
    dat.tbl = dat.tbl[dat.tbl$Processed=="true",]
    
    dbDisconnect(RTS.db) # disconnect after extracted
    
    # Filter by chosen
    dat.tbl$station = basename(dirname(dat.tbl$RelativePath))
    dat.tbl$night = basename(dat.tbl$RelativePath)
    
    dat.tbl$stn_night = paste0(dat.tbl$station,"_",dat.tbl$night)
    chosen$stn_night = paste0(chosen$Station,"_",chosen$Night)
    
    # filter names by chosen 
    chs.specs = dat.tbl[dat.tbl$stn_night %in% chosen$stn_night,]
    
    # Find real spec paths from chosen specs
    chs.specs$Full.spec = file.path(dirname(db.paths[i]),chs.specs$RelativePath,chs.specs$File)
    chs.specs$Full.spec = gsub("\\\\","/",chs.specs$Full.spec)
    
    # parent folder
    chs.specs$parent = unlist(strsplit(chs.specs$Full.spec,"/PMRA_SAR/"))[1]
    
    # new columns for filling in loops below
    chs.specs$dest.path = NA
    chs.specs$new_path = NA
    chs.specs$extra = NA
    chs.specs$rec_name = NA
    
    
    
    # j =1 
    
   # loop through destination folders
    for (j in 1:length(out.dirs)){
      
      cat("\n\n")
      cat("Directory",out.dirs[j])
      
      # make new df for each destination folder
      chs.in = chs.specs
      
      #k=1
      # loop through specs going into each folder
      for (k in 1:nrow(chs.specs)){
        
        
        
        chs.in$extra[k] = unlist(strsplit(chs.in$File[k],"_"))[4]
        chs.in$extra[k] = paste0("_",chs.in$extra[k])
        chs.in$rec_name[k] = gsub(chs.in$extra[k],".wav",chs.in$File[k])
        
        chs.in$dest.path[k] = out.dirs[j]
        chs.in$new_path[k] = gsub(chs.in$parent[k],chs.in$dest.path[k],chs.in$Full.spec[k])
        
        if(k%%100==0){
          
          cat("\n")
          
          cat("Complete",k,"of",nrow(chs.in))
          
          }
      }
      
      # keep it all in return list
      tbls.out[[j]] = chs.in
      
      
    }
    
   tbls.return[[i]] = do.call(rbind,tbls.out)
    
  }
  
  df.out = do.call(rbind,tbls.return)
  
  return(df.out)
  
}

# Apply function
dat.tbl = get_specs(db.paths,"DataTable",chosen)

# now let's get recordings that correspond to each spec
move = dat.tbl[c("File","DateTime","station","night","Full.spec","dest.path","new_path","rec_name")]

# let's move the specs real quick
for (i in 1:nrow(move)){
  
  if(!exists(dirname(move$new_path[i]))){
    
    dir.create(dirname(move$new_path[i]))
    
  }
  
  file_copy(move$Full.spec[i],move$new_path[i])
  
  if(i%%500 == 0){print(i)}
  
  
}

#### IF you move spectrograms from other machines and want to transfer audio files only using this machine,
#### then go move the spetrograms now and come back and re-read the file list in the destination folder from here down
# if not, skip this section
# either way worth saving this tracking file
# write.csv(dat.tbl,"S:/Projects/107182-01/07a Working Folder/Protocols/ARU/Rapid SCan/Rapid_Scanning_Training/R_Output/RTS_Spec_Transfer_For_Jamie_Kelsey.csv",row.names = F)

# List all files prestent in destenation folder
move = read.csv("S:/Projects/107182-01/07a Working Folder/Protocols/ARU/Rapid SCan/Rapid_Scanning_Training/R_Output/RTS_Spec_Transfer_For_Jamie_Kelsey.csv")
move = move[c("File","DateTime","station","night","Full.spec","dest.path","new_path","rec_name")]

# list files themselves
files = data.frame(Full.jpg = list.files("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Training/Transferring_Files",recursive = T,full.names = T))
files$jpg = basename(files$Full.jpg)


jpg_wav_rnm = function(jpg.name){
  
  spread = unlist(strsplit(jpg.name,"_"))
  end = tail(spread,1)
  wav = gsub(paste0("_",end),".wav",jpg.name)
  
  return(wav)
  
}

files$rec.base = sapply(files$jpg, jpg_wav_rnm)


# Now recordings
# find the recordings in the source location
recs = data.frame(Full = list.files(prt.source,recursive = T,pattern = ".wav",full.names = T))
recs.move = data.frame(Full = unique(recs[basename(recs$Full) %in% files$rec.base,]))



recs.move$rec.new = NA


# loop through files to make the new name


recs.ret = list()

for (d in 1:length(out.dirs)){
  
  
  recs.out = recs.move
  recs.out$dest.path = out.dirs[d]
  
  for(i in 1:nrow(recs.out)){
    
    recs.out$rec.new[i] = gsub(prt.source,recs.out$dest.path[i],recs.out$Full[i])
    
    
  }
  
  recs.ret[[d]] = recs.out
  
  
  
  
  
  
}

new.recs = do.call(rbind,recs.ret)

# now we can loop through again and move them

for (i in 1:nrow(new.recs)){
  
  if(!exists(dirname(new.recs$rec.new[i]))){dir.create(dirname(new.recs$rec.new[i]),recursive = T)}
  
  file_copy(new.recs$Full[i],new.recs$rec.new[i])
  
  if(i%%100==0){cat("\n\nCOmpleted",i,"of",nrow(new.recs))
    } else if(i == nrow(new.recs)){cat("\n\nCOmpleted",i,"of",nrow(new.recs))}
  
  
  
}

write.csv(new.recs,file = "S:/Projects/107182-01/07a Working Folder/Protocols/ARU/Rapid SCan/Rapid_Scanning_Training/R_Output/RTS_Recording_Training_Transfer_For_Jamie_Kelsey.csv")



# quick check if any overlap 
spec_duplicates = data.frame(table(dat.tbl$File))
nrow(spec_duplicates[spec_duplicates$Freq>1,])

## Next, group by nights and species 
# start by creating station and night ID



# Start moving files ID spec files









# start by creating a quick function to rename and copy files 
copy.name.file = function(sub.source,sub.dest,prt.dest,prt.source){
  
  dir.create(sub.dest,recursive = T)
  file.source = list.files(sub.source,recursive = T, full.names = T, pattern = ".jpg")
  file.dest = gsub(prt.source,prt.dest,file.source)
  file.copy(from = file.source,to = file.dest,recursive = T)
  
}


for (i in 1:length(sub.source)){
  
  sub.dest = gsub(prt.source,prt.dest,sub.source[i]) # create name
  
  if (!exists(sub.dest)){copy.name.file(sub.source,sub.dest,prt.dest,prt.source)}
  
  
  
}























