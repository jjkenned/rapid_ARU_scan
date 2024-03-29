##################################################
########### Quick move files ################
##################################################

## This script is for quickly moving files from one location to another
# usually for training purposes

# Re-set  your script when needed
dev.off()
rm(list=ls())


# libs
library(tidyverse)
library(RSQLite)
library(RODBC)
library(DBI)
library(fs)


# Move files to training folder

# define locations for 
prt.dest = "S:/Projects/107182-01/07a Working Folder/Protocols/ARU/Rapid SCan/Rapid_Scanning_Training/R_Output" # parent directory of training material

prt.source = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Recordings/BIRD/2023" # parent directory of material to move, matching depth of prt.train depth

out.dirs = c("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Training/Transferring_Files/Jamie","S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Training/Transferring_Files/Kelsey")

db.paths = c("D:/PMRA_SAR/processing/Timelapse_files/RTS/BIRD/2022/TimelapseData_merged.ddb")

# db.path = "D:/PMRA_SAR/processing/Timelapse_files/RTS/BIRD/2022/TimelapseData_merged.ddb"
# db.path.2 ="S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/Timelapse_files/RTS/BIRD/2022/CoastOwlsBC.ddb"



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
    
    dir.create(dirname(move$new_path[i]),recursive = T)
    
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























