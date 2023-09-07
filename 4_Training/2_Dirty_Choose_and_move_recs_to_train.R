##################################################
########### Bulk and Dirty Move Recs################
##################################################

## This script is for quickly moving files from one location to another
# usually for training purposes

# Re-set  your script when needed
dev.off()
rm(list=ls())


# libs
library(tidyverse)
library(fs)


# Move files to training folder

# define locations for 
prt.dest = "S:/Projects/107182-01/07a Working Folder/Protocols/ARU/Rapid SCan/Rapid_Scanning_Training/R_Output" # parent directory of training material

prt.source = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Recordings/BIRD/2023" # parent directory of material to move, matching depth of prt.train depth

prt.replace.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke"

out.dirs = c("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Training/Transferring_Files/Jamie","S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Training/Transferring_Files/Kelsey")



## Files to work with 
recs = data.frame(Full = list.files(prt.source,recursive = T,pattern = ".wav",full.names = T))

choose = readxl::read_excel("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Training/ShareTrack.xlsx")


recs.ret = list()
recs$dest.path = NA

for (d in 1:length(recs)){
  
  
  recs.out = recs
  recs.out$dest.path = out.dirs[d]
  
  for(i in 1:nrow(recs.out)){
    
    recs.out$rec.new[i] = gsub(prt.replace.dir,recs.out$dest.path[i],recs.out$Full[i])
    
    
  }
  
  recs.ret[[d]] = recs.out
  
  
  
  
  
  
}

###  

new.recs = do.call(rbind,recs.ret)

### let's choose stations and transects

new.recs$station = basename(dirname(new.recs$Full))
new.recs$rec.name = basename(new.recs$Full)



new.recs$rec.prefix = sapply(new.recs$rec.name,FUN = function(x) unlist(strsplit(x,"_"))[1])

new.recs$status = "choose"

new.recs[!new.recs$station == new.recs$rec.prefix,]$status = "remove"

# review

keepers = new.recs %>% group_by(rec.prefix,station,status) %>% tally()

convert = new.recs[new.recs$station %in% choose$Station,]

# now filter by volunteer surveyor
convert$processor = basename(convert$dest.path)
#i=1
for (i in 1:nrow(choose)){
  
  proc.chosen = convert[convert$rec.prefix == choose$Station[i],]
  proc.chosen = proc.chosen[proc.chosen$processor == choose$Volunteer[i],]
 # j=1
  for(j in 1:nrow(proc.chosen)){
    
    
    
    if(!exists(dirname(proc.chosen$rec.new[j]))){dir.create(dirname(proc.chosen$rec.new[j]),recursive = T)}
    fs::file_copy(proc.chosen$Full[j],proc.chosen$rec.new[j])
    
    
    
  }
  
  
  cat("\n\n~~~~~~\n","completed",choose$Station[i],"for",choose$Volunteer[i])
  
  
  
}
















get_rec_stn = function(full.name){
  
  spread = unlist(strsplit(full.name,"/"))
   = tail(spread,1)
  wav = gsub(paste0("_",end),".wav",jpg.name)
  
  return(wav)
  
}

files$rec.base = sapply(files$jpg, jpg_wav_rnm)




# now we can loop through again and move them


for (i in 1:nrow(new.recs)){
  
  if(!exists(dirname(new.recs$rec.new[i]))){dir.create(dirname(new.recs$rec.new[i]),recursive = T)}
  
  file_copy(new.recs$Full[i],new.recs$rec.new[i])
  
  if(i%%100==0){cat("\n\nCOmpleted",i,"of",nrow(new.recs))
  } else if(i == nrow(new.recs)){cat("\n\nCOmpleted",i,"of",nrow(new.recs))}
  
  
  
}


