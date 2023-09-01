################################################################
############ SD Card Transfer Validation Script ################
################################################################


# Re-set  your script when needed
dev.off()
rm(list=ls())



#### First section of script is for creating directories for recording files to go to #####
destination.parent = "E:/PMRA_SAR/Recordings"
year = "2023"

###### Build comparison pairs for now and for transfer validation ###### 
SD.pair=data.frame(SD=c("F:/","G:/","D:/"))
SD.pair$station = c("MKVI-08-S01","MKVI-08-S01","MKVI-08-S03")


# create directory function
create.dir = function(destination.parent,year,SD.pair){
  SD.pair$dest = NA
  # run loop to name
  
  for (i in 1:nrow(SD.pair)){
    # Pull transect and region IDs from SD.pair$station[i]
    region = substr(SD.pair$station[i],1,4)
    transect = substr(SD.pair$station[i],1,7)
    if(substr(SD.pair$station[i],9,9)=="S"){
      type = "BIRD"
    } else if (substr(SD.pair$station[i],9,9)=="U"){
      type = "BAT"
      }
    dir.out = paste0(destination.parent,"/",
                     type,"/",
                     year,"/",
                     region,"/",
                     transect,"/",
                     SD.pair$station[i]
                     )
    
    if(!exists(dir.out)){dir.create(dir.out,recursive = T)}
    
    # now record this directory
    SD.pair$dest[i] = dir.out
   
    
  }
  
  Pair.data = SD.pair
  return(Pair.data)
  
  
}


Pair.data = create.dir(destination.parent,year,SD.pair)


##### Second Section of script is for running check against SD cards that  ######
## have already been transferred to their final location 

# Check transfer

check.transfer = function(Pair.data){
  
  for (i in 1:nrow(Pair.data)){
    
    # set station
    station = Pair.data$station[i]
    
    # list these recordings 
    Destination.Full=list.files(Pair.data$dest[i], recursive=T, pattern='*.wav')
    destination.list=basename(Destination.Full) # names of all recordings 
    
    
    # List the recordings (wav files) on SD cards
    SD.files = list.files(Pair.data$SD[i], recursive=T, pattern='*.wav')
    SD.files = basename(SD.files) # only base file names for comparison
    
    # filter for the right station
    # SD.files<-SD.files[substr(SD.files,1,11) == station]
    
    # check if locations are present
    NOT_COPIED=SD.files[(!SD.files %in% destination.list)]
    EXTRAS=destination.list[(!destination.list %in% SD.files)]
    
    # Now return in consol
    print(Pair.data$station[i])
    print("Not Copied")
    print(NOT_COPIED)
    
    print("Extras")
    print(EXTRAS)
    
    cat("\n")
    
    
  }
  
  
  
  
}


check.transfer(Pair.data)



## Rename files post transfer

# find directory 
dir_change = "D:/PMRA_SAR/Recordings/BIRD/2023/MKVI/MKVI-24/MKVI-24-S04"
dir_to = "D:/PMRA_SAR/Recordings/BIRD/2023/MKVI/MKVI-21/MKVI-21-S12"

# list files
file_names = data.frame(Orig=list.files(dir_change,full.names = T,recursive = T))


# make new name
find = "MKVI-21-S04"
replace = "MKVI-24-S04"

# spread out to transect if needed
file_names$new = gsub(find,replace,file_names$Orig) # for stations
dir.create(path = dir_to)


# apply new name
for (i in 1:nrow(file_names)){
  
  file.rename(file_names$Orig[i],file_names$new[i])
  
  
}




















