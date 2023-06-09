##################################################
########### Quick move files ################
##################################################

## This script is for quickly moving files from one location to another
# usually for training purposes

# Re-set  your script when needed
dev.off()
rm(list=ls())

# Move files to training folder

# define locations for 
prt.dest = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/2023_WLRS_Contract/Training/CC" # parent directory of training material
prt.source = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing" # parent directory of material to move, matching depth of prt.train depth
sub.source = c("S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/Timelapse_files/LDFCS/BIRD/2022/MKSC/MKSC-01/MKSC-01-S04",
               "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/Timelapse_files/LDFCS/BIRD/2022/MKSC/MKSC-02/MKSC-02-S04",
               "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/Timelapse_files/LDFCS/BIRD/2022/MKSC/MKSC-03/MKSC-03-S03",
               "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/Timelapse_files/LDFCS/BIRD/2022/MKSC/MKSC-01/MKSC-01-S08",
               "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/Timelapse_files/LDFCS/BIRD/2022/MKSC/MKSC-02/MKSC-02-S08",
               "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/Timelapse_files/LDFCS/BIRD/2022/MKSC/MKSC-03/MKSC-03-S07") # full names of all sub-directories of material to move


# Loop through the subdirs and copy imgs into new folders while making the appropriate folders

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























