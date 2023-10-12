##################################################
########### Check Recording Files ################
##################################################

## Script Sections

# Rename files if needed




# Re-set  your script when needed
dev.off()
rm(list=ls())

# libraries
library(stringr)
library(tidyverse)

# set file path to location of recording files

par.dir = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Recordings/BIRD/2023/MKSC/MKSC-03/MKSC-03-S04"


##### 1) Get Folder Structure & Depth ###### 

## list files ##
files = data.frame(full = list.files(par.dir,full.names = T,recursive = T,pattern = ".wav"))

# new name
files$new.name = gsub("-003-A04_","-03-S04_",files$full)
files$new.name = gsub("*_0\\+1_*","_",files$new.name)



for(i in 1:nrow(files)){
  
  file.rename(from = files$full[i], files$new.name[i])
  
  
}




# Now let's deal with name issues
?file.rename




