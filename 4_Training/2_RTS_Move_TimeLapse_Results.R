################################################################################################
########### Move all Timelapse Files in folder structure for review and share ##################
################################################################################################


# Re-set  your script when needed

rm(list=ls())
dev.off()

# Packages
library(tools)


#################################################
#### Step 1 - Set timelapase file locations #####
#################################################

# Source and destination locations (make sure no unwanted files are in here, other than wav or image files)
from = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Training/Transferring_Files/Jamie/training/PMRA_SAR/processing"
to = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Training/Intake_Files/Jamie/training/PMRA_SAR/processing"

# file types NOT to include
exclude = c("wav","jpg","jpeg","png")

# get all appropriate files
# list all files
files = data.frame(Full = list.files(path = from,recursive = T,full.names = T))

# keep only files not matching extentions above
files.move = files$Full[!file_ext(files$Full) %in% exclude]



# copy files to new location
for (i in 1:length(files.move)){
  
  # new file name
  new.name = gsub(from,to,files.move[i])
  
  # make new dir
  if(!exists(dirname(new.name))){dir.create(dirname(new.name),recursive = T)}
  
  # copy files
  file.copy(files.move[i],to = new.name ,recursive = T,copy.mode = T)
  
  
  
  
}






