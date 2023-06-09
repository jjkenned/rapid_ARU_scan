################################################################
############ Deployment and Retrieval Tracking ################
################################################################


# Re-set  your script when needed
dev.off()
rm(list=ls())

# Required libraries
library(tidyverse)

#### THis script has ___ sections

# Section 1 ~ Read in deployment and retrieval data from jotform csv export
# - Check what is still deployed
# - 


## Directory and filename for csv export 

direct = "S:/Projects/107182-01/06 Data/ARU processing/Deployments"
csv_name = "ARU_Deployment_Retrieval2023-04-20_12_47_48.csv"


## Read in data
dep_ret = read.csv(paste0(direct,"/",csv_name))

# Keep only ARU ID, station ID and deployment/retrieval type
# dep_ret = dep_ret[c("Deployment.or.Retrieval","Full.Station.ID","ARU.ID")]


# Now let's see what we got 
depl = dep_ret[dep_ret$Deployment.or.Retrieval=="Deployment",] %>% group_by(Full.Station.ID,ARU.ID) %>% count(Full.Station.ID,ARU.ID)

#### Check specific transect for Full retrieval 

transects = c("VI-24 (Tsitika Lower)","VI-21 (Schoen Lake)") # set transect

trans = dep_ret[dep_ret$Transect %in% transects,] %>% 
  group_by(Deployment.or.Retrieval,Full.Station.ID,ARU.ID) %>% 
  count(Deployment.or.Retrieval,Full.Station.ID,ARU.ID)

# Save the data for tracking
write.csv(trans,paste0(direct,"/","Tsitika_Schoen_Check.csv"),row.names = F)
